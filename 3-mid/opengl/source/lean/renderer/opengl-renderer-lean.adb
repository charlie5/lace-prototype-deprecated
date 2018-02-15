with
     openGL.Viewport,
     openGL.Program,
     openGL.Camera,
     openGL.Light.directional,
     openGL.Model.billboard.textured,
     openGL.Font.texture,
     openGL.Server,
     openGL.Tasks,
     openGL.IO,
     openGL.Errors,

     GL.lean,

     float_math.Algebra.linear.d3,

     interfaces.C,
     gnat.heap_Sort,
     System,

     ada.Text_IO,
     ada.Exceptions,
     ada.Task_Identification,
     ada.unchecked_Deallocation;


package body openGL.Renderer.lean
is
   use GL, openGL.Program, interfaces.C, ada.Text_IO;


   ---------
   --- Forge
   --

   procedure define (Self : access Item)
   is
   begin
      Self.safe_camera_updates_Map.define;
   end define;



   procedure destroy (Self : in out Item)
   is
      use openGL.Texture;
   begin
      Self.stop_Engine;

      while not Self.Engine'Terminated
      loop
         delay Duration'Small;
      end loop;

      Self.safe_camera_updates_Map.destruct;

      declare
         procedure free is new ada.Unchecked_Deallocation (visual_geometry_Couples,
                                                           visual_geometry_Couples_view);
      begin
         free (Self.all_opaque_Couples);
         free (Self.all_lucid_Couples);
      end;

      vacuum  (Self.texture_Pool);
      destroy (Self.texture_Pool);

      while not Self.Fonts.Is_Empty
      loop
         declare
            use Font, Font.font_id_Maps_of_font;
            Cursor   : font_id_Maps_of_font.Cursor := Self.Fonts.First;
            the_Font : Font.view                   := Element (Cursor);
         begin
            free (the_Font);
            Self.Fonts.delete (Cursor);
         end;
      end loop;

   end destroy;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;




   --------------
   --- Attributes
   --

   procedure Context_is (Self : in out Item;   Now : in Context.view)
   is
   begin
      Self.Context := Now;
   end Context_is;


   procedure Context_Setter_is (Self : in out Item;   Now : in context_Setter)
   is
   begin
      Self.context_Setter := Now;
   end Context_Setter_is;


   procedure Swapper_is (Self : in out Item;   Now : in Swapper)
   is
   begin
      Self.Swapper := Now;
   end Swapper_is;



   procedure queue_Impostor_updates (Self : in out Item;    the_Updates   : in     impostor_Updates;
                                                            the_Camera    : access openGL.Camera.item'Class)
   is
      the_camera_Updates : updates_for_Camera_view;
   begin
      Self.safe_camera_updates_Map.add (the_Updates, Camera_view (the_Camera));
   end queue_Impostor_updates;



   procedure queue_Visuals (Self : in out Item;    the_Visuals   : in     Visual.views;
                                                   the_Camera    : access openGL.Camera.item'Class)
   is
      the_Updates : updates_for_Camera_view;
   begin
      Self.safe_camera_updates_Map.add (the_Visuals, Camera_view (the_Camera));
   end queue_Visuals;



   procedure update_Impostors_and_draw_Visuals (Self : in out Item;   all_Updates : in camera_updates_Couples)
   is
      use camera_Maps_of_updates;

   begin
      for i in all_Updates'Range
      loop
         declare
            the_Camera  : constant Camera_view             := all_Updates (i).Camera;
            the_Updates : constant updates_for_Camera_view := all_Updates (i).Updates;
         begin
            openGL.Viewport.Extent_is ((the_Camera.Viewport.Max (1),
                                        the_Camera.Viewport.Max (2)));

            Self.update_Impostors (the_Updates.Impostor_updates (1 .. the_Updates.impostor_updates_Last),
                                   camera_world_Transform => the_Camera.world_Transform,
                                   view_Transform         => the_Camera.view_Transform,
                                   perspective_Transform  => the_Camera.projection_Transform);

            the_Updates.impostor_updates_Last := 0;
         end;
      end loop;


      Self.swap_Required := False;

      for i in all_Updates'Range
      loop
         declare
            the_Camera  : constant Camera_view             := all_Updates (i).Camera;
            the_Updates : constant updates_for_Camera_view := all_Updates (i).Updates;
            clear_Frame : constant Boolean                 := i = all_Updates'First;
         begin
            openGL.Viewport.Extent_is ((the_Camera.Viewport.Max (1),
                                        the_Camera.Viewport.Max (2)));


            Self.draw (the_Visuals            => the_Updates.Visuals (1 .. the_Updates.visuals_Last),
                       camera_world_Transform => the_Camera.world_Transform,
                       view_Transform         => the_Camera.view_Transform,
                       perspective_Transform  => the_Camera.projection_Transform,
                       clear_Frame            => clear_Frame,
                       to_Surface             => null);

            the_Updates.visuals_Last := 0;
            Self.swap_Required       := True;
         end;
      end loop;
   end update_Impostors_and_draw_Visuals;



   procedure free_old_Models (Self : in out Item)
   is
      use openGL.Model;

      free_Models : graphics_Models;
      Last        : Natural;
   begin
      Self.obsolete_Models.fetch (free_Models, Last);

      for i in 1 .. Last
      loop
         free (free_Models (i));
      end loop;
   end free_old_Models;



   procedure free_old_Impostors (Self : in out Item)
   is
      use openGL.Impostor;

      free_Impostors : Impostor_Set;
      Last           : Natural;
   begin
      Self.obsolete_Impostors.fetch (free_Impostors, Last);

      for i in 1 .. Last
      loop
         free (free_Impostors (i));
      end loop;
   end free_old_Impostors;




   ---------
   -- Engine
   --

   task body Engine
   is
      the_Context : openGL.Context.view;
      Done        : Boolean            := False;

   begin
      select
         accept start (Context : openGL.Context.view)
         do
            the_Context := Context;
         end start;

         openGL.Tasks.current_Task := ada.Task_Identification.Current_Task;
         Self.context_Setter.all;

         put_Line ("openGL Server version: " & openGL.Server.Version);

      or
         accept Stop
         do
            Done := True;
         end Stop;
      end select;


      while not Done
      loop
         declare
            all_Updates        : camera_updates_Couples (1 .. 100);
            Length             : Natural;

            new_font_Name      : asset_Name := null_Asset;
            new_font_Size      : Integer;

            new_snapshot_Name  : asset_Name := null_Asset;
            snapshot_has_Alpha : Boolean;

         begin
            select
               accept render
               do
                  Self.is_Busy := True;
                  Self.safe_camera_updates_Map.fetch_all_Updates (all_Updates, Length);
               end render;

            or accept add_Font (font_Id : in openGL.Font.font_Id)
               do
                  new_font_Name := font_Id.Name;
                  new_font_Size := font_Id.Size;
               end add_Font;

            or accept Screenshot (Filename   : in String;
                                  with_Alpha : in Boolean := False)
               do
                  new_snapshot_Name  := to_Asset (Filename);
                  snapshot_has_Alpha := with_Alpha;
               end Screenshot;

            or accept Stop
               do
                  Done := True;
               end Stop;
            end select;

            exit when Done;


            if new_font_Name /= null_Asset
            then
               Self.Fonts.insert ((new_font_Name,
                                   new_font_Size),
                                  Font.texture.new_Font_texture (to_String (new_font_Name)).all'Access);

            elsif new_snapshot_Name /= null_Asset
            then
               openGL.IO.Screenshot (Filename   => to_String (new_snapshot_Name),
                                     with_Alpha => snapshot_has_Alpha);

            else
               Self.update_Impostors_and_draw_Visuals (all_Updates (1 .. Length));

               Self.free_old_Models;
               Self.free_old_Impostors;

               Self.is_Busy := False;

               if    Self.Swapper /= null
                 and Self.swap_Required
               then
                  Self.Swapper.all;
               end if;
            end if;
         end;
      end loop;

      Self.free_old_Models;
      Self.free_old_Impostors;


   exception
      when E : others =>
         new_Line;
         put_Line ("Unhandled exception in openGL Renderer engine !");
         put_Line (Ada.Exceptions.Exception_Information (E));
   end Engine;




   --------------
   --- Operations
   --

   procedure start_Engine (Self : in out Item)
   is
   begin
      Self.Engine.start (null);
   end start_Engine;



   procedure stop_Engine (Self : in out Item)
   is
   begin
      Self.Engine.stop;
   end stop_Engine;



   procedure render (Self : in out Item;   to_Surface : in openGL.Surface.view := null)
   is
      pragma Unreferenced (to_Surface);
   begin
      Self.Engine.render;
   end render;


   procedure add_Font (Self : in out Item;   font_Id : in openGL.Font.font_Id)
   is
   begin
      Self.Engine.add_Font (font_Id);
   end add_Font;



   procedure Screenshot   (Self : in out Item;   Filename   : in String;
                                                 with_Alpha : in Boolean := False)
   is
   begin
      Self.Engine.Screenshot (Filename, with_Alpha);
   end Screenshot;



   function is_Busy (Self : in Item) return Boolean
   is
   begin
      return Self.is_Busy;
   end is_Busy;



   procedure update_Impostors (Self : in out Item;   the_Updates            : in impostor_Updates;
                                                     camera_world_Transform : in math.Matrix_4x4;
                                                     view_Transform         : in math.Matrix_4x4;
                                                     perspective_Transform  : in math.Matrix_4x4)
   is
      use math.Algebra.linear.d3, math.Vectors;

      check_is_OK              : constant Boolean                      := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      inverse_view_Transform   : constant openGL.Matrix_3x3            := inverse_Rotation (get_Rotation (view_Transform));

      light_Site               : constant openGL.Vector_3              := (10_000.0, -10_000.0, 10_000.0); --, 0.0);
      the_Light                :          openGL.Light.directional.item;

   begin
      the_Light.inverse_view_Transform_is (inverse_view_Transform);
      the_Light.Site_is  (light_Site);
      the_Light.Color_is (ambient  => (0.7, 0.7, 0.7, 1.0),
                          diffuse  => (1.0, 1.0, 1.0, 1.0),
                          specular => (1.0, 1.0, 1.0, 1.0));


      for i in the_Updates'Range
      loop
         declare
            use openGL.Texture, openGL.Renderer.lean, openGL.Visual,
                GL.lean;

            the_Update               :          impostor_Update renames the_Updates (i);
            the_Impostor             :          impostor.view   renames the_Update.Impostor;

            texture_Width            : constant gl.glSizei           := power_of_2_Ceiling (Natural (the_Update.current_Width_pixels ));
            texture_Height           : constant gl.glSizei           := power_of_2_Ceiling (Natural (the_Update.current_Height_pixels));

            the_Model                : constant openGL.Model.billboard.textured.view
                                                                     := openGL.Model.billboard.textured.view (the_Impostor.Visual.Model);
            GL_Error : Boolean;

         begin
            the_Impostor.Visual.Scale_is         (the_Impostor.Target.Scale);
            the_Impostor.Visual.is_Terrain       (the_Impostor.Target.is_Terrain);
            the_Impostor.Visual.face_Count_is    (the_Impostor.Target.face_Count);

            the_Impostor.Visual.apparent_Size_is (the_Impostor.Target.apparent_Size);

            -- Render the target after clearing openGL buffers.
            --

            gl.glClearColor (0.0, 0.0, 0.0,  0.0);
            glClear         (   GL_COLOR_BUFFER_BIT
                             or GL_DEPTH_BUFFER_BIT);

            declare
               new_view_Transform : Matrix_4x4 := camera_world_Transform;
            begin
               set_Rotation (new_view_Transform, the_Update.current_Camera_look_at_Rotation);
               new_view_Transform := inverse_Transform (new_view_Transform);

               -- Render the target for subsequent copy to impostor texture.
               --
               Self.draw (the_Visuals            => (1 => the_Impostor.Target),
                          camera_world_Transform => camera_world_Transform,
                          view_Transform         => new_view_Transform,
                          perspective_Transform  => perspective_Transform,
                          clear_Frame            => False,
                          to_Surface             => null);
            end;

            -- Get a new sized texture, if needed.
            --
--              if        the_Update.Width_size  /= openGL.texture.Size_width  (the_Model.Texture)
--                or else the_Update.Height_size /= openGL.texture.Size_height (the_Model.Texture)
            if        Natural (the_Update.current_Width_pixels)  /= the_Model.Texture.Size.Width
              or else Natural (the_Update.current_Height_pixels) /= the_Model.Texture.Size.Height
            then
               free (Self.texture_Pool,  the_Model.Texture);
               the_Model.Texture := new_Texture (Self.texture_Pool'Access,
                                                 (Natural (texture_Width),  Natural (texture_Height)));
            end if;

            -- Set texture coordinates.
            --
            declare
               X_first    : constant Real := the_Impostor.expand_X;
               Y_first    : constant Real := the_Impostor.expand_Y;
               X_last     : constant Real := Real (the_Update.current_Width_pixels)  / Real (texture_Width)  - X_First;
               Y_last     : constant Real := Real (the_Update.current_Height_pixels) / Real (texture_Height) - Y_First;
            begin
               the_Model.Texture_Coords_are ((1 => (s => X_first,     t => Y_first),
                                              2 => (s => X_last,      t => Y_first),
                                              3 => (s => X_last,      t => Y_last),
                                              4 => (s => X_first,     t => Y_last)));
            end;

            enable (the_Model.Texture);

            gl.lean.glCopyTexSubImage2D (gl.GL_TEXTURE_2D,                 0,
                                         the_Update.current_copy_x_Offset, the_Update.current_copy_y_Offset,
                                         the_Update.current_copy_X,        the_Update.current_copy_Y,
                                         the_Update.current_copy_Width,    the_Update.current_copy_Height);

            openGL.Errors.log (error_occurred => gl_Error);
         end;
      end loop;

      openGL.Errors.log;
   end update_Impostors;



   procedure draw (Self : in out Item;   the_Visuals            : in Visual.views;
                                         camera_world_Transform : in math.Matrix_4x4;
                                         view_Transform         : in math.Matrix_4x4;
                                         perspective_Transform  : in math.Matrix_4x4;
                                         clear_Frame            : in Boolean;
                                         to_Surface             : in openGL.Surface.view := null)
   is
      pragma Unreferenced (to_Surface, camera_world_Transform);

      use math.Algebra.linear.d3, math.Vectors;

      check_is_OK              : constant Boolean           := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);

      opaque_Count             :          math.Index        := 0;
      lucid_Count              :          math.Index        := 0;

      inverse_view_Transform   : constant openGL.Matrix_3x3 := inverse_Rotation (get_Rotation (view_Transform));
      view_and_perspective_Transform
                               : constant openGL.Matrix_4x4 := view_Transform * perspective_Transform;

      light_Sites              : constant openGL.Vector_3_array := (1 => (10_000.0,  10_000.0,  10_000.0),
                                                                    2 => (     0.0, -10_000.0,  0.0));
      Lights                   :          light_Set := Self.Lights.fetch;
   begin
      for Light of Lights
      loop
         Light.Site_is  (light_Sites (1));
         Light.inverse_view_Transform_is (inverse_view_Transform);
      end loop;

      --        Lights (1).Site_is  (light_Sites (1), inverse_view_Transform);
      --        Lights (2).Site_is  (light_Sites (2), inverse_view_Transform);

      Lights (1).Color_is (ambient  => (0.2, 0.2, 0.2, 0.0),
                           diffuse  => (0.3, 0.3, 0.3, 0.0),
                           specular => (0.91, 0.91, 0.91, 0.0));

      Lights (2).Color_is (ambient  => (0.2, 0.2, 0.2, 0.0),
                           diffuse  => (0.6, 0.1, 0.1, 0.0),
                           specular => (0.01, 0.01, 0.01, 0.0));
      if clear_Frame then
         Self.clear_Frame;
      end if;


      ---------------------
      --- Draw the visuals.
      --

      --  Collect opaque geometry (for state sorting) and collect lucid geometry (for depth sorting).
      --
      for Each in the_Visuals'Range
      loop
         declare
            use type Model.view,
                     Model.access_Geometry_views;

            the_Visual : Visual.view renames the_Visuals (Each);

         begin
            if    Boolean (the_Visual.Model.needs_Rebuild)
               or (    the_Visual.Model.opaque_Geometries = null
                   and the_Visual.Model. lucid_Geometries = null)
            then
               the_Visual.Model.create_GL_Geometries (Self.Textures'Access, Self.Fonts);

            elsif the_Visual.Model.is_Modified
            then
               the_Visual.Model.modify;
            end if;


            declare
               opaque_Geometries : Model.access_Geometry_views renames the_Visual.Model.opaque_Geometries;
               lucid_Geometries  : Model.access_Geometry_views renames the_Visual.Model. lucid_Geometries;
            begin
               the_Visual.mvp_Transform_is            (the_Visual.Transform * view_and_perspective_Transform);
               the_Visual.inverse_modelview_Matrix_is (inverse_Rotation (get_Rotation (the_Visual.Transform * view_Transform)));

               if opaque_Geometries /= null
               then
                  for i in opaque_Geometries'Range
                  loop
                     opaque_Count                           := opaque_Count + 1;
                     Self.all_opaque_Couples (opaque_Count) := (visual   => the_Visual,
                                                                Geometry => opaque_Geometries (i));
                  end loop;
               end if;

               if lucid_Geometries /= null
               then
                  for i in lucid_Geometries'Range
                  loop
                     lucid_Count                          := lucid_Count + 1;
                     Self.all_lucid_Couples (lucid_Count) := (visual   => the_Visual,
                                                              Geometry => lucid_Geometries (i));
                  end loop;
               end if;

            end;
         end;
      end loop;

      openGL.Errors.log;


      --  State sort opaque geometries and render them.
      --
      declare

         procedure heap_swap (L, R : in Natural)
         is
            Pad : constant visual_geometry_Couple := Self.all_opaque_Couples (L);
         begin
            Self.all_opaque_Couples (L) := Self.all_opaque_Couples (R);
            Self.all_opaque_Couples (R) := Pad;
         end heap_swap;


         function heap_LT (L, R : in Natural) return Boolean
         is
            use System;
            L_Geometry : openGL.Geometry.view renames Self.all_opaque_Couples (L).Geometry;
            R_Geometry : openGL.Geometry.view renames Self.all_opaque_Couples (R).Geometry;
         begin
            if L_Geometry.Program.gl_Program = R_Geometry.Program.gl_Program
            then
               return L_Geometry.all'Address < R_Geometry.all'Address;
            end if;

            return L_Geometry.Program.gl_Program < R_Geometry.Program.gl_Program;
         end heap_LT;


         the_Couple      : visual_geometry_Couple;
         current_Program : openGL.Program.view;

      begin
         if opaque_Count > 1
         then
            gnat.heap_Sort.sort (opaque_Count,
                                 heap_swap'unrestricted_Access,
                                 heap_LT  'unrestricted_Access);
         end if;

         glDisable   (GL_BLEND);
         glEnable    (GL_DEPTH_TEST);
         glDepthMask (gl_TRUE);                 -- Make depth buffer read/write.

         for Each in 1 .. opaque_Count
         loop
            the_Couple := Self.all_opaque_Couples (Each);

            if the_Couple.Geometry.Program /= current_Program
            then
               current_Program := the_Couple.Geometry.Program;
            end if;

            current_Program.enable;

            current_Program.mvp_Matrix_is               (the_Couple.Visual.mvp_Transform);
            current_Program.inverse_modelview_Matrix_is (the_Couple.Visual.inverse_modelview_Matrix);
            current_Program.directional_Light_is        (1, Lights (1));
            current_Program.directional_Light_is        (2, Lights (2));
            current_Program.Scale_is                    (the_Couple.Visual.Scale);


            if the_Couple.Visual.program_Parameters /= null then
               the_Couple.Visual.program_Parameters.enable;
            end if;

            the_Couple.Geometry.render;
         end loop;
      end;

      openGL.Errors.log;


      --  Depth sort lucid geometries and render them.
      --
      declare
         procedure heap_swap (L, R : in Natural)
         is
            Pad : constant visual_geometry_Couple := Self.all_lucid_Couples (L);
         begin
            Self.all_lucid_Couples (L) := Self.all_lucid_Couples (R);
            Self.all_lucid_Couples (R) := Pad;
         end heap_swap;


         function heap_LT (L, R : in Natural) return Boolean
         is
         begin
            return   Self.all_lucid_Couples (L).Visual.Transform (4, 3)   -- Depth_in_camera_space   -- nb: In camera space, negative Z is
                   < Self.all_lucid_Couples (R).Visual.Transform (4, 3);  --                                forward, so use '<'.
         end heap_LT;


         the_Couple      : visual_geometry_Couple;
         current_Program : openGL.Program.view;

      begin
         if lucid_Count > 1
         then
            gnat.heap_Sort.sort (lucid_Count,
                                 heap_swap'unrestricted_Access,
                                 heap_LT  'unrestricted_Access);
         end if;

         glDepthMask  (gl_False);     -- Make depth buffer read-only, for correct transparency.

         glEnable     (GL_BLEND);
         gl.lean.glBlendEquation (gl.lean.GL_FUNC_ADD);

         glBlendFunc (GL_SRC_ALPHA,
                      GL_ONE_MINUS_SRC_ALPHA);

         for Each in 1 .. lucid_Count
         loop
            the_Couple     := Self.all_lucid_Couples (Each);

            current_Program := the_Couple.Geometry.Program;
            current_Program.enable;

            current_Program.mvp_Matrix_is               (the_Couple.Visual.mvp_Transform);
            current_Program.inverse_modelview_Matrix_is (the_Couple.Visual.inverse_modelview_Matrix);
            current_Program.directional_Light_is        (1, Lights (1));
            current_Program.directional_Light_is        (2, Lights (2));
            current_Program.Scale_is                    (the_Couple.Visual.Scale);

            if the_Couple.Visual.program_Parameters /= null then
               the_Couple.Visual.program_Parameters.enable;
            end if;

            the_Couple.Geometry.render;
         end loop;

         glDepthMask (gl_True);
      end;

      openGL.Errors.log;
   end draw;



   ---------
   -- Lights
   --

   procedure Light_is (Self : in out Item;   Id  : in light_Id;
                                             Now : in openGL.Light.directional.item)
   is
   begin
      Self.Lights.set (Id, Now);
   end Light_is;



   function  Light    (Self : in out Item;   Id  : in light_Id) return openGL.Light.directional.item
   is
   begin
      return Self.Lights.fetch (Id);
   end Light;






   -----------------------------
   -- safe_camera_Map_of_updates
   --

   protected
   body safe_camera_Map_of_updates
   is
      procedure define
      is
      begin
         current_Map := Map_1'Unchecked_Access;
      end define;


      procedure destruct
      is
         use camera_Maps_of_updates;
         procedure free is new ada.Unchecked_Deallocation (updates_for_Camera,
                                                           updates_for_Camera_view);

         the_Updates : updates_for_Camera_view;
         Cursor      : camera_Maps_of_updates.Cursor := Map_1.First;

      begin
         while has_Element (Cursor)
         loop
            the_Updates := Element (Cursor);
            free (the_Updates);

            next (Cursor);
         end loop;

         Cursor := Map_2.First;

         while has_Element (Cursor)
         loop
            the_Updates := Element (Cursor);
            free (the_Updates);

            next (Cursor);
         end loop;

         current_Map := null;
      end destruct;


      procedure add (the_Updates : in impostor_Updates;
                     the_Camera  : in Camera_view)
      is
         the_camera_Updates :          updates_for_Camera_view;
         our_Camera         : constant Camera_view            := the_Camera;

      begin
         begin
            the_camera_Updates := current_Map.Element (our_Camera);
         exception
            when Constraint_Error =>     -- No element exists for this camera yet.
               the_camera_Updates := new updates_for_Camera;
               current_Map.insert (our_Camera, the_camera_Updates);
         end;

         declare
            First : constant Integer := the_camera_Updates.impostor_updates_Last + 1;
            Last  : constant Integer := the_camera_Updates.impostor_updates_Last + the_Updates'Length;
         begin
            the_camera_Updates.Impostor_updates (First .. Last) := the_Updates;
            the_camera_Updates.impostor_updates_Last            := Last;
         end;
      end add;


      procedure add (the_Visuals : in Visual.views;
                     the_Camera  : in Camera_view)
      is
         our_Camera         : constant Camera_view            := the_Camera;
         the_camera_Updates :          updates_for_Camera_view;

      begin
         begin
            the_camera_Updates := current_Map.Element (our_Camera);
         exception
            when Constraint_Error =>     -- No element exists for this camera yet.
               the_camera_Updates := new updates_for_Camera;
               current_Map.Insert (our_Camera, the_camera_Updates);
         end;

         declare
            First : constant Integer := the_camera_Updates.visuals_Last + 1;
            Last  : constant Integer := the_camera_Updates.visuals_Last + the_Visuals'Length;
         begin
            the_camera_Updates.Visuals (First .. Last) := the_Visuals;
            the_camera_Updates.visuals_Last            := Last;
         end;
      end add;



      procedure fetch_all_Updates (the_Updates : out camera_updates_Couples;
                                   Length      : out Natural)
      is
         use camera_Maps_of_updates;

         the_Couples : camera_updates_Couples (1 .. Integer (current_Map.Length));
         Cursor      : camera_Maps_of_updates.Cursor                             := current_Map.First;

      begin
         for i in the_Couples'Range
         loop
            the_Couples (i).Camera  := Key     (Cursor);
            the_Couples (i).Updates := Element (Cursor);

            next (Cursor);
         end loop;

         if current_Map = Map_1'unchecked_Access
         then   current_Map := Map_2'unchecked_Access;
         else   current_Map := Map_1'unchecked_Access;
         end if;

         the_Updates (1 .. the_Couples'Length) := the_Couples;
         Length                                := the_Couples'Length;
      end fetch_all_Updates;

   end safe_camera_Map_of_updates;



   --------------
   -- safe_Models
   --

   protected
   body safe_Models
   is
      procedure add (the_Model : in openGL.Model.view)
      is
      begin
         my_Count             := my_Count + 1;
         my_Models (my_Count) := the_Model;
      end add;

      procedure fetch (the_Models : out graphics_Models;
                       Count      : out Natural)
      is
      begin
         the_Models (1 .. my_Count) := my_Models (1 .. my_Count);
         Count                      := my_Count;
         my_Count                   := 0;
      end fetch;
   end safe_Models;



   procedure free (Self : in out Item;   the_Model : in openGL.Model.view)
   is
   begin
      Self.obsolete_Models.add (the_Model);
   end free;



   -----------------
   -- safe_Impostors
   --

   protected
   body safe_Impostors
   is
      procedure add (the_Impostor : in openGL.Impostor.view)
      is
      begin
         my_Count                := my_Count + 1;
         my_Impostors (my_Count) := the_Impostor;
      end add;

      procedure fetch (the_Impostors : out Impostor_Set;
                       Count         : out Natural)
      is
      begin
         the_Impostors (1 .. my_Count) := my_Impostors (1 .. my_Count);
         Count                         := my_Count;
         my_Count                      := 0;
      end fetch;
   end safe_Impostors;



   procedure free (Self : in out Item;   the_Impostor : in openGL.Impostor.view)
   is
   begin
      Self.obsolete_Impostors.add (the_Impostor);
   end free;



   -----------------
   -- safe_Lights
   --

   protected
   body safe_Lights
   is
      procedure set   (the_Light : in light_Id;
                      To         : in openGL.Light.directional.item)
      is
      begin
         my_Lights (the_Light) := To;
      end set;

      function  fetch return light_Set
      is
      begin
         return my_Lights;
      end fetch;
   end safe_Lights;



end openGL.Renderer.lean;
