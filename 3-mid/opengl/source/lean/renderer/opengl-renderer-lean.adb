with
     openGL.Viewport,
     openGL.Program,
     openGL.Camera,
     openGL.Palette,
     openGL.Model.billboard.textured,
     openGL.Geometry.        lit_textured_skinned,
     openGL.Geometry.lit_colored_textured_skinned,
     openGL.Font.texture,
     openGL.Server,
     openGL.Tasks,
     openGL.IO,
     openGL.Errors,

     GL.Binding,
     GL.lean,

     Interfaces.C,
     gnat.heap_Sort,
     System,

     ada.Text_IO,
     ada.Exceptions,
     ada.Task_Identification,
     ada.unchecked_Deallocation;


package body openGL.Renderer.lean
is
   use GL,
       Program,
       Interfaces.C,
       ada.Text_IO;

   ---------
   --- Forge
   --

   procedure define (Self : access Item)
   is
   begin
      Self.safe_Camera_updates_Map.define;
   end define;



   procedure destroy (Self : in out Item)
   is
      use Texture;
   begin
      Self.stop_Engine;

      while not Self.Engine'Terminated
      loop
         delay Duration'Small;
      end loop;

      Self.safe_Camera_updates_Map.destruct;

      declare
         procedure free is new ada.unchecked_Deallocation (visual_geometry_Couples,
                                                           visual_geometry_Couples_view);
      begin
         free (Self.all_opaque_Couples);
         free (Self.all_lucid_Couples);
      end;

      vacuum  (Self.texture_Pool);
      destroy (Self.texture_Pool);
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



   procedure queue_Impostor_updates (Self : in out Item;    the_Updates : in     impostor_Updates;
                                                            the_Camera  : access Camera.item'Class)
   is
   begin
      Self.safe_Camera_updates_Map.add (the_Updates,
                                        Camera_view (the_Camera));
   end queue_Impostor_updates;



   procedure queue_Visuals (Self : in out Item;    the_Visuals : in     Visual.views;
                                                   the_Camera  : access Camera.item'Class)
   is
   begin
      Self.safe_Camera_updates_Map.add (the_Visuals,
                                        Camera_view (the_Camera));
   end queue_Visuals;



   procedure update_Impostors_and_draw_Visuals (Self : in out Item;   all_Updates : in camera_updates_Couples)
   is
   begin
      for i in all_Updates'Range
      loop
         declare
            the_Camera  : constant Camera_view             := all_Updates (i).Camera;
            the_Updates : constant updates_for_Camera_view := all_Updates (i).Updates;
         begin
            Viewport.Extent_is ((the_Camera.Viewport.Max (1),
                                 the_Camera.Viewport.Max (2)));

            Self.update_Impostors (the_Updates.impostor_Updates (1 .. the_Updates.impostor_updates_Last),
                                   camera_world_Transform => the_Camera.World_Transform,
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
            Viewport.Extent_is ((the_Camera.Viewport.Max (1),
                                 the_Camera.Viewport.Max (2)));

            Self.draw (the_Visuals            => the_Updates.Visuals (1 .. the_Updates.visuals_Last),
                       camera_world_Transform => the_Camera.World_Transform,
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
      use Model;

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
      use Impostor;

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
      the_Context : Context.view with unreferenced;
      Done        : Boolean := False;

   begin
      select
         accept start (Context : openGL.Context.view)
         do
            the_Context := Context;     -- TODO: This is not used.
         end start;

         openGL.Tasks.Renderer_Task := ada.Task_Identification.current_Task;
         Self.context_Setter.all;

         put_Line ("openGL Server version: " & Server.Version);

      or
         accept Stop
         do
            Done := True;
         end Stop;
      end select;

      openGL.Geometry.        lit_textured_skinned.define_Program;
      openGL.Geometry.lit_colored_textured_skinned.define_Program;


      while not Done
      loop
         declare
            all_Updates        : camera_updates_Couples (1 .. 100);     -- Caters for 100 cameras.
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
                  Self.safe_Camera_updates_Map.fetch_all_Updates (all_Updates, Length);
               end render;

            or accept add_Font (font_Id : in Font.font_Id)
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
               IO.Screenshot (Filename   => to_String (new_snapshot_Name),
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

      -- Free any fonts.
      --
      while not Self.Fonts.is_Empty
      loop
         declare
            use Font,
                Font.font_id_Maps_of_font;
            the_Cursor : Cursor    := Self.Fonts.First;
            the_Font   : Font.view := Element (the_Cursor);
         begin
            free (the_Font);
            Self.Fonts.delete (the_Cursor);
         end;
      end loop;

   exception
      when E : others =>
         new_Line;
         put_Line ("Unhandled exception in openGL Renderer engine !");
         put_Line (ada.Exceptions.Exception_Information (E));
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



   procedure render (Self : in out Item;   to_Surface : in Surface.view := null)
   is
      pragma unreferenced (to_Surface);
   begin
      Self.Engine.render;
   end render;


   procedure add_Font (Self : in out Item;   font_Id : in Font.font_Id)
   is
   begin
      Self.Engine.add_Font (font_Id);
   end add_Font;



   procedure Screenshot (Self : in out Item;   Filename   : in String;
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
                                                     camera_world_Transform : in Matrix_4x4;
                                                     view_Transform         : in Matrix_4x4;
                                                     perspective_Transform  : in Matrix_4x4)
   is
      use linear_Algebra_3D;

      light_Site : constant Vector_3 := (10_000.0, -10_000.0, 10_000.0);
      the_Light  : openGL.Light.item;

   begin
      Tasks.check;

      the_Light. Site_is (light_Site);
      the_Light.Color_is (Palette.White);

      for i in the_Updates'Range
      loop
         declare
            use Texture,
                Visual,
                GL.Binding;

            the_Update     : impostor_Update renames the_Updates (i);
            the_Impostor   : Impostor.view   renames the_Update.Impostor;

            texture_Width  : constant gl.glSizei := power_of_2_Ceiling (Natural (the_Update.current_Width_pixels ));
            texture_Height : constant gl.glSizei := power_of_2_Ceiling (Natural (the_Update.current_Height_pixels));

            the_Model      : constant openGL.Model.billboard.textured.view
                                                 := openGL.Model.billboard.textured.view (the_Impostor.Visual.Model);
         begin
            the_Impostor.Visual.Scale_is         (the_Impostor.Target.Scale);
            the_Impostor.Visual.is_Terrain       (the_Impostor.Target.is_Terrain);
            the_Impostor.Visual.face_Count_is    (the_Impostor.Target.face_Count);
            the_Impostor.Visual.apparent_Size_is (the_Impostor.Target.apparent_Size);

            -- Render the target after clearing openGL buffers.
            --
            glClearColor (0.0, 0.0, 0.0,  0.0);
            glClear      (   GL_COLOR_BUFFER_BIT
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
            if        Natural (the_Update.current_Width_pixels)  /= the_Model.Texture.Size.Width
              or else Natural (the_Update.current_Height_pixels) /= the_Model.Texture.Size.Height
            then
               free (Self.texture_Pool, the_Model.Texture);
               the_Model.Texture_is (new_Texture (From => Self.texture_Pool'Access,
                                                  Size => (Natural (texture_Width),
                                                           Natural (texture_Height))));
            end if;

            -- Set texture coordinates.
            --
            declare
               X_first : constant Real := the_Impostor.expand_X;
               Y_first : constant Real := the_Impostor.expand_Y;
               X_last  : constant Real := Real (the_Update.current_Width_pixels)  / Real (texture_Width)  - X_First;
               Y_last  : constant Real := Real (the_Update.current_Height_pixels) / Real (texture_Height) - Y_First;
            begin
               the_Model.Texture_Coords_are ((1 => (S => X_first, T => Y_first),
                                              2 => (S => X_last,  T => Y_first),
                                              3 => (S => X_last,  T => Y_last),
                                              4 => (S => X_first, T => Y_last)));
            end;

            the_Model.Texture.enable;

            GL.lean.glCopyTexSubImage2D (gl.GL_TEXTURE_2D,                 0,
                                         the_Update.current_copy_x_Offset, the_Update.current_copy_y_Offset,
                                         the_Update.current_copy_X,        the_Update.current_copy_Y,
                                         the_Update.current_copy_Width,    the_Update.current_copy_Height);
            Errors.log;
         end;
      end loop;

      Errors.log;
   end update_Impostors;



   procedure draw (Self : in out Item;   the_Visuals            : in Visual.views;
                                         camera_world_Transform : in Matrix_4x4;
                                         view_Transform         : in Matrix_4x4;
                                         perspective_Transform  : in Matrix_4x4;
                                         clear_Frame            : in Boolean;
                                         to_Surface             : in Surface.view := null)
   is
      pragma unreferenced (to_Surface);

      use linear_Algebra_3D;

      opaque_Count : math.Index := 0;
      lucid_Count  : math.Index := 0;

      view_and_perspective_Transform : constant Matrix_4x4 := view_Transform * perspective_Transform;


      function get_on_Lights return openGL.Light.items
      is
         all_Lights : constant openGL.Light.items := Self.Lights.fetch;
         lit_Lights :          openGL.Light.items (all_Lights'Range);
         Count      :          Natural            := 0;
      begin
         for i in all_Lights'Range
         loop
            if all_Lights (i).is_On
            then
               Count              := Count + 1;
               lit_Lights (Count) := all_Lights (i);
            end if;
         end loop;

         return lit_Lights (1 .. Count);
      end get_on_Lights;

      Lights : constant openGL.Light.items := get_on_Lights;

   begin
      Tasks.check;

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
            if    the_Visual.Model.needs_Rebuild
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
               the_Visual.mvp_Transform_is (the_Visual.Transform * view_and_perspective_Transform);

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

      Errors.log;

      --  State sort opaque geometries and render them.
      --
      declare
         use GL.Binding;

         procedure Heap_swap (Left, Right : in Natural)
         is
            Pad : constant visual_geometry_Couple := Self.all_opaque_Couples (Left);
         begin
            Self.all_opaque_Couples (Left)  := Self.all_opaque_Couples (Right);
            Self.all_opaque_Couples (Right) := Pad;
         end Heap_swap;


         function Heap_less_than (Left, Right : in Natural) return Boolean
         is
            use System;
            L_Geometry : openGL.Geometry.view renames Self.all_opaque_Couples (Left) .Geometry;
            R_Geometry : openGL.Geometry.view renames Self.all_opaque_Couples (Right).Geometry;
         begin
            if L_Geometry.Program.gl_Program = R_Geometry.Program.gl_Program
            then
               return L_Geometry.all'Address < R_Geometry.all'Address;
            end if;

            return L_Geometry.Program.gl_Program < R_Geometry.Program.gl_Program;
         end Heap_less_than;

         the_Couple      : visual_geometry_Couple;
         current_Program : openGL.Program.view;

      begin
         if opaque_Count > 1
         then
            gnat.heap_Sort.sort (opaque_Count,
                                 Heap_swap     'unrestricted_Access,
                                 Heap_less_than'unrestricted_Access);
         end if;

         glDisable   (GL_BLEND);
         glEnable    (GL_DEPTH_TEST);
         glDepthMask (gl_TRUE);           -- Make depth buffer read/write.

         for Each in 1 .. opaque_Count
         loop
            the_Couple := Self.all_opaque_Couples (Each);

            if the_Couple.Geometry.Program /= current_Program
            then
               current_Program := the_Couple.Geometry.Program;
            end if;

            current_Program.enable;                                               -- TODO: Only need to do this when program changes ?
            current_Program.mvp_Transform_is (the_Couple.Visual.mvp_Transform);
            current_Program.model_Matrix_is  (the_Couple.Visual.Transform);
            current_Program.camera_Site_is   (get_Translation (camera_world_Transform));
            current_Program.Lights_are       (Lights);
            current_Program.Scale_is         (the_Couple.Visual.Scale);

            if the_Couple.Visual.program_Parameters /= null then
               the_Couple.Visual.program_Parameters.enable;
            end if;

            the_Couple.Geometry.render;
         end loop;
      end;

      Errors.log;

      --  Depth sort lucid geometries and render them.
      --
      declare
         use GL.Binding;

         procedure Heap_swap (Left, Right : in Natural)
         is
            Pad : constant visual_geometry_Couple := Self.all_lucid_Couples (Left);
         begin
            Self.all_lucid_Couples (Left)  := Self.all_lucid_Couples (Right);
            Self.all_lucid_Couples (Right) := Pad;
         end Heap_swap;


         function Heap_less_than (Left, Right : in Natural) return Boolean
         is
         begin
            return   Self.all_lucid_Couples (Left) .Visual.Transform (4, 3)   -- Depth_in_camera_space   -- NB: In camera space, negative Z is
                   < Self.all_lucid_Couples (Right).Visual.Transform (4, 3);  --                                forward, so use '<'.
         end Heap_less_than;

         the_Couple      : visual_geometry_Couple;
         current_Program : openGL.Program.view;

      begin
         if lucid_Count > 1
         then
            gnat.heap_Sort.sort (lucid_Count,
                                 Heap_swap     'unrestricted_Access,
                                 Heap_less_than'unrestricted_Access);
         end if;

         glDepthMask  (gl_False);     -- Make depth buffer read-only, for correct transparency.

         glEnable     (GL_BLEND);
         gl.lean.glBlendEquation (gl.lean.GL_FUNC_ADD);

         glBlendFunc (GL_SRC_ALPHA,
                      GL_ONE_MINUS_SRC_ALPHA);

         for Each in 1 .. lucid_Count
         loop
            the_Couple := Self.all_lucid_Couples (Each);

            current_Program := the_Couple.Geometry.Program;     -- TODO: Only do this when program changes (as is done above with opaques) ?
            current_Program.enable;
            current_Program.mvp_Transform_is (the_Couple.Visual.mvp_Transform);
            current_Program.camera_Site_is   (get_Translation (camera_world_Transform));
            current_Program.model_Matrix_is  (the_Couple.Visual.Transform);
            current_Program.Lights_are       (Lights);
            current_Program.Scale_is         (the_Couple.Visual.Scale);

            if the_Couple.Visual.program_Parameters /= null then
               the_Couple.Visual.program_Parameters.enable;
            end if;

            the_Couple.Geometry.render;
         end loop;

         glDepthMask (gl_True);
      end;

      Errors.log;
   end draw;



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
         procedure deallocate is new ada.unchecked_Deallocation (updates_for_Camera,
                                                                 updates_for_Camera_view);

         the_Updates : updates_for_Camera_view;
         Cursor      : camera_Maps_of_updates.Cursor := Map_1.First;

      begin
         while has_Element (Cursor)
         loop
            the_Updates := Element (Cursor);
            deallocate (the_Updates);

            next (Cursor);
         end loop;

         Cursor := Map_2.First;

         while has_Element (Cursor)
         loop
            the_Updates := Element (Cursor);
            deallocate (the_Updates);

            next (Cursor);
         end loop;

         current_Map := null;
      end destruct;


      procedure add (the_Updates : in impostor_Updates;
                     the_Camera  : in Camera_view)
      is
         the_camera_Updates : updates_for_Camera_view;
         our_Camera         : constant Camera_view   := the_Camera;

      begin
         begin
            the_camera_Updates := current_Map.Element (our_Camera);
         exception
            when constraint_Error =>     -- No element exists for this camera yet.
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
         the_camera_Updates : updates_for_Camera_view;
         our_Camera         : constant Camera_view   := the_Camera;

      begin
         begin
            the_camera_Updates := current_Map.Element (our_Camera);
         exception
            when constraint_Error =>     -- No element exists for this camera yet.
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
         Cursor      : camera_Maps_of_updates.Cursor := current_Map.First;

      begin
         for i in the_Couples'Range
         loop
            the_Couples (i).Camera  := Key     (Cursor);
            the_Couples (i).Updates := Element (Cursor);

            next (Cursor);
         end loop;

         if current_Map = Map_1'unrestricted_Access
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
      procedure add (the_Model : in Model.view)
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



   procedure free (Self : in out Item;   the_Model : in Model.view)
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
      procedure add (the_Impostor : in Impostor.view)
      is
      begin
         the_Count                 := the_Count + 1;
         the_Impostors (the_Count) := the_Impostor;
      end add;

      procedure fetch (Impostors : out Impostor_Set;
                       Count     : out Natural)
      is
      begin
         Impostors (1 .. the_Count) := the_Impostors (1 .. the_Count);
         Count                      := the_Count;
         the_Count                  := 0;
      end fetch;
   end safe_Impostors;



   procedure free (Self : in out Item;   the_Impostor : in Impostor.view)
   is
   begin
      Self.obsolete_Impostors.add (the_Impostor);
   end free;


   ---------
   -- Lights
   --

   function Hash (Id : in openGL.Light.Id_t) return ada.Containers.Hash_type
   is
   begin
      return ada.Containers.Hash_type (Id);
   end Hash;


   protected
   body safe_Lights
   is
      procedure add (Light : in openGL.Light.item)
      is
      begin
         the_Lights.insert (Light.Id,
                            Light);
      end add;


      procedure set (Light : in openGL.Light.item)
      is
      begin
         the_Lights.replace (Light.Id,
                             Light);
      end set;


      procedure rid (Light : in openGL.Light.item)
      is
      begin
         the_Lights.delete (Light.Id);
      end rid;


      function get (Id : in openGL.Light.Id_t) return openGL.Light.item
      is
      begin
         return the_Lights.Element (Id);
      end get;


      function fetch return openGL.Light.items
      is
         all_Lights : openGL.Light.items (1 .. Natural (the_Lights.Length));
         i          : Natural := 0;
      begin
         for Each of the_Lights
         loop
            i              := i + 1;
            all_Lights (i) := Each;
         end loop;

         return all_Lights;
      end fetch;

   end safe_Lights;



   function new_Light (Self : in out Item) return openGL.Light.item
   is
      the_Light : openGL.Light.item;
   begin
      Self.prior_Light_Id := Self.prior_Light_Id + 1;
      the_Light.Id_is (Self.prior_Light_Id);
      Self.Lights.add (the_Light);

      return the_Light;
   end new_Light;


   procedure set (Self : in out Item;   the_Light : in openGL.Light.item)
   is
   begin
      Self.Lights.set (the_Light);
   end set;


   procedure rid (Self : in out Item;   the_Light : in openGL.Light.item)
   is
   begin
      Self.Lights.rid (the_Light);
   end rid;


   function Light (Self : in out Item;   Id : in openGL.light.Id_t) return openGL.Light.item
   is
   begin
      return Self.Lights.get (Id);
   end Light;


   function fetch (Self : in out Item) return openGL.Light.items
   is
   begin
      return Self.Lights.fetch;
   end fetch;


end openGL.Renderer.lean;
