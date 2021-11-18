with
     ada.Text_IO,
     ada.Exceptions;

package body openGL.Camera
is
   use math.Algebra.linear,
       math.Algebra.linear.d3,
       ada.Text_IO;

   ---------
   --  Forge
   --

   procedure define (Self : in out Item)
   is
   begin
      Self.Culler    .define;
      Self.Impostorer.define;

      Self.world_Transform := Identity_4x4;
      Self. view_Transform := Identity_4x4;
      Self.Viewport        := (Min => (0, 0),
                               Max => (0, 0));
   end define;


   procedure destroy (Self : in out Item)
   is
   begin
      Self.cull_Engine.stop;
   end destroy;


   --------------
   --  Attributes
   --

   function to_World_Site (Self : in Item;   Window_Site : in math.Vector_3) return math.Vector_3
   is
      perspective_Transform : constant math.Matrix_4x4 := to_Perspective (FoVy   => Self.FoVy,
                                                                          Aspect => Self.Aspect,
                                                                          zNear  => Self.near_Plane_Distance,
                                                                          zFar   => Self. far_Plane_Distance);
      Viewport              : constant Rectangle := Self.Viewport;
      Position_window_space : constant Vector_3  := (Window_Site (1),
                                                     Real (Viewport.Max (2)) - Window_Site (2),
                                                     Window_Site (3));
      Site_world_space      : constant Vector_3  := unProject (Position_window_space,
                                                               Model      => Self.view_Transform,
                                                               Projection => perspective_Transform,
                                                               Viewport   => Viewport);
   begin
      return Site_world_space;
   end to_World_Site;



   procedure Site_is (Self : in out Item;   now : in math.Vector_3)
   is
   begin
      Self.world_Transform := to_transform_Matrix ((Self.Spin,
                                                    now));
      Self.update_View_Transform;
   end Site_is;


   function Site (Self : in Item) return math.Vector_3
   is
   begin
      return get_Translation (Self.world_Transform);
   end Site;



   procedure Position_is (Self : in out Item'Class;   Site : in math.Vector_3;
                                                      Spin : in math.Matrix_3x3)
   is
   begin
      Self.world_Transform := to_transform_Matrix ((Spin,
                                                    Site));
      Self.update_View_Transform;
   end Position_is;



   procedure Spin_is (Self : in out Item'Class;   now : in math.Matrix_3x3)
   is
   begin
      set_Rotation (Self.world_Transform, to => now);
      Self.update_View_Transform;
   end Spin_is;


   function Spin (Self : in Item'Class) return math.Matrix_3x3
   is
   begin
      return get_Rotation (Self.world_Transform);
   end Spin;



   function World_Transform (Self : in Item) return math.Matrix_4x4
   is
   begin
      return Self.world_Transform;
   end World_Transform;


   function FoVy (Self : in Item'Class) return math.Degrees
   is
   begin
      return Self.FoVy;
   end FOVy;


   procedure FoVy_is (Self : in out Item'Class;   Now : in math.Degrees)
   is
   begin
      Self.FoVy := Now;
   end FoVy_is;



   function Aspect (Self : in Item'Class) return math.Real
   is
   begin
      return Self.Aspect;
   end Aspect;


   procedure Aspect_is (Self : in out Item'Class;   now : in math.Real)
   is
   begin
      Self.Aspect := now;
   end Aspect_is;



   function near_Plane_Distance (Self : in Item'Class) return math.Real
   is
   begin
      return Self.near_Plane_Distance;
   end near_Plane_Distance;


   procedure near_Plane_Distance_is (Self : in out Item'Class;   now : in math.Real)
   is
   begin
      Self.near_Plane_Distance := now;
   end near_Plane_Distance_is;



   function far_Plane_Distance (Self : in Item'Class) return math.Real
   is
   begin
      return Self.far_Plane_Distance;
   end far_Plane_Distance;


   procedure far_Plane_Distance_is (Self : in out Item'Class;   now : in math.Real)
   is
   begin
      Self.far_Plane_Distance := now;
   end far_Plane_Distance_is;



   function view_Transform (Self : in Item'Class) return math.Matrix_4x4
   is
   begin
      return Self.view_Transform;
   end view_Transform;


   function projection_Transform (Self : in Item'Class) return math.Matrix_4x4
   is
   begin
      return Self.projection_Transform;
   end projection_Transform;



   procedure Viewport_is (Self : in out Item'Class;   Width, Height : in Positive)
   is
      use real_Functions;

      half_FoV_max        : Radians       := to_Radians (0.5 * Self.FoVy);
      Tan_of_half_FoV_max : constant Real := Tan (half_FoV_max);

   begin
      Self.Viewport.Min (1) := 0;
      Self.Viewport.Min (2) := 0;

      Self.Viewport.Max (1) := Width  - 1;
      Self.Viewport.Max (2) := Height - 1;

      Self.Aspect := Real (Width) / Real (Height);

      Self.near_plane_Height := Self.near_plane_Distance * Tan_of_half_FoV_max;
      Self.near_plane_Width  := Self.near_plane_Height   * Self.Aspect;

      Self.far_plane_Height  := Self.far_plane_Distance  * Tan_of_half_FoV_max;
      Self.far_plane_Width   := Self.far_plane_Height    * Self.Aspect;

      if Self.Aspect > 1.0
      then -- X side angle broader than y side angle.
         half_FoV_max := arcTan (Self.aspect * Tan_of_half_FoV_max);     -- TODO: 'half_FoV_max' is not used after here. Why is it set ?
      end if;

      Self.projection_Transform := to_Perspective (FoVy   => Self.FoVy,
                                                   Aspect => Self.Aspect,
                                                   zNear  => Self.near_Plane_Distance,
                                                   zFar   => Self. far_Plane_Distance);
   end Viewport_is;



   function Viewport (Self : in Item) return linear_Algebra_3d.Rectangle
   is
   begin
      return Self.Viewport;
   end Viewport;


   procedure Renderer_is (Self : in out Item;   now : in Renderer.lean.view)
   is
   begin
      Self.Renderer := now;
   end Renderer_is;


   function cull_completed (Self : in Item) return Boolean
   is
   begin
      return Boolean (Self.cull_Completed);
   end cull_completed;


   procedure disable_cull (Self : in out Item)
   is
   begin
      Self.is_Culling := False;
   end disable_cull;



   function vanish_Point_Size_min (Self : in Item'Class) return Real
   is
   begin
      return Self.Culler.vanish_Point_Size_min;
   end vanish_Point_Size_min;


   procedure vanish_Point_Size_min_is (Self : in out Item'Class;   now : in Real)
   is
   begin
      Self.Culler.vanish_Point_Size_min_is (now);
   end vanish_Point_Size_min_is;



   -- Impostors
   --

   function Impostor_Size_min (Self : in Item) return Real
   is
   begin
      return Self.Impostorer.Impostor_Size_min;
   end Impostor_Size_min;


   procedure Impostor_Size_min_is (Self : in out Item;   now : in Real)
   is
   begin
      Self.Impostorer.Impostor_Size_min_is (now);
   end Impostor_Size_min_is;


   procedure allow_Impostors (Self : in out Item;   now : in Boolean := True)
   is
   begin
      Self.Impostors_allowed := now;
   end allow_Impostors;



   ----------
   --  Engine
   --
   task body cull_Engine
   is
      Done             : Boolean := False;
      culling          : Boolean;

      all_Visuals      : openGL.Visual.views (1 .. 20_000);
      all_Visuals_last : Natural;

   begin
      loop
         select
            accept stop
            do
               Done := True;
            end stop;
         or
            accept cull (the_Visuals : in Visual.views;   do_cull : in Boolean)
            do
               all_Visuals (the_Visuals'Range) := the_Visuals;
               all_visuals_Last                := the_Visuals'Last;

               culling             := do_cull;
               Self.Cull_completed := False;
            end cull;
         end select;

         exit when Done;

         declare
            function get_Visuals return Visual.views
            is
            begin
               if culling
               then
                  return Self.Culler.cull (the_Visuals    => all_Visuals (1 .. all_Visuals_last),
                                           Camera_Frustum => Self.current_Planes,
                                           Camera_Site    => Self.Site);
               else
                  return all_Visuals (1 .. all_visuals_Last);
               end if;
            end get_Visuals;

            the_Visuals : Visual.views := get_Visuals;

         begin
            if Self.Impostors_allowed
            then
               Self.Impostorer.Renderer_is (Self.Renderer);
               Self.Impostorer.substitute  (the_Visuals,
                                            Camera => Self);
            end if;

            Self.Renderer.queue_Visuals (the_Visuals, Self);

            Self.Cull_completed := True;
         end;
      end loop;

      Self.Impostorer.destruct;

   exception
      when E : others =>
         new_Line;
         put_Line ("Unhandled exception in openGL camera Cull engine.");
         put_Line (ada.Exceptions.Exception_Information (E));
   end cull_Engine;



   --------------
   --  Operations
   --

   procedure render (Self : in out Item;   Visuals : in Visual.views;
                                           to      : in Surface.view := null)
   is
      pragma Unreferenced (To);     -- TODO: Finish using surfaces.
   begin
      Self.cull_Engine.cull (Visuals, do_cull => Self.is_Culling);
   end render;



   function current_Planes (Self : in Item) return openGL.Frustum.Plane_array
   is
      use openGL.Frustum;

      the_Planes : Frustum.Plane_array;

      Projection : constant Matrix_4x4 := Self.projection_Transform;
      Model      : constant Matrix_4x4 := Self.view_Transform;
      Clip       : constant Matrix_4x4 := Model * Projection;

   begin
      -- Extract the Right plane.
      --
      the_Planes (Right)(1) := clip (1,4) - clip (1,1);
      the_Planes (Right)(2) := clip (2,4) - clip (2,1);
      the_Planes (Right)(3) := clip (3,4) - clip (3,1);
      the_Planes (Right)(4) := clip (4,4) - clip (4,1);

      -- Extract the Left plane.
      --
      the_Planes (Left)(1)  := clip (1,4) + clip (1,1);
      the_Planes (Left)(2)  := clip (2,4) + clip (2,1);
      the_Planes (Left)(3)  := clip (3,4) + clip (3,1);
      the_Planes (Left)(4)  := clip (4,4) + clip (4,1);

      -- Extract the Low plane.
      --
      the_Planes (Low)(1)   := clip (1,4) + clip (1,2);
      the_Planes (Low)(2)   := clip (2,4) + clip (2,2);
      the_Planes (Low)(3)   := clip (3,4) + clip (3,2);
      the_Planes (Low)(4)   := clip (4,4) + clip (4,2);

      -- Extract the High plane.
      --
      the_Planes (High)(1)  := clip (1,4) - clip (1,2);
      the_Planes (High)(2)  := clip (2,4) - clip (2,2);
      the_Planes (High)(3)  := clip (3,4) - clip (3,2);
      the_Planes (High)(4)  := clip (4,4) - clip (4,2);

      -- Extract the Far plane.
      --
      the_Planes (Far)(1)   := clip (1,4) - clip (1,3);
      the_Planes (Far)(2)   := clip (2,4) - clip (2,3);
      the_Planes (Far)(3)   := clip (3,4) - clip (3,3);
      the_Planes (Far)(4)   := clip (4,4) - clip (4,3);

      -- Extract the Near plane.
      --
      the_Planes (Near)(1)  := clip (1,4) + clip (1,3);
      the_Planes (Near)(2)  := clip (2,4) + clip (2,3);
      the_Planes (Near)(3)  := clip (3,4) + clip (3,3);
      the_Planes (Near)(4)  := clip (4,4) + clip (4,3);

      normalise (the_Planes);
      return     the_Planes;
   end current_Planes;



   procedure update_View_Transform (Self : in out Item)
   is
   begin
      Self.view_Transform := inverse_Transform (Self.world_Transform);
   end update_View_Transform;


end openGL.Camera;
