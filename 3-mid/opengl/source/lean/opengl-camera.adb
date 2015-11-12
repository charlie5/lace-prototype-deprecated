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

   procedure define  (Self : in out Item)
   is
   begin
      Self.Culler    .define;
      Self.Impostorer.define;

      Self.world_Transform := Identity_4x4;
      Self.view_Transform  := Identity_4x4;
      Self.Viewport        := (min => (0, 0),
                               max => (0, 0));
   end define;



   procedure destroy (Self : in out Item)
   is
   begin
      Self.cull_Engine.stop;
   end destroy;



   --------------
   --  Attributes
   --

   function to_world_Site (Self : in Item'Class;   window_Site : in math.Vector_3) return math.Vector_3
   is
      use math.Vectors;

      perspective_Transform : constant math.Matrix_4x4 := to_Perspective (fovy   => 60.0,
                                                                          aspect => Self.Aspect, -- 1200.0 / 1200.0,
                                                                          zNear  => Self.near_plane_Distance,
                                                                          zFar   => Self.far_plane_Distance);
      Viewport              : constant Rectangle       := Self.Viewport;
      Position_window_space : constant Vector_3        := (window_Site (1),
                                                           Real (Viewport.Max (2)) - window_Site (2),
                                                           window_Site (3));
      Site_world_space      : constant Vector_3        := unProject (Position_window_space,
                                                                     modelMatrix => Self.view_Transform,
                                                                     projMatrix  => perspective_Transform,
                                                                     viewport    => Viewport);
   begin
      return Site_world_space;
   end to_world_Site;



   procedure Site_is (Self : in out Item'Class;   Now : in math.Vector_3)
   is
      use math.Vectors;
   begin
      Self.world_Transform := to_transform_Matrix ((Self.Spin,
                                                    Now));
      Self.update_view_Transform;
   end Site_is;



   function  Site (Self : in     Item'Class) return  math.Vector_3
   is
   begin
      return get_Translation (Self.world_Transform);
   end Site;



   procedure Position_is (Self : in out Item'Class;   Site : in math.Vector_3;
                                                      Spin : in math.Matrix_3x3)
   is
      use math.Vectors;
   begin
      Self.world_Transform := to_transform_Matrix ((Spin,
                                                    Site));
      Self.update_view_Transform;
   end Position_is;



   procedure Spin_is (Self : in out Item'Class;   Now : in math.Matrix_3x3)
   is
      use math.Vectors;
   begin
      set_Rotation (Self.world_Transform, to => Now);
      Self.update_view_Transform;
   end Spin_is;


   function  Spin (Self : in     Item'Class) return math.Matrix_3x3
   is
   begin
      return get_Rotation (Self.world_Transform);
   end Spin;



   function world_Transform (Self : in     Item) return math.Matrix_4x4
   is
   begin
      return Self.world_Transform;
   end world_Transform;



   function FOVy   (Self : in Item'Class) return math.Degrees
   is
   begin
      return Self.FOVy;
   end FOVy;


   function Aspect (Self : in Item'Class) return math.Real
   is
   begin
      return Self.Aspect;
   end Aspect;


   procedure Aspect_is (Self : in out Item'Class;   Now : in math.Real)
   is
   begin
      Self.Aspect := Now;
   end Aspect_is;



   function near_plane_Distance (Self : in Item'Class) return math.Real
   is
   begin
      return Self.near_plane_Distance;
   end near_plane_Distance;


   procedure near_plane_Distance_is (Self : in out Item'Class;   Now : in math.Real)
   is
   begin
      Self.near_plane_Distance := Now;
   end near_plane_Distance_is;



   function far_plane_Distance (Self : in Item'Class) return math.Real
   is
   begin
      return Self.far_plane_Distance;
   end far_plane_Distance;


   procedure far_plane_Distance_is  (Self : in out Item'Class;   Now : in math.Real)
   is
   begin
      Self.far_plane_Distance := Now;
   end far_plane_Distance_is;



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



   procedure Viewport_is (Self : in out Item'Class;   width, height : in Integer)
   is
      deg2rad                  : constant := pi / 180.0;
      half_fov_max_rads        : Real;
      Tan_of_half_fov_max_rads : Real;

   begin
      Self.Viewport.Min (1) := 0;
      Self.Viewport.Min (2) := 0;

      Self.Viewport.Max (1) := width  - 1;
      Self.Viewport.Max (2) := height - 1;

      if   Width  = 0
        or Height = 0
      then   Self.Aspect := 1.0;
      else   Self.Aspect := Real (Width) / Real (Height);
      end if;

      half_fov_max_rads        := 0.5 * Real (Self.FOVy)   * deg2rad;
      Tan_of_half_fov_max_rads := Tan (half_fov_max_rads);

      Self.near_plane_Height   := Self.near_plane_Distance * Tan_of_half_fov_max_rads;
      Self.near_plane_Width    := Self.near_plane_Height   * Self.Aspect;

      Self.far_plane_Height    := Self.far_plane_Distance  * Tan_of_half_fov_max_rads;
      Self.far_plane_Width     := Self.far_plane_Height    * Self.Aspect;

      if Self.aspect > 1.0
      then -- X side angle broader than y side angle.
         half_fov_max_rads := arcTan (Self.aspect * Tan_of_half_fov_max_rads);
      end if;

      Self.projection_Transform := to_Perspective (fovy   => Real (Self.FOVy),
                                                   aspect => Self.Aspect,
                                                   zNear  => Self.near_plane_Distance,
                                                   zFar   => Self.far_plane_Distance);
   end Viewport_is;



   function  Viewport (Self : in Item) return linear_Algebra_3d.Rectangle
   is
   begin
      return Self.Viewport;
   end Viewport;



   procedure Renderer_is (Self : in out Item'Class;   Now : in openGL.Renderer.lean.view)
   is
   begin
      Self.Renderer := Now;
   end Renderer_is;



   function cull_Completed (Self : in Item)return Boolean
   is
   begin
      return Boolean (Self.cull_Completed);
   end cull_Completed;



   procedure disable_Cull (Self : in out Item)
   is
   begin
      Self.is_Culling := False;
   end disable_Cull;



   function  vanish_point_size_Min (Self : in     Item'Class) return Real
   is
   begin
      return Self.Culler.vanish_point_size_Min;
   end vanish_point_size_Min;



   procedure vanish_point_size_Min_is (Self : in out Item'Class;   Now : in Real)
   is
   begin
      Self.Culler.vanish_point_size_Min_is (Now);
   end vanish_point_size_Min_is;



   -- Impostors
   --

   function  impostor_size_Min (Self : in     Item) return Real
   is
   begin
      return Self.Impostorer.impostor_size_Min;
   end impostor_size_Min;


   procedure impostor_size_Min_is (Self : in out Item;   Now : in Real)
   is
   begin
      Self.Impostorer.impostor_size_Min_is (Now);
   end impostor_size_Min_is;



   procedure allow_Impostors (Self : in out Item;   Now : in Boolean := True)
   is
   begin
      Self.impostors_Allowed := Now;
   end allow_Impostors;



   --------------
   --  Operations
   --

   task
   body cull_Engine
   is
      Done             : Boolean := False;
      Culling          : Boolean;

      all_Visuals      : openGL.Visual.views (1 .. 20_000);
      all_visuals_Last : Natural;

   begin
      loop
         select
            accept stop
            do
               Done := True;
            end stop;
         or
            accept cull  (the_Visuals : in Visual.views;   do_Cull : in Boolean)
            do
               all_Visuals (the_Visuals'Range) := the_Visuals;
               all_visuals_Last                := the_Visuals'Last;

               Culling             := do_Cull;
               Self.cull_Completed := False;
            end cull;
         end select;

         exit when Done;

         declare
            use math.Vectors;


            function get_Visuals return Visual.views
            is
            begin
               if Culling
               then
                  return Self.Culler.cull (the_Visuals    => all_Visuals (1 .. all_visuals_Last),
                                           camera_Frustum => Self.current_Planes,
                                           camera_Site    => Self.Site);
               else
                  return all_Visuals (1 .. all_visuals_Last);
               end if;
            end get_Visuals;


            the_Visuals : Visual.views := get_Visuals;

         begin
            if Self.impostors_Allowed
            then
               Self.Impostorer.Renderer_is (Self.Renderer);
               Self.Impostorer.substitute  (the_Visuals,
                                            Camera => Self);
            end if;

            Self.Renderer.queue_Visuals (the_Visuals, Self);

            Self.cull_Completed := True;
         end;
      end loop;

      Self.Impostorer.destruct;

   exception
      when E : others =>
         new_Line;
         put_Line ("Unhandled exception in openGL camera Cull engine !");
         put_Line (ada.Exceptions.Exception_Information (E));
   end cull_Engine;




   procedure render (Self : in out Item;   the_Sprites : in     Visual.views;
                                           To          : in     Surface.view := null)
   is
      pragma Unreferenced (To);
   begin
      Self.cull_Engine.cull (the_Sprites, do_cull => Self.is_Culling);
   end render;




   function current_Planes (Self : in Item) return openGL.Frustum.plane_Array
   is
      use openGL.Frustum;

      the_Planes :          Frustum.plane_Array;

      Proj       : constant Matrix_4x4 := Self.projection_Transform;
      Modl       : constant Matrix_4x4 := Self.view_Transform;
      Clip       :          Matrix_4x4 := Modl * Proj;

   begin
      -- Extract the RIGHT plane.
      --
      the_Planes (Right)(1) := clip( 1,4) - clip( 1,1);
      the_Planes (Right)(2) := clip( 2,4) - clip( 2,1);
      the_Planes (Right)(3) := clip( 3,4) - clip( 3,1);
      the_Planes (Right)(4) := clip( 4,4) - clip( 4,1);

      -- Extract the LEFT plane.
      --
      the_Planes (Left)(1) := clip( 1,4) + clip( 1,1);
      the_Planes (Left)(2) := clip( 2,4) + clip( 2,1);
      the_Planes (Left)(3) := clip( 3,4) + clip( 3,1);
      the_Planes (Left)(4) := clip( 4,4) + clip( 4,1);

      -- Extract the LOW plane.
      --
      the_Planes (Low)(1) := clip( 1,4) + clip( 1,2);
      the_Planes (Low)(2) := clip( 2,4) + clip( 2,2);
      the_Planes (Low)(3) := clip( 3,4) + clip( 3,2);
      the_Planes (Low)(4) := clip( 4,4) + clip( 4,2);

      -- Extract the HIGH plane.
      --
      the_Planes (High)(1) := clip( 1,4) - clip( 1,2);
      the_Planes (High)(2) := clip( 2,4) - clip( 2,2);
      the_Planes (High)(3) := clip( 3,4) - clip( 3,2);
      the_Planes (High)(4) := clip( 4,4) - clip( 4,2);

      -- Extract the FAR plane.
      --
      the_Planes (Far)(1) := clip( 1,4) - clip( 1,3);
      the_Planes (Far)(2) := clip( 2,4) - clip( 2,3);
      the_Planes (Far)(3) := clip( 3,4) - clip( 3,3);
      the_Planes (Far)(4) := clip( 4,4) - clip( 4,3);

      -- Extract the NEAR plane.
      --
      the_Planes (Near)(1) := clip( 1,4) + clip( 1,3);
      the_Planes (Near)(2) := clip( 2,4) + clip( 2,3);
      the_Planes (Near)(3) := clip( 3,4) + clip( 3,3);
      the_Planes (Near)(4) := clip( 4,4) + clip( 4,3);

      normalise (the_Planes);
      return     the_Planes;
   end current_Planes;



   procedure update_view_Transform (Self : in out Item)
   is
   begin
      Self.view_Transform := inverse_Transform (Self.world_Transform);
   end update_view_Transform;


end openGL.Camera;
