with
     openGL.Visual,
     ada.unchecked_Deallocation;


package body mmi.Camera
is
   use math.Algebra.linear,
       math.Algebra.linear.d3;



   --------
   -- Forge
   --

   procedure define  (Self : in out Item)
   is
   begin
      Self.view_Transform := Look_at (eye    => Self.Clipper.eye_position,
                                      center => (Self.Clipper.eye_position (1), Self.Clipper.eye_position (2), -100.0),
                                      up     => (0.0, 1.0, 0.0));

      Self.world_Rotation := y_Rotation_from (0.0);
      Self.GL.define;
   end define;



   procedure destroy (Self : in out Item)
   is
   begin
      Self.GL.destroy;
   end destroy;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;




   --------------
   --  Attributes
   --

   function to_world_Site (Self : in Item'Class;   Site : in math.Vector_3) return math.Vector_3
   is
      use math.Vectors;

      the_perspective_Transform : constant math.Matrix_4x4 := to_Perspective (fovy   => 60.0,
                                                                              aspect => Self.Aspect, -- 1200.0 / 1200.0,
                                                                              zNear  => Self.near_plane_Distance,
                                                                              zFar   => Self.far_plane_Distance);

      Viewport                  : constant Rectangle       := Self.Clipper.main_Clipping;
      Position_window_space     : constant Vector_3        := (Site (1),
                                                               Real (Viewport.Max (2)) - Site (2),
                                                               Site (3));
      Site_world_space          : constant Vector_3        := unProject (Position_window_space,
                                                                         modelMatrix => Self.GL.view_Transform,
                                                                         projMatrix  => the_perspective_Transform,
                                                                         viewport    => Viewport);
   begin
      return Site_world_space;
   end to_world_Site;




   procedure Site_is (Self : in out Item'Class;   Now : in math.Vector_3)
   is
      use math.Vectors;
   begin
      Self.Clipper.eye_position := Now;
      Self.view_Transform       := to_transform_Matrix ((Self.world_Rotation, -Self.Site));

      Self.GL.Site_is (Now);
   end Site_is;



   function  Site (Self : in     Item'Class)    return  math.Vector_3
   is
   begin
      return Self.Clipper.eye_position;
   end Site;



   procedure Position_is (Self : in out Item'Class;   Site : in math.Vector_3;
                                                      Spin : in math.Matrix_3x3)
   is
      use math.Vectors;
   begin
      Self.Clipper.eye_position := Site;
      Self.world_Rotation       := Spin;
      Self.view_Transform       := to_transform_Matrix ((Self.world_Rotation, -Self.Site));

      Self.GL.Position_is (Site, Spin);
   end Position_is;





   procedure world_Rotation_is (Self : in out Item'Class;   Now : in math.Matrix_3x3)
   is
      use math.Vectors;
   begin
      Self.world_Rotation := Now;
      Self.view_Transform := to_transform_Matrix ((Self.world_Rotation, -Self.Site));

      Self.GL.Spin_is (Now);
   end world_Rotation_is;



   function  world_Rotation (Self : in     Item'Class)     return math.Matrix_3x3
   is
   begin
      return Self.world_Rotation;
   end world_Rotation;



   procedure view_Transform_is (Self : in out Item'Class;   Now : in math.Matrix_4x4)
   is
   begin
      Self.view_Transform := Now;
   end view_Transform_is;



   procedure rotation_Speed_is (Self : in out Item'Class;   Now : math.Vector_3)
   is
   begin
      null;
   end rotation_Speed_is;


   function rotation_Speed (Self : in Item'Class) return math.Vector_3
   is
      pragma Unreferenced (Self);
   begin
      return (0.0, 0.0, 0.0);
   end rotation_Speed;




   function  Speed    (Self : in     Item'Class)     return math.Vector_3
   is
      pragma Unreferenced (Self);
   begin
      return (0.0, 0.0, 0.0);
   end Speed;


   procedure Speed_is (Self : in out Item'Class;   Now : in math.Vector_3)
   is
   begin
      null;
   end Speed_is;



   function FOVy   (Self : in Item'Class) return math.Real
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
      Self.GL.near_plane_Distance_is (Now);
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



   function ModelView_Matrix (Self : in Item'Class) return math.Matrix_4x4
   is
      use math.Vectors;

      R : Matrix_3x3 renames Self.world_Rotation;
      S : Vector_3   renames Self.Site;

   begin
      return ((R (1, 1),  R (1, 2),  R (1, 3),  0.0),
              (R (2, 1),  R (2, 2),  R (2, 3),  0.0),
              (R (3, 1),  R (3, 2),  R (3, 3),  0.0),
              (  -S (1),    -S (2),    -S (3),  1.0));
   end ModelView_Matrix;




   --------------
   --  Operations
   --

   procedure set_viewport_Size (Self : in out Item'Class;   width, height : in Integer)
   is
      use math.Functions;

      deg2rad                  : constant := pi / 180.0;

      half_fov_max_rads        : Real;
      Tan_of_half_fov_max_rads : Real;

   begin
      Self.Clipper.main_clipping.Min (1) := 0;
      Self.Clipper.main_clipping.Min (2) := 0;

      Self.Clipper.main_clipping.Max (1) := width - 1;
      Self.Clipper.main_clipping.Max (2) := height - 1;

      if   Width  = 0
        or Height = 0
      then   Self.Aspect := 1.0;
      else   Self.Aspect := Real (Width) / Real (Height);
      end if;

      half_fov_max_rads        := 0.5 * Self.FOVy * deg2rad;
      Tan_of_half_fov_max_rads := Tan (half_fov_max_rads);

      Self.near_plane_Height   := Self.near_plane_Distance * Tan_of_half_fov_max_rads;
      Self.near_plane_Width    := Self.near_plane_Height   * Self.Aspect;

      Self.far_plane_Height    := Self.far_plane_Distance  * Tan_of_half_fov_max_rads;
      Self.far_plane_Width     := Self.far_plane_Height    * Self.Aspect;


      if Self.aspect > 1.0
      then -- X side angle broader than Y side angle.
         half_fov_max_rads := ArcTan (Self.aspect * Tan_of_half_fov_max_rads);
      end if;

      Self.Clipper.max_dot_product := Sin (half_fov_max_rads);
      Self.Projection_Matrix       := to_Perspective (fovy   => Self.FOVy,
                                                      aspect => Self.Aspect,
                                                      zNear  => Self.near_plane_Distance,
                                                      zFar   => Self.far_plane_Distance);
      Self.GL.Viewport_is (Width, Height);
   end set_viewport_Size;



   procedure Renderer_is (Self : in out Item'Class;   Now : in openGL.Renderer.lean.view)
   is
   begin
      Self.   Renderer :=  Now;
      Self.GL.Renderer_is (Now);
   end Renderer_is;



   procedure render (Self : in out Item;   the_World : in     mmi.World.view;
                                           To        : in     openGL.Surface.view)
   is
      use math.Vectors;

      all_Sprites   : mmi.World.sprite_transform_Pairs renames the_World.sprite_Transforms;

      the_Visuals   : openGL.Visual.views (1 .. all_Sprites'Length);
      visuals_Count : Natural := 0;

   begin
      for i in all_Sprites'Range
      loop
         if         not all_Sprites (i).Sprite.is_Destroyed
           and then     all_Sprites (i).Sprite.is_Visible
         then
            visuals_Count := visuals_Count + 1;

            the_Visuals (visuals_Count)            := all_Sprites (i).Sprite.Visual;
            the_Visuals (visuals_Count).Transform_is (all_Sprites (i).Transform);
            the_Visuals (visuals_Count).Scale_is     ((1.0, 1.0, 1.0));

            the_Visuals (visuals_Count).program_Parameters_are (all_Sprites (i).Sprite.program_Parameters);
         end if;
      end loop;

      Self.GL.render (the_Visuals (1 .. visuals_Count));
   end render;



   function cull_Completed (Self : in Item) return Boolean
   is
   begin
      return Self.GL.cull_Completed;
   end cull_Completed;



   procedure disable_Cull   (Self : in out Item)
   is
   begin
      Self.is_Culling := False;
      Self.GL.disable_Cull;
   end disable_Cull;


end mmi.Camera;
