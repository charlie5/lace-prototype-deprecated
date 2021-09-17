with
     openGL.Visual,
     ada.unchecked_Deallocation;

package body gel.Camera
is
   use linear_Algebra_3D;

   --------
   -- Forge
   --

   procedure define (Self : in out Item)
   is
   begin
      Self.view_Transform := Look_at (Eye    => Self.Clipper.eye_Position,
                                      Center => (Self.Clipper.eye_Position (1),
                                                 Self.Clipper.eye_Position (2),
                                                 -100.0),
                                      Up     => (0.0, 1.0, 0.0));

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

   function to_world_Site (Self : in Item'Class;   Site : in Vector_3) return Vector_3
   is
      the_perspective_Transform : constant Matrix_4x4 := to_Perspective (FoVy   => 60.0,
                                                                         Aspect => Self.Aspect,
                                                                         zNear  => Self.near_plane_Distance,
                                                                         zFar   => Self. far_plane_Distance);

      Viewport              : constant Rectangle := Self.Clipper.main_Clipping;
      Position_window_space : constant Vector_3  := (Site (1),
                                                     Real (Viewport.Max (2)) - Site (2),
                                                     Site (3));
      Site_world_space      : constant Vector_3  := unProject (Position_window_space,
                                                               Model      => Self.GL.view_Transform,
                                                               Projection => the_perspective_Transform,
                                                               Viewport   => Viewport);
   begin
      return Site_world_space;
   end to_world_Site;



   procedure Site_is (Self : in out Item'Class;   Now : in Vector_3)
   is
   begin
      Self.Clipper.eye_Position := Now;
      Self.view_Transform       := to_transform_Matrix ((Self.world_Rotation, -Self.Site));

      Self.GL.Site_is (Now);
   end Site_is;



   function Site (Self : in Item'Class) return Vector_3
   is
   begin
      return Self.Clipper.eye_Position;
   end Site;



   procedure Position_is (Self : in out Item'Class;   Site : in Vector_3;
                                                      Spin : in Matrix_3x3)
   is
   begin
      Self.Clipper.eye_position := Site;
      Self.world_Rotation       := Spin;
      Self.view_Transform       := to_transform_Matrix ((Self.world_Rotation,
                                                        -Self.Site));
      Self.GL.Position_is (Site, Spin);
   end Position_is;



   procedure world_Rotation_is (Self : in out Item'Class;   Now : in Matrix_3x3)
   is
   begin
      Self.world_Rotation := Now;
      Self.view_Transform := to_transform_Matrix ((Self.world_Rotation,
                                                  -Self.Site));
      Self.GL.Spin_is (Now);
   end world_Rotation_is;



   function world_Rotation (Self : in Item'Class) return Matrix_3x3
   is
   begin
      return Self.world_Rotation;
   end world_Rotation;



   procedure view_Transform_is (Self : in out Item'Class;   Now : in Matrix_4x4)
   is
   begin
      Self.view_Transform := Now;
   end view_Transform_is;



   procedure rotation_Speed_is (Self : in out Item'Class;   Now : Vector_3)
   is
   begin
      null;     -- TODO
   end rotation_Speed_is;



   function rotation_Speed (Self : in Item'Class) return Vector_3
   is
      pragma unreferenced (Self);
   begin
      return (0.0, 0.0, 0.0);     -- TODO
   end rotation_Speed;



   function Speed (Self : in Item'Class) return Vector_3
   is
      pragma Unreferenced (Self);
   begin
      return (0.0, 0.0, 0.0);     -- TODO
   end Speed;



   procedure Speed_is (Self : in out Item'Class;   Now : in Vector_3)
   is
   begin
      null;     -- TODO
   end Speed_is;



   function FoVy (Self : in Item'Class) return Real
   is
   begin
      return Self.FOVy;
   end FOVy;



   function Aspect (Self : in Item'Class) return Real
   is
   begin
      return Self.Aspect;
   end Aspect;



   procedure Aspect_is (Self : in out Item'Class;   Now : in Real)
   is
   begin
      Self.Aspect := Now;
   end Aspect_is;



   function near_plane_Distance (Self : in Item'Class) return Real
   is
   begin
      return Self.near_plane_Distance;
   end near_plane_Distance;



   function far_plane_Distance (Self : in Item'Class) return Real
   is
   begin
      return Self.far_plane_Distance;
   end far_plane_Distance;



   procedure far_plane_Distance_is (Self : in out Item'Class;   Now : in Real)
   is
   begin
      Self.far_plane_Distance := Now;
   end far_plane_Distance_is;



   procedure near_plane_Distance_is (Self : in out Item'Class;   Now : in Real)
   is
   begin
      Self.near_plane_Distance := Now;
      Self.GL.near_plane_Distance_is (Now);
   end near_plane_Distance_is;



   function ModelView_Matrix (Self : in Item'Class) return Matrix_4x4
   is
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

   procedure set_viewport_Size (Self : in out Item'Class;   Width, Height : in Integer)
   is
      use math.Functions;

      deg2rad                  : constant := pi / 180.0;

      half_FoV_max_Rads        : Real;
      Tan_of_half_FoV_max_Rads : Real;

   begin
      Self.Clipper.main_Clipping.Min (1) := 0;
      Self.Clipper.main_Clipping.Min (2) := 0;

      Self.Clipper.main_Clipping.Max (1) := width  - 1;
      Self.Clipper.main_Clipping.Max (2) := height - 1;

      if   Width  = 0
        or Height = 0
      then   Self.Aspect := 1.0;
      else   Self.Aspect := Real (Width) / Real (Height);
      end if;

      half_FoV_max_Rads        :=  0.5 * Self.FoVy * deg2rad;
      Tan_of_half_FoV_max_Rads := Tan (half_FoV_max_Rads);

      Self.near_plane_Height   := Self.near_plane_Distance * Tan_of_half_FoV_max_Rads;
      Self.near_plane_Width    := Self.near_plane_Height   * Self.Aspect;

      Self.far_plane_Height    := Self.far_plane_Distance  * Tan_of_half_FoV_max_Rads;
      Self.far_plane_Width     := Self.far_plane_Height    * Self.Aspect;


      if Self.aspect > 1.0
      then -- X side angle is broader than Y side angle.
         half_FoV_max_Rads := arcTan (Self.Aspect * Tan_of_half_FoV_max_Rads);
      end if;

      Self.Clipper.max_dot_product := Sin (half_FoV_max_Rads);
      Self.projection_Matrix       := to_Perspective (FoVy   => Self.FoVy,
                                                      Aspect => Self.Aspect,
                                                      zNear  => Self.near_plane_Distance,
                                                      zFar   => Self. far_plane_Distance);
      Self.GL.Viewport_is (Width, Height);
   end set_viewport_Size;



   procedure Renderer_is (Self : in out Item'Class;   Now : in openGL.Renderer.lean.view)
   is
   begin
      Self.   Renderer :=  Now;
      Self.GL.Renderer_is (Now);
   end Renderer_is;



   procedure render (Self : in out Item;   the_World : in gel.World.view;
                                           To        : in openGL.Surface.view)
   is
      all_Sprites : gel.World.sprite_transform_Pairs renames the_World.sprite_Transforms;

      the_Visuals : openGL.Visual.views (1 .. all_Sprites'Length);
      Count       : Natural := 0;

   begin
      for i in all_Sprites'Range
      loop
         if         not all_Sprites (i).Sprite.is_Destroyed
           and then     all_Sprites (i).Sprite.is_Visible
         then
            Count := Count + 1;

            the_Visuals (Count)            := all_Sprites (i).Sprite.Visual;
            the_Visuals (Count).Transform_is (all_Sprites (i).Transform);
            the_Visuals (Count).Scale_is     ((1.0, 1.0, 1.0));

            the_Visuals (Count).program_Parameters_are (all_Sprites (i).Sprite.program_Parameters);
         end if;
      end loop;

      Self.GL.render (the_Visuals (1 .. Count));
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


end gel.Camera;
