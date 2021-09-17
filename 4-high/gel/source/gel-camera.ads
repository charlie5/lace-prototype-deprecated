with
     gel.World,

     openGL.Surface,
     openGL.Camera,
     openGL.Renderer.lean;

package gel.Camera
--
-- Models a camera.
--
is
   type Item  is tagged limited private;
   type View  is access all Camera.item'Class;

   type Views is array (Positive range <>) of View;

   use Math;


   ---------
   --  Forge
   --

   procedure define  (Self : in out Item);
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   -------------
   -- Core Types
   --

   fairly_Far                  : constant         := 100_000.0;
   default_field_of_view_Angle : constant Degrees := 60.0;


   subtype Rectangle is linear_Algebra_3D.Rectangle;

   type Clipping_data is
      record
         eye_Position    : aliased Vector_3  := (0.0,  0.0,  5.0);
         view_Direction  :         Vector_3  := (0.0,  0.0, -1.0);
         max_dot_Product :         Real      := 0.0;                   -- Depends on the field of view.
         main_Clipping   :         Rectangle := ((0, 0), (0, 0));
      end record;


   --------------
   --  Attributes
   --

   procedure Renderer_is            (Self : in out Item'Class;   Now : in openGL.Renderer.lean.view);

   procedure Site_is                (Self : in out Item'Class;   Now : in Vector_3);
   function  Site                   (Self : in     Item'Class)     return Vector_3;

   procedure world_Rotation_is      (Self : in out Item'Class;   Now : in Matrix_3x3);
   function  world_Rotation         (Self : in     Item'Class)     return Matrix_3x3;

   procedure Position_is            (Self : in out Item'Class;   Site : in Vector_3;
                                                                 Spin : in Matrix_3x3);

   procedure view_Transform_is      (Self : in out Item'Class;   Now : in Matrix_4x4);

   procedure rotation_Speed_is      (Self : in out Item'Class;   Now : in Vector_3);
   function  rotation_Speed         (Self : in     Item'Class)     return Vector_3;

   function  Speed                  (Self : in     Item'Class)     return Vector_3;        -- Linear speed
   procedure Speed_is               (Self : in out Item'Class;   Now : in Vector_3);

   function  FoVy                   (Self : in     Item'Class) return Degrees;             -- Field of view angle in the y direction.

   procedure set_viewport_Size      (Self : in out Item'Class;   Width,
                                                                 Height : in Integer);

   function  Aspect                 (Self : in     Item'Class)     return Real;             -- X/Y aspect ratio.
   procedure Aspect_is              (Self : in out Item'Class;   Now : in Real);

   function  near_plane_Distance    (Self : in     Item'Class) return Real;                 -- Distance to the near clipping plane.
   function  far_plane_Distance     (Self : in     Item'Class) return Real;                 -- Distance to the far  clipping plane.

   procedure far_plane_Distance_is  (Self : in out Item'Class;   Now : in Real);
   procedure near_plane_Distance_is (Self : in out Item'Class;   Now : in Real);

   function  ModelView_Matrix       (Self : in     Item'Class) return Matrix_4x4;

   function  to_world_Site          (Self : in     Item'Class;   Site : in Vector_3) return Vector_3;
   --
   --  Returns the window space 'Site' transformed to the equivalent world space site.


   --------------
   --  Operations
   --

   procedure render         (Self : in out Item;   the_World : in gel.World.view;
                                                   To        : in openGL.Surface.view);

   function  cull_Completed (Self : in     Item) return Boolean;
   procedure disable_Cull   (Self : in out Item);



private

   type Item is tagged limited
      record
         GL                  : aliased openGL.Camera.item;
         Renderer            :         openGL.Renderer.lean.view;

         Clipper             : aliased Clipping_data;

         world_Rotation      :         Matrix_3x3;
         view_Transform      :         Matrix_4x4;

         FoVy                :         Degrees := default_field_of_view_Angle;
         Aspect              :         Real    := 1.0;

         near_plane_Distance :         Real := 0.1;
         near_plane_Width    :         Real;
         near_plane_Height   :         Real;

         far_plane_Distance  :         Real := fairly_Far;
         far_plane_Width     :         Real;
         far_plane_Height    :         Real;

         Projection_Matrix   :         Matrix_4x4;

         is_Culling          :         Boolean := True;
      end record;


end gel.Camera;
