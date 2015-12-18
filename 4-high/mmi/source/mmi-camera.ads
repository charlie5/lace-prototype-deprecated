with
     mmi.World,

     openGL.Surface,
     openGL.Camera,
     openGL.Renderer.lean;


package mmi.Camera
--
-- Models a camera.
--
is
   use Math;


   type Item  is tagged limited private;
   type View  is access all Camera.item'Class;

   type Views is array (Positive range <>) of View;



   ---------
   --  Forge
   --

   procedure define   (Self : in out Item);
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);



   -------------
   -- Core Types
   --

   fairly_Far                  : constant := 100_000.0;
   default_field_of_view_Angle : constant :=     60.0;



   subtype Rectangle is float_math.Algebra.linear.d3.Rectangle;

   type Clipping_data is
      record
         eye_position    : aliased math.Vector_3  := (0.0,  0.0,  5.0);
         view_direction  :         math.Vector_3  := (0.0,  0.0, -1.0);
         max_dot_product :         math.Real      := 0.0;                -- Depends on the field of view.
         main_clipping   :         Rectangle := ((0, 0), (0, 0));
      end record;



   --------------
   --  Attributes
   --

   procedure Renderer_is            (Self : in out Item'Class;   Now  : in openGL.Renderer.lean.view);

   procedure Site_is                (Self : in out Item'Class;   Now  : in math.Vector_3);
   function  Site                   (Self : in     Item'Class)     return  math.Vector_3;

   procedure world_Rotation_is      (Self : in out Item'Class;   Now  : in math.Matrix_3x3);
   function  world_Rotation         (Self : in     Item'Class)      return math.Matrix_3x3;

   procedure Position_is            (Self : in out Item'Class;   Site : in math.Vector_3;
                                                                 Spin : in math.Matrix_3x3);

   procedure view_Transform_is      (Self : in out Item'Class;   Now  : in math.Matrix_4x4);

   procedure rotation_Speed_is      (Self : in out Item'Class;   Now  : in math.Vector_3);
   function  rotation_Speed         (Self : in     Item'Class)   return math.Vector_3;

   function  Speed                  (Self : in     Item'Class)     return math.Vector_3;     -- Linear speed
   procedure Speed_is               (Self : in out Item'Class;   Now : in math.Vector_3);

   function  FOVy                   (Self : in Item'Class) return math.Real;                 -- Field of view angle (deg) in the y direction.

   procedure set_viewport_Size      (Self : in out Item'Class;   Width,
                                                                 Height : in Integer);

   function  Aspect                 (Self : in     Item'Class)     return math.Real;         -- X/Y aspect ratio.
   procedure Aspect_is              (Self : in out Item'Class;   Now : in math.Real);


   function  near_plane_Distance    (Self : in Item'Class) return math.Real;                 -- Distance to the near clipping plane.
   function  far_plane_Distance     (Self : in Item'Class) return math.Real;                 -- Distance to the far  clipping plane.

   procedure far_plane_Distance_is  (Self : in out Item'Class;   Now : in math.Real);
   procedure near_plane_Distance_is (Self : in out Item'Class;   Now : in math.Real);


   function  ModelView_Matrix       (Self : in    Item'Class) return math.Matrix_4x4;

   function  to_world_Site          (Self : in    Item'Class;   Site : in math.Vector_3) return math.Vector_3;
   --
   --  Returns the window space 'Site' transformed to the equivalent world space site.



   --------------
   --  Operations
   --

   procedure render         (Self : in out Item;   the_World : in     mmi.World.view;
                                                   To        : in     openGL.Surface.view);

   function  cull_Completed (Self : in     Item) return Boolean;
   procedure disable_Cull   (Self : in out Item);




private

   type Item is tagged limited
      record
         GL                  : aliased openGL.Camera.item;

         Renderer            :         openGL.Renderer.lean.view;

         Clipper             : aliased Clipping_data;

         world_Rotation      :         math.Matrix_3x3;
         view_Transform      :         math.Matrix_4x4;

         FOVy                :         math.Real := default_field_of_view_Angle;   -- Field of view angle (deg) in the y direction.
         Aspect              :         math.Real := 1.0;                           -- X/Y aspect ratio.

         near_plane_Distance :         math.Real := 0.1;                           -- Distance to the near clipping plane.
         near_plane_Width    :         math.Real;
         near_plane_Height   :         math.Real;

         far_plane_Distance  :         math.Real := fairly_Far;                    -- Distance to the far clipping plane.
         far_plane_Width     :         math.Real;
         far_plane_Height    :         math.Real;

         Projection_Matrix   :         math.Matrix_4x4;

         is_Culling          :         Boolean   := True;
      end record;


end mmi.Camera;
