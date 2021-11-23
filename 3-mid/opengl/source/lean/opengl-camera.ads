with
     openGL.Culler.frustum,
     openGL.Impostorer,
     openGL.Frustum,
     openGL.Visual,
     openGL.Surface,
     openGL.Renderer.lean;

package openGL.Camera
--
-- Simulates a camera.
--
is
   type Item is tagged limited private;
   type View is access all Camera.item'Class;


   ---------
   --  Forge
   --

   procedure define  (Self : in out Item);
   procedure destroy (Self : in out Item);


   --------------
   --  Attributes
   --

   fairly_Far                  : constant         := 1_000_000.0;
   default_field_of_view_Angle : constant Degrees :=        60.0;

   procedure Renderer_is              (Self : in out Item;   now    : in Renderer.lean.view);

   procedure Site_is                  (Self : in out Item;         now    : in math.Vector_3);
   function  Site                     (Self : in     Item)              return math.Vector_3;

   procedure Spin_is                  (Self : in out Item'Class;   now    : in math.Matrix_3x3);
   function  Spin                     (Self : in     Item'Class)        return math.Matrix_3x3;

   procedure Position_is              (Self : in out Item'Class;   Site   : in math.Vector_3;
                                                                   Spin   : in math.Matrix_3x3);
   function  World_Transform          (Self : in     Item)              return math.Matrix_4x4;

   function  FoVy                     (Self : in     Item'Class)        return math.Degrees;   -- Field of view angle in the Y direction.
   procedure FoVy_is                  (Self : in out Item'Class;   Now    : in math.Degrees);

   function  Aspect                   (Self : in     Item'Class)        return math.Real;      -- X/Y Aspect ratio.
   procedure Aspect_is                (Self : in out Item'Class;   now    : in math.Real);

   function  near_Plane_Distance      (Self : in     Item'Class)        return math.Real;      -- Distance to the near clipping plane.
   function   far_Plane_Distance      (Self : in     Item'Class)        return math.Real;      -- Distance to the far  clipping plane.

   procedure near_Plane_Distance_is   (Self : in out Item'Class;   now    : in math.Real);
   procedure  far_Plane_Distance_is   (Self : in out Item'Class;   now    : in math.Real);

   function        view_Transform     (Self : in     Item'Class)        return math.Matrix_4x4;
   function  projection_Transform     (Self : in     Item'Class)        return math.Matrix_4x4;

   function  Viewport                 (Self : in     Item)              return linear_Algebra_3d.Rectangle;
   procedure Viewport_is              (Self : in out Item'Class;   Width,
                                                                   Height : in Positive);

   function  to_World_Site            (Self : in     Item;   Window_Site : in math.Vector_3) return math.Vector_3;
   --
   --  Returns the 'window space' site transformed to the equivalent 'world space' site.

   function  cull_completed           (Self : in     Item) return Boolean;
   procedure disable_cull             (Self : in out Item);

   function  vanish_Point_Size_min    (Self : in     Item'Class) return Real;
   procedure vanish_Point_Size_min_is (Self : in out Item'Class;   now : in Real);
   --
   -- Visuals whose projected size falls below this minimum will be culled.

   function  Impostor_Size_min        (Self : in     Item)     return Real;
   procedure Impostor_Size_min_is     (Self : in out Item;   now : in Real);
   --
   -- Visuals whose projected size falls below this minimum will be substituted with impostors.

   procedure allow_Impostors          (Self : in out Item;   now : in Boolean := True);


   --------------
   --  Operations
   --

   procedure render (Self : in out Item;   Visuals : in Visual.views;
                                           to      : in Surface.view := null);

   function  current_Planes (Self : in Item) return Frustum.plane_Array;
   --
   -- Returns the frustum planes calculated from the current GL projection and modelview matrices.



private

   task
   type cull_Engine (Self : access Camera.item'Class)
   is
      entry cull (the_Visuals : in Visual.views;   do_cull : in Boolean);
      entry stop;
   end cull_Engine;


   type Item is tagged limited
      record
         cull_Engine          : camera.cull_Engine (Item'Access);
         cull_Completed       : safe_Boolean := False;
         Culler               : openGL.Culler.frustum.item;

         Impostorer           : openGL.Impostorer.item;
         Impostors_allowed    : Boolean      := False;

         Renderer             : openGL.Renderer.lean.view;

         world_Transform      : math.Matrix_4x4;
         view_Transform       : math.Matrix_4x4;
         projection_Transform : math.Matrix_4x4;

         Viewport             : linear_Algebra_3d.Rectangle;
         FoVy                 : math.Degrees := default_field_of_view_Angle; -- Field of view angle (deg) in the y direction.
         Aspect               : math.Real    := 1.0;                         -- X/Y aspect ratio.

         near_Plane_Distance  : math.Real    := 0.1;                         -- Distance to the near clipping plane.
         near_Plane_Width     : math.Real;
         near_Plane_Height    : math.Real;

         far_Plane_Distance   : math.Real    := fairly_Far;                  -- Distance to the far clipping plane.
         far_Plane_Width      : math.Real;
         far_Plane_Height     : math.Real;

         is_Culling           : Boolean      := True;
      end record;


   procedure update_View_Transform (Self : in out Item);


end openGL.Camera;
