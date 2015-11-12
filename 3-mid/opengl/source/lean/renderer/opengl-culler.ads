with
     openGL.Renderer.lean,
     openGL.Visual,
     openGL.Frustum;


package openGL.Culler
--
-- Provides a base class for cullers.
--
is

   type Item   is abstract tagged limited private;
   type View is access all Item'Class;


   --------------
   --- Attributes
   --

   procedure add          (Self : in out Item;   the_Visual : in openGL.Visual.view)   is abstract;
   procedure rid          (Self : in out Item;   the_Visual : in openGL.Visual.view)   is abstract;

   function  object_Count (Self : in     Item)           return Natural                is abstract;

   function  Viewer       (Self : in     Item'Class)     return openGL.Renderer.lean.view;
   procedure Viewer_is    (Self : in out Item'Class;   Now : in openGL.Renderer.lean.view);


   --------------
   --  Operations
   --

   function  cull (Self : in Item;   the_Visuals    : in openGL.Visual.views;
                                     camera_Frustum : in openGL.frustum.Plane_array;
                                     camera_Site    : in math.Vector_3) return openGL.Visual.views
                   is abstract;



private

   type Item is abstract tagged limited
      record
         Viewer : openGL.Renderer.lean.view;
      end record;

end openGL.Culler;
