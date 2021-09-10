with
     ada.unchecked_Conversion;

package openGL.Culler.frustum
--
-- Provides a frustrum culler.
--
is
   type Item is new Culler.Item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure define (Self : in out Item);


   --------------
   --- Attributes
   --

   overriding
   procedure add          (Self : in out Item;   the_Visual     : in Visual.view);
   overriding
   procedure rid          (Self : in out Item;   the_Visual     : in Visual.view);

   overriding
   function  object_Count (Self : in     Item) return Natural;

   overriding
   function  cull         (Self : in     Item;   the_Visuals    : in Visual.views;
                                                 camera_Frustum : in openGL.frustum.Plane_array;
                                                 camera_Site    : in Vector_3) return Visual.views;

   function  vanish_point_size_Min    (Self : in     Item'Class)     return Real;
   procedure vanish_point_size_Min_is (Self : in out Item'Class;   Now : in Real);
   --
   -- Visuals whose projected size falls below this minimum will not be displayed.




private

   type Item is new Culler.item with
      record
         countDown   : Natural := 0;
         frame_Count : Natural := 0;

         vanish_point_size_Min : safe_Real;
      end record;

end openGL.Culler.frustum;
