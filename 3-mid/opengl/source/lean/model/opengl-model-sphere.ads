package openGL.Model.sphere
--
--  Models a sphere.
--
is

   Radius : constant := 0.5;


   type Item is abstract new openGL.Model.item with
      record
         null;
      end record;


   overriding
   function  Bounds (Self : in Item) return openGL.Bounds;

end openGL.Model.sphere;
