package openGL.Model.sphere
--
--  Provides an abstract model of a sphere.
--
is

   type Item is abstract new Model.item with
      record
         Radius : Real;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure define (Self : out Item;   Radius : Real);


   --------------
   --- Attributes
   --

   overriding
   function Bounds (Self : in Item) return openGL.Bounds;

end openGL.Model.sphere;
