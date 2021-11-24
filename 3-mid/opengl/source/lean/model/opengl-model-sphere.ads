package openGL.Model.sphere
--
--  Provides an abstract model of a sphere.
--
is
   type Item is abstract new Model.item with private;
   type View is access all Item'Class;


   default_latitude_Count  : constant := 26;
   default_longitude_Count : constant := 52;


   ---------
   --- Forge
   --

   procedure define (Self : out Item;   Radius : Real);


   --------------
   --- Attributes
   --

   overriding
   function Bounds (Self : in Item) return openGL.Bounds;



private

   type Item is abstract new Model.item with
      record
         Radius     : Real;

         lat_Count  : Positive;
         long_Count : Positive;
      end record;


   Degrees_180 : constant := Pi;
   Degrees_360 : constant := Pi * 2.0;


end openGL.Model.sphere;
