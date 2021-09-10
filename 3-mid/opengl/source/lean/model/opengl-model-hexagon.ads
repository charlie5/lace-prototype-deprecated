package openGL.Model.hexagon
--
--  Provides an abstract model of a hexagon.
--
is
   type Item is abstract new openGL.Model.item with     -- TODO: Make private.
      record
         Radius : Real := 1.0;
      end record;


   subtype site_Id is Integer range 1 .. 6;
   type    Sites   is array (site_Id) of Vector_3;

   function vertex_Sites (Radius : in Real) return Sites;



private

   Normal : constant Vector_3 := (0.0, 0.0, 1.0);

end openGL.Model.Hexagon;
