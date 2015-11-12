package openGL.Model.hexagon
--
--  Models a hexagon.
--
is

   type Item is abstract new openGL.Model.item with
      record
         Radius : Real          := 1.0;
         Bounds : openGL.Bounds := null_Bounds;
      end record;

   overriding
   function  Bounds (Self : in Item) return openGL.Bounds;



private

   subtype site_Id is Integer range 1 .. 6;
   type    Sites   is array (site_Id) of openGL.Vector_3;

   function vertex_Sites (Self : in Item'Class) return Sites;

   Normal : constant openGL.Vector_3 := (0.0, 0.0, 1.0);

end openGL.Model.Hexagon;
