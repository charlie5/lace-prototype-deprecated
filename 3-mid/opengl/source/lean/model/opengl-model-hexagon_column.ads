with
     openGL.Model;


package openGL.Model.hexagon_Column
--
--  Models a column with haxegaon sides.
--
is

   type Item is abstract new openGL.Model.item with
      record
         Radius : Real := 1.0;
         Height : Real := 1.0;
--           Bounds : openGL.Bounds := null_Bounds;
      end record;


--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds;




private

   procedure dummy;

   Normal : constant openGL.Vector_3 := (0.0,  0.0,  1.0);

end openGL.Model.hexagon_Column;
