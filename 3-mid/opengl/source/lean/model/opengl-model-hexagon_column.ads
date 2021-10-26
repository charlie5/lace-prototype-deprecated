package openGL.Model.hexagon_Column
--
--  Models a column with six sides.
--
is

   type Item is abstract new Model.item with private;



private

   type Item is abstract new Model.item with
      record
         Radius : Real := 1.0;
         Height : Real := 1.0;
      end record;

   Normal : constant Vector_3 := (0.0, 0.0, 1.0);

end openGL.Model.hexagon_Column;
