with
     openGL.Model;


package openGL.Model.box
--
--  Models a box.
--
--  Each side may be separately colored via each of its 4 vertices.
--
is

   type Item is abstract new openGL.Model.item with private;

   type Side is (Front, Rear,
                 Upper, Lower,
                 Left,  Right);


private

   type Item is abstract new openGL.Model.item with
      record
         Size : Vector_3;
      end record;


   type site_Id is ( Left_Lower_Front, Right_Lower_Front,
                    Right_Upper_Front,  Left_Upper_Front,
                    Right_Lower_Rear,   Left_Lower_Rear,
                     Left_Upper_Rear,  Right_Upper_Rear);

   type Sites is array (site_Id) of openGL.Vector_3;


   front_Normal : constant openGL.Vector_3 := ( 0.0,  0.0,  1.0);
   rear_Normal  : constant openGL.Vector_3 := ( 0.0,  0.0, -1.0);
   upper_Normal : constant openGL.Vector_3 := ( 0.0,  1.0,  0.0);
   lower_Normal : constant openGL.Vector_3 := ( 0.0, -1.0,  0.0);
   left_Normal  : constant openGL.Vector_3 := (-1.0,  0.0,  0.0);
   right_Normal : constant openGL.Vector_3 := ( 1.0,  0.0,  0.0);


   function vertex_Sites (Self : in Item'Class) return Sites;


end openGL.Model.box;
