package openGL.Model.box
--
--  Provides an abstract model of a box.
--
is
   type Item is abstract new Model.item with private;

   type Side is (Front, Rear,
                 Upper, Lower,
                 Left,  Right);

   function Size (Self : in Item) return Vector_3;



private

   type Item is abstract new Model.item with
      record
         Size : Vector_3;
      end record;


   type site_Id is ( Left_Lower_Front, Right_Lower_Front,
                    Right_Upper_Front,  Left_Upper_Front,
                    Right_Lower_Rear,   Left_Lower_Rear,
                     Left_Upper_Rear,  Right_Upper_Rear);

   type Sites is array (site_Id) of Site;


   front_Normal : constant Vector_3 := ( 0.0,  0.0,  1.0);
   rear_Normal  : constant Vector_3 := ( 0.0,  0.0, -1.0);
   upper_Normal : constant Vector_3 := ( 0.0,  1.0,  0.0);
   lower_Normal : constant Vector_3 := ( 0.0, -1.0,  0.0);
   left_Normal  : constant Vector_3 := (-1.0,  0.0,  0.0);
   right_Normal : constant Vector_3 := ( 1.0,  0.0,  0.0);

   function vertex_Sites (Self : in Item'Class) return Sites;

end openGL.Model.box;
