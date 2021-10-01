package body openGL.Model.box
is
   --------------
   --- Attributes
   --

   function vertex_Sites (Self : in Item'Class) return Sites
   is
      left_Offset  : constant Real := -0.5;
      right_Offset : constant Real :=  0.5;

      lower_Offset : constant Real := -0.5;
      upper_Offset : constant Real :=  0.5;

      front_Offset : constant Real :=  0.5;
      rear_Offset  : constant Real := -0.5;
   begin
      return (Left_Lower_Front  => Scaled (( left_Offset, lower_Offset, front_Offset),   by => Self.Size),
              Right_Lower_Front => Scaled ((right_Offset, lower_Offset, front_Offset),   by => Self.Size),
              Right_Upper_Front => Scaled ((right_Offset, upper_Offset, front_Offset),   by => Self.Size),
              Left_Upper_Front  => Scaled (( left_Offset, upper_Offset, front_Offset),   by => Self.Size),
              Right_Lower_Rear  => Scaled ((right_Offset, lower_Offset,  rear_Offset),   by => Self.Size),
              Left_Lower_Rear   => Scaled (( left_Offset, lower_Offset,  rear_Offset),   by => Self.Size),
              Left_Upper_Rear   => Scaled (( left_Offset, upper_Offset,  rear_Offset),   by => Self.Size),
              Right_Upper_Rear  => Scaled ((right_Offset, upper_Offset,  rear_Offset),   by => Self.Size));
   end vertex_Sites;



   function Size (Self : in Item) return Vector_3
   is
   begin
      return Self.Size;
   end Size;


end openGL.Model.box;
