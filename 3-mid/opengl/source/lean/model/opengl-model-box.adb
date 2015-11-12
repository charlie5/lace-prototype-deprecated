package body openGL.Model.box
is

   --------------
   --- Attributes
   --

   procedure set_Bounds (Self : in out Item)
   is
      use openGL.Geometry;

      left_Offset  : constant Real := -0.5 * Real (self.Scale (1));
      right_Offset : constant Real :=  0.5 * Real (self.Scale (1));

      lower_Offset : constant Real := -0.5 * Real (self.Scale (2));
      upper_Offset : constant Real :=  0.5 * Real (self.Scale (2));

      front_Offset : constant Real :=  0.5 * Real (self.Scale (3));
      rear_Offset  : constant Real := -0.5 * Real (self.Scale (3));

      the_Bounds   : openGL.Bounds := null_Bounds;

   begin
      the_Bounds.Box := the_Bounds.Box or Site' ( left_Offset, lower_Offset,  rear_Offset);
      the_Bounds.Box := the_Bounds.Box or Site' (right_Offset, upper_Offset, front_Offset);

      set_Ball_from_Box (the_Bounds);

      Self.Bounds := the_Bounds;
   end set_Bounds;



   overriding
   function  Bounds (Self : in Item) return openGL.Bounds
   is
   begin
      return Self.Bounds;
   end Bounds;



   function vertex_Sites (Self : in Item'Class) return Sites
   is
      pragma Unreferenced (Self);

      use openGL;

      left_Offset  : constant Real := -0.5;
      right_Offset : constant Real :=  0.5;

      lower_Offset : constant Real := -0.5;
      upper_Offset : constant Real :=  0.5;

      front_Offset : constant Real :=  0.5;
      rear_Offset  : constant Real := -0.5;
   begin
      return (Left_Lower_Front  => (left_Offset,   lower_Offset,  front_Offset),
              Right_Lower_Front => (right_Offset,  lower_Offset,  front_Offset),
              Right_Upper_Front => (right_Offset,  upper_Offset,  front_Offset),
              Left_Upper_Front  => (left_Offset,   upper_Offset,  front_Offset),
              Right_Lower_Rear  => (right_Offset,  lower_Offset,  rear_Offset),
              Left_Lower_Rear   => (left_Offset,   lower_Offset,  rear_Offset),
              Left_Upper_Rear   => (left_Offset,   upper_Offset,  rear_Offset),
              Right_Upper_Rear  => (right_Offset,  upper_Offset,  rear_Offset));
   end vertex_Sites;


end openGL.Model.box;
