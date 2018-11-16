package body Impact.d3
is

   --  C-ish types
   --

   function to_C    (From : in math.Vector_3) return c_Vector_3
   is
   begin
      return (From (1),
              From (2),
              From (3));
   end to_C;


   function to_Math (From : in c_Vector_3) return math.Vector_3
   is
   begin
      return (From (0),
              From (1),
              From (2));
   end to_Math;

end Impact.d3;
