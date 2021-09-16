with
     float_Math,
     c_math_c.Vector_2,
     c_math_c.Vector_3,
     c_math_c.Matrix_3x3,
     c_math_c.Matrix_4x4,
     Interfaces;

package c_math_C.Conversion
--
-- Provide a set of conversion utilities.
--
is
   package Math renames float_Math;

   use Interfaces;

   function "+" (Self : in Integer)                  return C.int;
   function "+" (Self : in C.int)                    return Integer;

   function "+" (Self : in math    .Real)            return c_math_c.Real;
   function "+" (Self : in c_math_c.Real)            return math    .Real;

   function "+" (Self : in math    .Vector_2)        return c_math_c.Vector_2.item;
   function "+" (Self : in c_math_c.Vector_2.item)   return math    .Vector_2;

   function "+" (Self : in math    .Vector_3)        return c_math_c.Vector_3.item;
   function "+" (Self : in c_math_c.Vector_3.item)   return math    .Vector_3;


   function "+" (Self : in math    .Matrix_3x3)      return c_math_c.Matrix_3x3.item;
   function "+" (Self : in c_math_c.Matrix_3x3.item) return math    .Matrix_3x3;


   function "+" (Self : in math    .Matrix_4x4)      return c_math_c.Matrix_4x4.item;
   function "+" (Self : in c_math_c.Matrix_4x4.item) return math    .Matrix_4x4;


   function to_Math (Self : in c_math_c.Matrix_4x4.item) return math.Matrix_4x4
     renames "+";

end c_math_C.Conversion;

