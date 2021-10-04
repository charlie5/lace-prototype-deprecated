with
     float_Math,
     ada.Strings.unbounded;

package Collada
--
-- Provides a namespace and core types for the Collada package family.
--
is
   -------
   -- Text
   --

   subtype Text is ada.Strings.unbounded.unbounded_String;

   function to_Text (From : in String) return Text
     renames ada.Strings.unbounded.To_unbounded_String;

   function to_String (From : in Text) return String
     renames ada.Strings.unbounded.To_String;


   type Text_array is array (Positive range <>) of Text;


   -------
   -- Math
   --

   -- Collada matrices use column vectors, so the translation vector is the 4th column.

   package Math renames float_Math;


   subtype Float_array      is math.Vector;
   subtype   Int_array      is math.Integers;

   subtype Vector_3         is math.Vector_3;
   subtype Vector_4         is math.Vector_4;

   subtype Matrix_3x3       is math.Matrix_3x3;
   subtype Matrix_4x4       is math.Matrix_4x4;

   type    Matrix_4x4_array is array (Positive range <>) of Matrix_4x4;


   Identity_4x4 : constant math.Matrix_4x4;

   function matrix_Count (From : in Float_array)                        return Natural;
   function get_Matrix   (From : in Float_array;   Which : in Positive) return Matrix_4x4;


   Error : exception;



private

   Identity_4x4 : constant math.Matrix_4x4 := ((1.0, 0.0, 0.0, 0.0),
                                               (0.0, 1.0, 0.0, 0.0),
                                               (0.0, 0.0, 1.0, 0.0),
                                               (0.0, 0.0, 0.0, 1.0));

end Collada;
