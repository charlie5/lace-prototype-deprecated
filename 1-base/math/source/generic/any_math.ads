with
     ada.Numerics.generic_elementary_Functions,
     ada.Numerics.generic_complex_Types,
     ada.Numerics.generic_real_Arrays,
     ada.Containers;

generic
   type Real_t is digits <>;

package any_Math
--
--  Provides math for any given floating point type.
--
is
   pragma Pure;

   ----------
   -- Indices
   --
   subtype Index   is standard.Integer;
   type    Indices is array (Index range <>) of Index;


   -----------
   -- Counters
   --
   subtype Counter is ada.Containers.Count_Type;

   procedure increment (Self : in out Counter;   By : in Counter := 1);
   procedure decrement (Self : in out Counter;   By : in Counter := 1);


   -----------
   -- Integers
   --
   procedure increment (Self : in out Integer;   By : in Integer := 1);
   procedure decrement (Self : in out Integer;   By : in Integer := 1);

   procedure swap (Left, Right : in out Integer);

   type Integers  is array (Index range <>) of aliased Integer;
   type Naturals  is array (Index range <>) of aliased Natural;
   type Positives is array (Index range <>) of aliased Positive;


   --------
   -- Reals
   --
   subtype Real          is Real_t;
   subtype unit_Interval is Real range 0.0 ..   1.0;

   function  almost_Zero (Self  : in     Real) return Boolean;

   function  Clamped     (Self  : in     Real;   Low, High : in Real) return Real;
   procedure clamp       (Self  : in out Real;   Low, High : in Real);

   procedure swap        (Left,
                          Right : in out Real);

   function  Image       (Self  : in     Real;   Precision : in Natural := 5) return String;


   -------------
   -- Percentage
   --
   type         Percentage is new Real;
   subtype unit_Percentage is Percentage range 0.0 .. 100.0;

   function to_Percentage (From      : in Real)         return Percentage;
   function to_Real       (Percent   : in Percentage)   return Real;
   function Image         (Percent   : in Percentage;
                           Precision : in Natural := 5) return String;
   function apply         (Percent   : in Percentage;
                           To        : in Real)         return Real;
   function apply         (Left,
                           Right     : in Percentage) return Percentage;

   ------------
   -- Functions
   --
   package Functions is new ada.Numerics.generic_elementary_Functions (Real);


   ------------------
   -- Complex Numbers
   --
   package complex_Reals is new ada.Numerics.generic_complex_Types (Real);


   ---------
   -- Angles
   --
   subtype Radians is     Real;
   type    Degrees is new Real;

   function to_Radians (Self : in Degrees) return Radians;
   function to_Degrees (Self : in Radians) return Degrees;


   ----------
   -- Vectors
   --
   package Vectors is new ada.Numerics.generic_real_Arrays (Real'Base);
   subtype Vector  is     Vectors.real_Vector;

   function Sum     (Self : in Vector) return Real;
   function Average (Self : in Vector) return Real;
   function Max     (Self : in Vector) return Real;
   function Min     (Self : in Vector) return Real;

   function Image   (Self : in Vector;   Precision : in Natural := 5) return String;

   type Vector_1  is new Vector (1 ..  1);
   type Vector_2  is new Vector (1 ..  2);
   type Vector_3  is new Vector (1 ..  3);
   type Vector_4  is new Vector (1 ..  4);
   type Vector_8  is new Vector (1 ..  8);
   type Vector_12 is new Vector (1 .. 12);
   type Vector_16 is new Vector (1 .. 16);


   -----------
   -- Vector_2
   --
   function to_Vector_2 (Self : in Vector_3) return Vector_2;
   function Image       (Self : in Vector_2;   Precision : in Natural := 5) return String;

   overriding
   function "+" (Left, Right : in Vector_2) return Vector_2;

   overriding
   function "-" (Left, Right : in Vector_2) return Vector_2;

   overriding
   function "*" (Left : in Real;       Right : in Vector_2) return Vector_2;

   overriding
   function "*" (Left : in Vector_2;   Right : in Real) return Vector_2;

   overriding
   function "/" (Left : in Vector_2;   Right : in Real) return Vector_2;


   -----------
   -- Vector_3
   --
   function to_Vector_3 (Self : in Vector_2;   Z : in Real := 0.0) return Vector_3;
   function Image       (Self : in Vector_3;   Precision : in Natural := 5) return String;

   overriding
   function "*" (Left : in Real;       Right : in Vector_3) return Vector_3;

   overriding
   function "*" (Left : in Vector_3;   Right : in Real)     return Vector_3;

   function "*" (Left, Right : in Vector_3) return Vector_3;   -- Cross product.

   overriding
   function "+" (Left, Right : in Vector_3) return Vector_3;

   overriding
   function "-" (Left, Right : in Vector_3) return Vector_3;

   overriding
   function "-" (Right : in Vector_3)       return Vector_3;

   overriding
   function "/" (Left : in Vector_3;   Right : in Real)     return Vector_3;

   overriding
   function "abs" (Right : in Vector_3) return Vector_3;


   -----------
   -- Matrices
   --

   -- Memory layout is row-major.

   subtype Matrix     is     Vectors.real_Matrix;
   type    Matrix_2x2 is new Matrix (1 .. 2,  1 .. 2);
   type    Matrix_3x3 is new Matrix (1 .. 3,  1 .. 3);
   type    Matrix_4x4 is new Matrix (1 .. 4,  1 .. 4);

   Identity_2x2 : aliased constant Matrix_2x2;
   Identity_3x3 :         constant Matrix_3x3;
   Identity_4x4 :         constant Matrix_4x4;

   function Image (Self : in Matrix) return String;


   -------------
   -- Matrix_2x2
   --
   overriding
   function Transpose (Self : in Matrix_2x2) return Matrix_2x2;

   function "*" (Left : in Vector_2;     Right : in Matrix_2x2) return Vector_2;
   function "*" (Left : in Matrix_2x2;   Right : in Vector_2)   return Vector_2;

   function Row (Self : in Matrix_2x2;   row_Id : in Index)     return Vector_2;
   function Col (Self : in Matrix_2x2;   col_Id : in Index)     return Vector_2;


   -------------
   -- Matrix_3x3
   --
   overriding
   function Transpose (Self : in Matrix_3x3) return Matrix_3x3;

   function "*" (Left : in Vector_3;     Right : in Matrix_3x3) return Vector_3;
   function "*" (Left : in Matrix_3x3;   Right : in Vector_3)   return Vector_3;

   function Row (Self : in Matrix_3x3;   row_Id : in Index)     return Vector_3;
   function Col (Self : in Matrix_3x3;   col_Id : in Index)     return Vector_3;


   -------------
   -- Matrix_4x4
   --
   overriding
   function Transpose (Self : in Matrix_4x4) return Matrix_4x4;

   function "*" (Left : in Vector_4;     Right : in Matrix_4x4)  return Vector_4;
   function "*" (Left : in Matrix_4x4;   Right : in Vector_4)    return Vector_4;

   function "*" (Left : in Matrix_4x4;   Right : in Vector_3)    return Vector_3;

   function "*" (Left : in Vector_3;     Right : in Matrix_4x4)  return Vector_4;
   function "*" (Left : in Matrix_4x4;   Right : in Vector_3)    return Vector_4;

   overriding
   function "*" (Left : in Matrix_4x4;   Right  : in Matrix_4x4) return Matrix_4x4;

   function Row (Self : in Matrix_4x4;   row_Id : in Index)      return Vector_4;
   function Col (Self : in Matrix_4x4;   col_Id : in Index)      return Vector_4;

   function to_Vector_16  (Self : in Matrix_4x4) return Vector_16;
   function to_Matrix_4x4 (Self : in Vector_16)  return Matrix_4x4;


   --------------
   -- Quaternions
   --
   type Quaternion is
      record
         R : Real;       -- Scalar part.
         V : Vector_3;   -- Vector part.
      end record;

   function to_Quaternion (From : in Vector_4)   return Quaternion;
   function to_Vector     (From : in Quaternion) return Vector_4;

   function "*" (Left : in Quaternion;   Right : in Real)       return Quaternion;
   function "*" (Left : in Real;         Right : in Quaternion) return Quaternion;

   function "/" (Left : in Quaternion;   Right : in Real)       return Quaternion;

   function "+" (Left, Right : in Quaternion) return Quaternion;
   function "-" (Left, Right : in Quaternion) return Quaternion;

   function Image (Self : in Quaternion;   Precision : in Natural := 5) return String;


   -------------
   -- Transforms
   --
   type Transform_2d is
      record
         Rotation    : aliased Matrix_2x2;
         Translation : aliased Vector_2;
      end record;

   type Transform_3d is
      record
         Rotation    : aliased Matrix_3x3;
         Translation : aliased Vector_3;
      end record;

   null_Transform_2d : constant Transform_2d;   -- No translation and no rotation.
   null_Transform_3d : constant Transform_3d;   --


   ------------
   -- Constants
   --
   Infinity  : constant Real;
   Pi        : constant := ada.numerics.Pi;
   Phi       : constant := 1.6180339887_4989484820_4586834365_6381177203_0917980576_2862135448_6227052604_6281890244_9707207204_1893911374;
   --
   --  The 'Golden' ratio.

   Origin_2D : constant Vector_2;
   Origin_3D : constant Vector_3;



private
   Infinity     : constant Real := Real'Last;

   Origin_2D    : constant Vector_2 := (0.0, 0.0);
   Origin_3D    : constant Vector_3 := (0.0, 0.0, 0.0);

   Identity_2x2 : aliased constant Matrix_2x2 := ((1.0, 0.0),
                                                  (0.0, 1.0));

   Identity_3x3 : constant Matrix_3x3 := ((1.0, 0.0, 0.0),
                                          (0.0, 1.0, 0.0),
                                          (0.0, 0.0, 1.0));

   Identity_4x4 : constant Matrix_4x4 := ((1.0, 0.0, 0.0, 0.0),
                                          (0.0, 1.0, 0.0, 0.0),
                                          (0.0, 0.0, 1.0, 0.0),
                                          (0.0, 0.0, 0.0, 1.0));

   null_Transform_2d : constant Transform_2d := (translation =>  (0.0, 0.0),
                                                 rotation    => ((1.0, 0.0),
                                                                 (0.0, 1.0)));

   null_Transform_3d : constant Transform_3d := (translation =>  (0.0, 0.0, 0.0),
                                                 rotation    => ((1.0, 0.0, 0.0),
                                                                 (0.0, 1.0, 0.0),
                                                                 (0.0, 0.0, 1.0)));
   pragma Inline_Always (increment);
   pragma Inline_Always (decrement);
   pragma Inline_Always (Clamped);

end any_Math;
