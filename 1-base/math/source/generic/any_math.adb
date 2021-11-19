with
     ada.Characters.latin_1;

package body any_Math
is
   use ada.Containers;

   -----------
   -- Integers
   --

   procedure increment (Self : in out Integer;   By : in Integer := 1)
   is
   begin
      Self := Self + By;
   end increment;


   procedure decrement (Self : in out Integer;   By : in Integer := 1)
   is
   begin
      Self := Self - By;
   end decrement;


   procedure swap (Left, Right : in out Integer)
   is
      Pad : constant Integer := Left;
   begin
      Left  := Right;
      Right := Pad;
   end swap;


   -----------
   -- Counters
   --

   procedure increment (Self : in out Count_type;   By : in Count_type := 1)
   is
   begin
      Self := Self + By;
   end increment;


   procedure decrement (Self : in out Count_type;   By : in Count_type := 1)
   is
   begin
      Self := Self - By;
   end decrement;


   ---------
   --  Reals
   --

   --  Ada 95 Quality and Style Guide, 7.2.7:
   --  Tests for
   --
   --  (1) absolute "equality" to 0 in storage,
   --  (2) absolute "equality" to 0 in computation,
   --  (3) relative "equality" to 0 in storage, and
   --  (4) relative "equality" to 0 in computation:
   --
   --  abs X <= Float_Type'Model_Small                      -- (1)
   --  abs X <= Float_Type'Base'Model_Small                 -- (2)
   --  abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
   --  abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)
   --
   function almost_Zero (Self : Real) return Boolean
   is
   begin
      return  abs Self <= Real'Base'Model_Small;
   end almost_Zero;


   function Clamped (Self : in Real;   Low, High : in Real) return Real
   is
   begin
      return Real'Max (Low,
                       Real'Min (Self, High));
   end Clamped;


   procedure clamp (Self : in out Real;   Low, High : in Real)
   is
   begin
      Self := Clamped (Self, Low, High);
   end clamp;


   procedure swap (Left, Right : in out Real)
   is
      Pad : constant Real := Left;
   begin
      Left  := Right;
      Right := Pad;
   end swap;


   -------------
   -- Percentage
   --

   function to_Percentage (From : in Real) return Percentage
   is
   begin
      return Percentage (From * Real' (100.0));
   end to_Percentage;


   function to_Real (Percent : in Percentage) return Real
   is
   begin
      return Real (Percent / 100.0);
   end to_Real;


   function Image (Percent   : in Percentage;
                   Precision : in Natural := 5) return String
   is
   begin
      return Image (Real (Percent),
                    Precision)
           & "%";
   end Image;


   function apply (Left, Right : in Percentage) return Percentage
   is
   begin
      return Percentage (Real (Left) * Real (Right) / 100.0**2);
   end apply;
   --
   -- Named "apply" (rather than "*") to prevent silently overriding the "*" function of the Real type.


   function apply (Percent : in Percentage;
                   To      : in Real) return Real
   is
   begin
      return to_Real (Percent) * To;
   end apply;
   --
   -- Named "apply" (rather than "*") to prevent ambiguous expressions when numeric literals are used.


   ---------
   -- Angles
   --

   function to_Radians (Self : in Degrees) return Radians
   is
   begin
      return Radians (Self * Pi / 180.0);
   end to_Radians;


   function to_Degrees (Self : in Radians) return Degrees
   is
   begin
      return Degrees (Self) * 180.0 / Pi;
   end to_Degrees;


   ----------
   -- Vectors
   --

   function Sum (Self : in Vector) return Real
   is
      the_Sum : Real := 0.0;
   begin
      for Each in Self'Range
      loop
         the_Sum := the_Sum + Self (Each);
      end loop;

      return the_Sum;
   end Sum;


   function Average (Self : in Vector) return Real
   is
   begin
      return Sum (Self) / Real (Self'Length);
   end Average;


   function Max (Self : in Vector) return Real
   is
      Max : Real := Self (Self'First);
   begin
      for i in Self'First + 1 .. Self'Last
      loop
         Max := Real'Max (Max, Self (i));
      end loop;

      return Max;
   end Max;


   function Min (Self : in Vector) return Real
   is
      Min : Real := Self (Self'First);
   begin
      for i in Self'First + 1 .. Self'Last
      loop
         Min := Real'Min (Min, Self (i));
      end loop;

      return Min;
   end Min;


   -----------
   -- Matrices
   --

   function Row (Self : in Matrix_2x2;   row_Id : in Index) return Vector_2
   is
   begin
      return (Self (row_Id, 1),
              Self (row_Id, 2));
   end Row;


   function Col (Self : in Matrix_2x2;   col_Id : in Index) return Vector_2
   is
   begin
      return (Self (1, col_Id),
              Self (2, col_Id));
   end Col;


   function Row (Self : in Matrix_3x3;   row_Id : in Index) return Vector_3
   is
   begin
      return (Self (row_Id, 1),
              Self (row_Id, 2),
              Self (row_Id, 3));
   end Row;


   function Col (Self : in Matrix_3x3;   col_Id : in Index) return Vector_3
   is
   begin
      return (Self (1, col_Id),
              Self (2, col_Id),
              Self (3, col_Id));
   end Col;


   function Row (Self : in Matrix_4x4;   row_Id : in Index) return Vector_4
   is
   begin
      return (Self (row_Id, 1),
              Self (row_Id, 2),
              Self (row_Id, 3),
              Self (row_Id, 4));
   end Row;


   function Col (Self : in Matrix_4x4;   col_Id : in Index) return Vector_4
   is
   begin
      return (Self (1, col_Id),
              Self (2, col_Id),
              Self (3, col_Id),
              Self (4, col_Id));
   end Col;


   function to_Vector_16 (Self : in Matrix_4x4) return Vector_16
   is
   begin
      return Vector_16 (  Vector_4' (Row (Self, 1))
                        & Vector_4' (Row (Self, 2))
                        & Vector_4' (Row (Self, 3))
                        & Vector_4' (Row (Self, 4)));
   end to_Vector_16;


   function to_Matrix_4x4 (Self : in Vector_16)  return Matrix_4x4
   is
   begin
      return Matrix_4x4' (1 => (Self (1),  Self (2),  Self (3),  Self (4)),
                          2 => (Self (5),  Self (6),  Self (7),  Self (8)),
                          3 => (Self (9),  Self (10), Self (11), Self (12)),
                          4 => (Self (13), Self (14), Self (15), Self (16)));
   end to_Matrix_4x4;


   --------------
   -- Quaternions
   --

   function to_Quaternion (From : in Vector_4) return Quaternion
   is
   begin
      return (From (1),
              (Vector_3 (From (2 .. 4))));
   end to_Quaternion;


   function to_Vector (From : in Quaternion) return Vector_4
   is
   begin
      return Vector_4 (From.R & From.V);
   end to_Vector;


   function "*" (Left : in Quaternion;   Right : in Real) return Quaternion
   is
   begin
      return (Left.R * Right,
              (Left.V * Right));
   end "*";


   function "*" (Left : in Real;   Right : in Quaternion) return Quaternion
   is
   begin
      return (Right.R * Left,
              (Right.V * Left));
   end "*";


   function "/" (Left : in Quaternion;   Right : in Real) return Quaternion
   is
   begin
      return (Left.R / Right,
              (Left.V / Right));
   end "/";


   function "+" (Left, Right : in Quaternion) return Quaternion
   is
   begin
      return (Left.R + Right.R,
              Left.V + Right.V);
   end "+";


   function "-" (Left, Right : in Quaternion) return Quaternion
   is
   begin
      return (Left.R - Right.R,
              Left.V - Right.V);
   end "-";


   function Image (Self : in Quaternion;   Precision : in Natural := 5) return String
   is
   begin
      return "(R => "  & Image (Self.R, Precision)
           & ", V => " & Image (Self.V, Precision)  & ")";
   end Image;


   ---------
   -- Images
   --

   -- Real Image
   --
   function Image (Self : in Real;   Precision : in Natural := 5) return String
   is
      type Fixed_1  is delta  0.1            range -100_000_000_000_000_000.0 .. 100_000_000_000_000_000.0;
      type Fixed_2  is delta  0.01           range  -10_000_000_000_000_000.0 ..  10_000_000_000_000_000.0;
      type Fixed_3  is delta  0.001          range   -1_000_000_000_000_000.0 ..   1_000_000_000_000_000.0;
      type Fixed_4  is delta  0.0001         range     -100_000_000_000_000.0 ..     100_000_000_000_000.0;
      type Fixed_5  is delta  0.00001        range      -10_000_000_000_000.0 ..      10_000_000_000_000.0;
      type Fixed_6  is delta  0.000001       range       -1_000_000_000_000.0 ..       1_000_000_000_000.0;
      type Fixed_7  is delta  0.0000001      range         -100_000_000_000.0 ..         100_000_000_000.0;
      type Fixed_8  is delta  0.00000001     range          -10_000_000_000.0 ..          10_000_000_000.0;
      type Fixed_9  is delta  0.000000001    range           -1_000_000_000.0 ..           1_000_000_000.0;
      type Fixed_10 is delta  0.0000000001   range             -100_000_000.0 ..             100_000_000.0;
      type Fixed_11 is delta  0.00000000001  range              -10_000_000.0 ..              10_000_000.0;
      type Fixed_12 is delta  0.000000000001 range               -1_000_000.0 ..               1_000_000.0;
   begin
      case Precision
      is
         when 0      => return Integer'Image  (Integer  (Self));
         when 1      => return Fixed_1'Image  (Fixed_1  (Self));
         when 2      => return Fixed_2'Image  (Fixed_2  (Self));
         when 3      => return Fixed_3'Image  (Fixed_3  (Self));
         when 4      => return Fixed_4'Image  (Fixed_4  (Self));
         when 5      => return Fixed_5'Image  (Fixed_5  (Self));
         when 6      => return Fixed_6'Image  (Fixed_6  (Self));
         when 7      => return Fixed_7'Image  (Fixed_7  (Self));
         when 8      => return Fixed_8'Image  (Fixed_8  (Self));
         when 9      => return Fixed_9'Image  (Fixed_9  (Self));
         when 10     => return Fixed_10'Image (Fixed_10 (Self));
         when 11     => return Fixed_11'Image (Fixed_11 (Self));
         when 12     => return Fixed_12'Image (Fixed_12 (Self));
         when others => return Fixed_12'Image (Fixed_12 (Self));
      end case;

   exception
      when Constraint_Error =>
         return Real'Image (Self);
   end Image;


   -- Vector Image
   --
   function Image (Self : in Vector;   Precision : in Natural := 5) return String
   is
      the_Image : String (1 .. 1 * 1024 * 1024);   -- Handles one megabyte string, excess is truncated.
      Count     : Standard.Natural := 0;

      procedure add (Text : in String)
      is
      begin
         the_Image (Count + 1 .. Count + text'Length) := Text;
         Count                                        := Count + text'Length;
      end add;

   begin
      add ("(");

      for Each in Self'Range
      loop
         if Each /= Self'First
         then
            add (", ");
         end if;

         add (Image (Self (Each),
                     Precision));
      end loop;

      add (")");
      return the_Image (1 .. Count);

   exception
      when others =>
         return the_Image (1 .. Count);
   end Image;


   -----------
   -- Vector_2
   --

   function to_Vector_2 (Self : in Vector_3) return Vector_2
   is
   begin
      return Vector_2 (Self (1 .. 2));
   end to_Vector_2;


   function Image (Self : in Vector_2;   Precision : in Natural := 5) return String
   is
   begin
      return Image (Vector (Self), Precision);
   end Image;


   overriding
   function "+" (Left, Right : in Vector_2) return Vector_2
   is
   begin
      return (Left (1) + Right (1),
              Left (2) + Right (2));
   end "+";


   overriding
   function "-" (Left, Right : in Vector_2) return Vector_2
   is
   begin
      return (Left (1) - Right (1),
              Left (2) - Right (2));
   end "-";


   overriding
   function "*" (Left : in Real;   Right : in Vector_2) return Vector_2
   is
   begin
      return (Right (1) * Left,
              Right (2) * Left);
   end "*";


   overriding
   function "*" (Left : in Vector_2;   Right : in Real) return Vector_2
   is
   begin
      return (Left (1) * Right,
              Left (2) * Right);
   end "*";


   overriding
   function "/" (Left : in Vector_2;   Right : in Real) return Vector_2
   is
   begin
      return (Left (1) / Right,
              Left (2) / Right);
   end "/";


   -----------
   -- Vector_3
   --

   function to_Vector_3 (Self : in Vector_2;   Z : in Real := 0.0) return Vector_3
   is
   begin
      return Vector_3 (Self & Z);
   end to_Vector_3;


   function Image (Self : in Vector_3;   Precision : in Natural := 5) return String
   is
   begin
      return Image (Vector (Self), Precision);
   end Image;


   overriding
   function "*" (Left : in Real;   Right : in Vector_3) return Vector_3
   is
   begin
      return (Right (1) * Left,
              Right (2) * Left,
              Right (3) * Left);
   end "*";


   function "*" (Left, Right : in Vector_3) return Vector_3
   is
   begin
      return (1 => Left (2) * Right (3)  -  Left (3) * Right (2),
              2 => Left (3) * Right (1)  -  Left (1) * Right (3),
              3 => Left (1) * Right (2)  -  Left (2) * Right (1));
   end "*";


   overriding
   function "+" (Left, Right : in Vector_3) return Vector_3
   is
   begin
      return (Left (1) + Right (1),
              Left (2) + Right (2),
              Left (3) + Right (3));
   end "+";


   overriding
   function "-" (Left, Right : in Vector_3) return Vector_3
   is
   begin
      return (Left (1) - Right (1),
              Left (2) - Right (2),
              Left (3) - Right (3));
   exception
      when Constraint_Error =>
         raise Constraint_Error with "any_math ""-"" (Left, Right : Vector_3) => "
                                    & Image (Left) & "   " & Image (Right);
   end "-";


   overriding
   function "-" (Right : in Vector_3) return Vector_3
   is
   begin
      return (-Right (1),
              -Right (2),
              -Right (3));
   end "-";


   overriding
   function "*" (Left : in Vector_3;   Right : in Real) return Vector_3
   is
   begin
      return (Left (1) * Right,
              Left (2) * Right,
              Left (3) * Right);
   end "*";


   overriding
   function "/" (Left : in Vector_3;   Right : in Real) return Vector_3
   is
   begin
      return (Left (1) / Right,
              Left (2) / Right,
              Left (3) / Right);
   end "/";


   overriding
   function "abs" (Right : in Vector_3) return Vector_3
   is
      use Vectors;
   begin
      return Vector_3 (Vector' (abs (Vector (Right))));
   end "abs";


   ---------
   -- Matrix
   --

   function Image (Self : Matrix) return String
   is
      Image : String (1 .. 1024);
      Last  : Natural := 0;
   begin
      for Row in Self'Range (1)
      loop
         for Col in Self'Range (2)
         loop
            declare
               Element : constant String := Real'Image (Self (Row, Col));
            begin
               Last         := Last + 1;
               Image (Last) := ' ';
               Last         := Last + 1;
               Image (Last .. Last + Element'Length - 1)
                            := Element;
               Last         := Last + Element'Length - 1;
            end;
         end loop;

         Last         := Last + 1;
         Image (Last) := ada.Characters.Latin_1.LF;
      end loop;

      return Image (1 .. Last);
   end Image;


   -------------
   -- Matrix_2x2
   --

   overriding
   function Transpose (Self : in Matrix_2x2) return Matrix_2x2
   is
   begin
      return Matrix_2x2 (Vectors.Transpose (Matrix (Self)));
   end Transpose;


   function "*" (Left : in Matrix_2x2;   Right : in Vector_2) return Vector_2
   is
      Result : Vector_2 := (others => 0.0);
   begin
      for Row in 1 .. 2
      loop
         for Col in 1 .. 2
         loop
            Result (Row) := Result (Row) +   Left (Row, Col)
                                           * Right (Col);
         end loop;
      end loop;

      return Result;
   end "*";


   function "*" (Left : in Vector_2;   Right : in Matrix_2x2) return Vector_2
   is
      use Vectors;
   begin
      return Vector_2 (  Vector (Left)
                       * Matrix (Right));
   end "*";


   -------------
   -- Matrix_3x3
   --

   overriding
   function Transpose (Self : in Matrix_3x3) return Matrix_3x3
   is
   begin
      return Matrix_3x3 (Vectors.Transpose (Matrix (Self)));
   end Transpose;


   function "*" (Left : in Matrix_3x3;   Right : in Vector_3) return Vector_3
   is
      A : Matrix_3x3 renames Left;
      B : Vector_3   renames Right;
   begin
      return ((a(1,1)*b(1) + a(1,2)*b(2) + a(1,3)*b(3)),
              (a(2,1)*b(1) + a(2,2)*b(2) + a(2,3)*b(3)),
              (a(3,1)*b(1) + a(3,2)*b(2) + a(3,3)*b(3)));
   end "*";


   function "*" (Left : in Vector_3;   Right : in Matrix_3x3) return Vector_3
   is
      A : Matrix_3x3 renames Right;
      B : Vector_3   renames Left;
   begin
      return ((a(1,1)*b(1) + a(2,1)*b(2) + a(3,1)*b(3)),
              (a(1,2)*b(1) + a(2,2)*b(2) + a(3,2)*b(3)),
              (a(1,3)*b(1) + a(2,3)*b(2) + a(3,3)*b(3)));
   end "*";


   -------------
   -- Matrix_4x4
   --

   overriding
   function Transpose (Self : in Matrix_4x4) return Matrix_4x4
   is
   begin
      return Matrix_4x4 (Vectors.Transpose (Matrix (Self)));
   end Transpose;


   function "*" (Left : in Matrix_4x4;   Right : in Vector_4) return Vector_4
   is
      A : Matrix_4x4 renames Left;
      B : Vector_4   renames Right;
   begin
      return ((a(1,1)*b(1) + a(1,2)*b(2) + a(1,3)*b(3) + a(1,4)*b(4)),
              (a(2,1)*b(1) + a(2,2)*b(2) + a(2,3)*b(3) + a(2,4)*b(4)),
              (a(3,1)*b(1) + a(3,2)*b(2) + a(3,3)*b(3) + a(3,4)*b(4)),
              (a(4,1)*b(1) + a(4,2)*b(2) + a(4,3)*b(3) + a(4,4)*b(4)));
   end "*";


   function "*" (Left : in Vector_4;   Right : in Matrix_4x4) return Vector_4
   is
      A : Matrix_4x4 renames Right;
      B : Vector_4   renames Left;
   begin
      return ((a(1,1)*b(1) + a(2,1)*b(2) + a(3,1)*b(3) + a(4,1)*b(4)),
              (a(1,2)*b(1) + a(2,2)*b(2) + a(3,2)*b(3) + a(4,2)*b(4)),
              (a(1,3)*b(1) + a(2,3)*b(2) + a(3,3)*b(3) + a(4,3)*b(4)),
              (a(1,4)*b(1) + a(2,4)*b(2) + a(3,4)*b(3) + a(4,4)*b(4)));
   end "*";


   function "*" (Left : in Matrix_4x4;   Right : in Vector_3) return Vector_3
   is
      V : Vector_4 := Vector_4 (Right & 1.0);
   begin
      V := Left * V;
      return Vector_3 (V (1..3));
   end "*";


   function "*" (Left : in Vector_3;   Right : in Matrix_4x4) return Vector_4
   is
      V : Vector_4 := Vector_4 (Left & 1.0);
   begin
      V := V * Right;
      return V;
   end "*";


   function "*" (Left : in Matrix_4x4;   Right : in Vector_3) return Vector_4
   is
      V : Vector_4 := Vector_4 (Right & 1.0);
   begin
      V := Left * V;
      return V;
   end "*";


   overriding
   function "*" (Left : in Matrix_4x4;   Right : in Matrix_4x4) return Matrix_4x4
   is
      A : Matrix_4x4 renames Left;
      B : Matrix_4x4 renames Right;
   begin
      return ((a(1,1)*b(1,1) + a(1,2)*b(2,1) + a(1,3)*b(3,1) + a(1,4)*b(4,1),  a(1,1)*b(1,2) + a(1,2)*b(2,2) + a(1,3)*b(3,2) + a(1,4)*b(4,2),  a(1,1)*b(1,3) + a(1,2)*b(2,3) + a(1,3)*b(3,3) + a(1,4)*b(4,3),  a(1,1)*b(1,4) + a(1,2)*b(2,4) + a(1,3)*b(3,4) + a(1,4)*b(4,4)),
              (a(2,1)*b(1,1) + a(2,2)*b(2,1) + a(2,3)*b(3,1) + a(2,4)*b(4,1),  a(2,1)*b(1,2) + a(2,2)*b(2,2) + a(2,3)*b(3,2) + a(2,4)*b(4,2),  a(2,1)*b(1,3) + a(2,2)*b(2,3) + a(2,3)*b(3,3) + a(2,4)*b(4,3),  a(2,1)*b(1,4) + a(2,2)*b(2,4) + a(2,3)*b(3,4) + a(2,4)*b(4,4)),
              (a(3,1)*b(1,1) + a(3,2)*b(2,1) + a(3,3)*b(3,1) + a(3,4)*b(4,1),  a(3,1)*b(1,2) + a(3,2)*b(2,2) + a(3,3)*b(3,2) + a(3,4)*b(4,2),  a(3,1)*b(1,3) + a(3,2)*b(2,3) + a(3,3)*b(3,3) + a(3,4)*b(4,3),  a(3,1)*b(1,4) + a(3,2)*b(2,4) + a(3,3)*b(3,4) + a(3,4)*b(4,4)),
              (a(4,1)*b(1,1) + a(4,2)*b(2,1) + a(4,3)*b(3,1) + a(4,4)*b(4,1),  a(4,1)*b(1,2) + a(4,2)*b(2,2) + a(4,3)*b(3,2) + a(4,4)*b(4,2),  a(4,1)*b(1,3) + a(4,2)*b(2,3) + a(4,3)*b(3,3) + a(4,4)*b(4,3),  a(4,1)*b(1,4) + a(4,2)*b(2,4) + a(4,3)*b(3,4) + a(4,4)*b(4,4)));
   end "*";

end any_Math;
