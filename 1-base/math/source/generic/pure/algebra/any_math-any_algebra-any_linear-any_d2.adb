package body any_math.any_Algebra.any_linear.any_d2
is


   function to_translation_Transform (Translation : Vector_2) return Matrix_3x3
   is
   begin
      return ((            1.0,             0.0,  0.0),
              (            0.0,             1.0,  0.0),
              (Translation (1), Translation (2),  1.0));
   end to_translation_Transform;



   function to_rotation_Transform (Angle : in Radians ) return Matrix_3x3
   is
      use Functions;
   begin
      return (( cos (Angle),  sin (Angle),  0.0),
              (-sin (Angle),  cos (Angle),  0.0),
              (         0.0,          0.0,  1.0));
   end to_rotation_Transform;



   function to_rotation_Matrix (Angle : in Radians ) return Matrix_2x2
   is
      use Functions;
   begin
      return (( cos (Angle),  sin (Angle)),
              (-sin (Angle),  cos (Angle)));
   end to_rotation_Matrix;



   function to_Transform_2d (Rotation    : in Radians;
                             Translation : in Vector_2) return Transform_2d
   is
   begin
      return (to_rotation_Matrix (Rotation),
              Translation);
   end to_Transform_2d;



   function angle_Between_preNorm (U : in Vector_2;   V : in Vector_2) return Real
   is
      use Functions, Vectors;
      Val : Real := U * V;   -- Dot product.
   begin
      if    val < -1.0 then   val := -1.0;   -- Clamp to avoid rounding errors; acos will
      elsif val >  1.0 then   val :=  1.0;   -- fail with values outside this range.
      end if;

      return arcCos (Val);
   end angle_Between_preNorm;



   function Midpoint (Self, Other : in Vector_2) return Vector_2
   is
   begin
      return ((Self (1) + Other (1)) * 0.5,
              (Self (2) + Other (2)) * 0.5);
   end Midpoint;



   function Distance (Self : in Vector_2;   To : in Vector_2) return Real
   is
      Pad : constant Vector_2 := Self - To;
   begin
      return abs (Pad);
   end Distance;




   function Interpolated (v0, v1 : in Vector_2;
                          rt     : in Real)  return Vector_2
   is
      s : constant Real := 1.0 - rt;
   begin
      return (s * v0 (1) + rt * v1 (1),
              s * v0 (2) + rt * v1 (2));
   end Interpolated;




   --------------
   --- Matrix_2x2
   --

   function to_Matrix (Row_1, Row_2 : in Vector_2) return Matrix_2x2
   is
   begin
      return ((Row_1 (1), Row_1 (2)),
              (Row_2 (1), Row_2 (2)));
   end to_Matrix;





   function up_Direction (Self : in Matrix_2x2) return Vector_2
   is
   begin
      return Normalised (Row (Self, 2));
   end up_Direction;



   function right_Direction (Self : in Matrix_2x2) return Vector_2
   is
   begin
      return Normalised (Row (Self, 1));
   end right_Direction;



   function    to_rotation_Transform (Rotation    : in Matrix_2x2) return Matrix_3x3
   is
   begin
      return ((Rotation (1, 1), Rotation (1, 2),  0.0),
              (Rotation (2, 1), Rotation (2, 2),  0.0),
              (            0.0,             0.0,  1.0));
   end to_rotation_Transform;



   -------------
   --  Transform
   --

   function to_Transform_2d (From : in Matrix_3x3) return Transform_2d
   is
   begin
      return (rotation    => get_Rotation    (From),
              translation => get_Translation (From));
   end to_Transform_2d;




   function to_Transform    (From : in Transform_2d) return Matrix_3x3
   is
   begin
      return to_rotation_Transform (From.Rotation) * to_translation_Transform (From.Translation);
   end to_Transform;



   function "*" (Left : in Vector_2;   Right : in Transform_2d) return Vector_2
   is
      Pad    : constant Vector_3 := (Left (1),  Left (2),  1.0);
      Result : constant Vector_3 := Pad * to_Transform (Right);
   begin
      return Vector_2 (Result (1 .. 2));
   end "*";



   function "*" (Left : in Vector_2;   Right : in Matrix_3x3) return Vector_2
   is
      use Vectors;

      Result : constant Vector := Vector (Left & 1.0) * Matrix (Right);
   begin
      return Vector_2 (Result (1 .. 2));
   end "*";



   function Invert (Self : in Transform_2d) return Transform_2d
   is
      inv : constant Matrix_2x2 := Transpose (Self.Rotation);
   begin
      return (translation => inv * (-Self.Translation),
              rotation    => inv);
   end Invert;



   function invXform (Self : in Transform_2d;   inVec : Vector_2) return Vector_2
   is
      v : constant Vector_2 := inVec - Self.Translation;
   begin
      return Transpose (Self.Rotation) * v;
   end invXform;




   --------------
   --- Matrix 3x3
   --

   function to_scale_Transform (Scale : in Vector_2) return Matrix_3x3
   is
   begin
      return ((Scale (1),        0.0,    0.0),
              (      0.0,  Scale (2),    0.0),
              (      0.0,        0.0,    1.0));
   end to_scale_Transform;



   function to_Transform (Rotation    : in Matrix_2x2;
                          Translation : in Vector_2)   return Matrix_3x3
   is
   begin
      return ((Rotation (1, 1),  Rotation (1, 2),   0.0),
              (Rotation (2, 1),  Rotation (2, 2),   0.0),
              (Translation (1),  Translation (2),   1.0));
   end to_Transform;




   --------------
   --- Transforms
   --

   function get_Rotation (Self : in     Matrix_3x3)    return Matrix_2x2
   is
   begin
      return ((Self (1, 1),  Self (1, 2)),
              (Self (2, 1),  Self (2, 2)));
   end get_Rotation;



   procedure set_Rotation (Self : in out Matrix_3x3;   To : in Matrix_2x2)
   is
   begin
      Self (1, 1) := To (1, 1);
      Self (1, 2) := To (1, 2);
      Self (1, 3) := 0.0;

      Self (2, 1) := To (2, 1);
      Self (2, 2) := To (2, 2);
      Self (2, 3) := 0.0;
   end set_Rotation;



   function get_Translation (Self : in     Matrix_3x3)    return Vector_2
   is
   begin
      return (Self (3, 1),
              Self (3, 2));
   end get_Translation;



   procedure set_Translation (Self : in out Matrix_3x3;   To : in Vector_2)
   is
   begin
      Self (3, 1) := To (1);
      Self (3, 2) := To (2);
      Self (3, 3) := 1.0;
   end set_Translation;


end any_math.any_Algebra.any_linear.any_d2;
