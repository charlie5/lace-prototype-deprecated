package body any_Math.any_Algebra.any_linear.any_d2
is

   -----------
   -- Vector_2
   --

   function Angle_between_pre_Norm (U, V : in Vector_2) return Radians
   is
      use Functions, Vectors;
      Val : Real := U * V;   -- Dot product.
   begin
      if    val < -1.0 then   val := -1.0;   -- Clamp to avoid rounding errors. arcCos will
      elsif val >  1.0 then   val :=  1.0;   -- fail with values outside this range.
      end if;

      return arcCos (Val);
   end Angle_between_pre_Norm;



   function Midpoint (From, To : in Vector_2) return Vector_2
   is
   begin
      return ((From (1) + To (1)) * 0.5,
              (From (2) + To (2)) * 0.5);
   end Midpoint;



   function Distance (From, To : in Vector_2) return Real
   is
   begin
      return abs (From - To);
   end Distance;



   function Interpolated (From, To : in Vector_2;
                          Percent  : in unit_Percentage) return Vector_2
   is
      P : constant Real := to_Real (Percent);
      S : constant Real := 1.0 - P;
   begin
      return (S * From (1) + P * To (1),
              S * From (2) + P * To (2));
   end Interpolated;



   -------------
   -- Matrix_2x2
   --

   function to_Matrix (Row_1, Row_2 : in Vector_2) return Matrix_2x2
   is
   begin
      return ((Row_1 (1), Row_1 (2)),
              (Row_2 (1), Row_2 (2)));
   end to_Matrix;



   function to_rotation_Matrix (Angle : in Radians ) return Matrix_2x2
   is
      use Functions;
   begin
      return (( cos (Angle),  sin (Angle)),
              (-sin (Angle),  cos (Angle)));
   end to_rotation_Matrix;



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



   function to_rotation_Transform (Rotation : in Matrix_2x2) return Matrix_3x3
   is
   begin
      return ((Rotation (1, 1), Rotation (1, 2),  0.0),
              (Rotation (2, 1), Rotation (2, 2),  0.0),
              (            0.0,             0.0,  1.0));
   end to_rotation_Transform;



   -------------
   -- Transform
   --

   function to_Transform_2d (From : in Matrix_3x3) return Transform_2d
   is
   begin
      return (Rotation    => get_Rotation    (From),
              Translation => get_Translation (From));
   end to_Transform_2d;



   function to_Transform (From : in Transform_2d) return Matrix_3x3
   is
   begin
      return to_rotation_Transform (From.Rotation) * to_translation_Transform (From.Translation);
   end to_Transform;



   function to_translation_Transform (Translation : Vector_2) return Matrix_3x3
   is
   begin
      return ((            1.0,             0.0,  0.0),
              (            0.0,             1.0,  0.0),
              (Translation (1), Translation (2),  1.0));
   end to_translation_Transform;



   function to_rotation_Transform (Angle : in Radians) return Matrix_3x3
   is
      use Functions;
   begin
      return (( cos (Angle),  sin (Angle),  0.0),
              (-sin (Angle),  cos (Angle),  0.0),
              (         0.0,          0.0,  1.0));
   end to_rotation_Transform;



   function to_scale_Transform (Scale : in Vector_2) return Matrix_3x3
   is
   begin
      return ((Scale (1),        0.0,    0.0),
              (      0.0,  Scale (2),    0.0),
              (      0.0,        0.0,    1.0));
   end to_scale_Transform;



   function to_Transform (Rotation    : in Matrix_2x2;
                          Translation : in Vector_2) return Matrix_3x3
   is
   begin
      return ((Rotation (1, 1),  Rotation (1, 2),   0.0),
              (Rotation (2, 1),  Rotation (2, 2),   0.0),
              (Translation (1),  Translation (2),   1.0));
   end to_Transform;



   function to_Transform_2d (Rotation    : in Radians;
                             Translation : in Vector_2) return Transform_2d
   is
   begin
      return (to_rotation_Matrix (Rotation),
              Translation);
   end to_Transform_2d;



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



   function Invert (Transform : in Transform_2d) return Transform_2d
   is
      inverse_Rotation : constant Matrix_2x2 := Transpose (Transform.Rotation);
   begin
      return (Translation => inverse_Rotation * (-Transform.Translation),
              Rotation    => inverse_Rotation);
   end Invert;



   function inverse_Transform (Transform : in Transform_2d;   Vector : in Vector_2) return Vector_2
   is
      V : constant Vector_2 := Vector - Transform.Translation;
   begin
      return Transpose (Transform.Rotation) * V;
   end inverse_Transform;



   function get_Rotation (Transform : in Matrix_3x3) return Matrix_2x2
   is
   begin
      return ((Transform (1, 1),  Transform (1, 2)),
              (Transform (2, 1),  Transform (2, 2)));
   end get_Rotation;



   procedure set_Rotation (Transform : in out Matrix_3x3;   To : in Matrix_2x2)
   is
   begin
      Transform (1, 1) := To (1, 1);
      Transform (1, 2) := To (1, 2);
      Transform (1, 3) := 0.0;

      Transform (2, 1) := To (2, 1);
      Transform (2, 2) := To (2, 2);
      Transform (2, 3) := 0.0;
   end set_Rotation;



   function get_Translation (Transform : in Matrix_3x3) return Vector_2
   is
   begin
      return (Transform (3, 1),
              Transform (3, 2));
   end get_Translation;



   procedure set_Translation (Transform : in out Matrix_3x3;   To : in Vector_2)
   is
   begin
      Transform (3, 1) := To (1);
      Transform (3, 2) := To (2);
      Transform (3, 3) := 1.0;
   end set_Translation;


end any_Math.any_Algebra.any_linear.any_d2;
