package body any_math.any_Algebra.any_linear.any_d3
is

   ------------
   --- Vector_3
   --

   function Angle (Point_1, Point_2, Point_3 : Vector_3) return Radians
   is
      use Functions;

      Vector_1  : constant Vector_3 := Normalised (Point_1 - Point_2);
      Vector_2  : constant Vector_3 := Normalised (Point_3 - Point_2);
      cos_Theta : constant Real     := Vector_1 * Vector_2;
   begin
      if cos_Theta >= 1.0
      then
         return ada.Numerics.Pi;
      else
         return arcCos (cos_Theta);
      end if;
   end Angle;



   function Angle_between_pre_Norm (U, V : in Vector_3) return Radians
   is
      use Functions;
      Val : Real := U * V;   -- Dot product.
   begin
      if    val < -1.0 then   val := -1.0;   -- Clamp to avoid rounding errors; arcCos will
      elsif val >  1.0 then   val :=  1.0;   -- fail with values outside this range.
      end if;

      return arcCos (Val);
   end Angle_between_pre_Norm;



   function Midpoint (From, To : in Vector_3) return Vector_3
   is
   begin
      return ((From (1) + To (1)) * 0.5,
              (From (2) + To (2)) * 0.5,
              (From (3) + To (3)) * 0.5);
   end Midpoint;



   function Distance (From, To : in Vector_3) return Real
   is
   begin
      return abs (From - To);
   end Distance;



   function Interpolated (From, To : in Vector_3;
                          Percent  : in unit_Percentage) return Vector_3
   is
      P : constant Real := to_Real (Percent);
      S : constant Real := 1.0 - P;
   begin
      return (S * From (1)  +  P * To (1),
              S * From (2)  +  P * To (2),
              S * From (3)  +  P * To (3));
   end Interpolated;



   --------------
   --- Matrix_3x3
   --

   function to_Matrix (Row_1, Row_2, Row_3 : in Vector_3) return Matrix_3x3
   is
   begin
      return ((Row_1 (1), Row_1 (2), Row_1 (3)),
              (Row_2 (1), Row_2 (2), Row_2 (3)),
              (Row_3 (1), Row_3 (2), Row_3 (3)));
   end to_Matrix;



   function forward_Direction (Matrix : in Matrix_3x3) return Vector_3
   is
   begin
      return Normalised (Row (Matrix, 3));
   end forward_Direction;



   function up_Direction (Matrix : in Matrix_3x3) return Vector_3
   is
   begin
      return Normalised (Row (Matrix, 2));
   end up_Direction;



   function right_Direction (Matrix : in Matrix_3x3) return Vector_3
   is
   begin
      return Normalised (Row (Matrix, 1));
   end right_Direction;


   --  Following procedure is from Project Spandex, by Paul Nettle.
   --
   procedure re_Orthonormalise (Matrix  : in out Matrix_3x3)
   is
      use Functions;
      m : Matrix_3x3 renames Matrix;

      dot1,
      dot2,
      vlen : Real;

   begin
      dot1 := m (1, 1) * m (2, 1) + m (1, 2) * m (2, 2) + m (1, 3) * m (2, 3);
      dot2 := m (1, 1) * m (3, 1) + m (1, 2) * m (3, 2) + m (1, 3) * m (3, 3);

      m (1, 1) := m (1, 1) - dot1 * m (2, 1) - dot2 * m (3, 1);
      m (1, 2) := m (1, 2) - dot1 * m (2, 2) - dot2 * m (3, 2);
      m (1, 3) := m (1, 3) - dot1 * m (2, 3) - dot2 * m (3, 3);

      vlen := 1.0 / SqRt (m (1, 1) * m (1, 1) +
                        m (1, 2) * m (1, 2) +
                        m (1, 3) * m (1, 3));

      m (1, 1) := m (1, 1) * vlen;
      m (1, 2) := m (1, 2) * vlen;
      m (1, 3) := m (1, 3) * vlen;

      dot1 := m (2, 1) * m (1, 1) + m (2, 2) * m (1, 2) + m (2, 3) * m (1, 3);
      dot2 := m (2, 1) * m (3, 1) + m (2, 2) * m (3, 2) + m (2, 3) * m (3, 3);

      m (2, 1) := m (2, 1) - dot1 * m (1, 1) - dot2 * m (3, 1);
      m (2, 2) := m (2, 2) - dot1 * m (1, 2) - dot2 * m (3, 2);
      m (2, 3) := m (2, 3) - dot1 * m (1, 3) - dot2 * m (3, 3);

      vlen := 1.0 / SqRt (m (2, 1) * m (2, 1) +
                        m (2, 2) * m (2, 2) +
                        m (2, 3) * m (2, 3));

      m (2, 1) := m (2, 1) * vlen;
      m (2, 2) := m (2, 2) * vlen;
      m (2, 3) := m (2, 3) * vlen;

      m (3, 1) := m (1, 2) * m (2, 3) - m (1, 3) * m (2, 2);
      m (3, 2) := m (1, 3) * m (2, 1) - m (1, 1) * m (2, 3);
      m (3, 3) := m (1, 1) * m (2, 2) - m (1, 2) * m (2, 1);
   end re_Orthonormalise;


   -------------
   --- Rotations
   --

   function xyz_Rotation (x_Angle, y_Angle, z_Angle : in Real) return Matrix_3x3
   is
   begin
      return   x_Rotation_from (x_Angle)
             * y_Rotation_from (y_Angle)
             * z_Rotation_from (z_Angle);
   end xyz_Rotation;



   function xyz_Rotation (Angles : in Vector_3) return Matrix_3x3
   is
   begin
      return xyz_Rotation (Angles (1),
                           Angles (2),
                           Angles (3));
   end xyz_Rotation;



   function x_Rotation_from (Angle : in Radians) return Matrix_3x3
   is
      use Functions;
      A : Radians renames Angle;
   begin
      return ((1.0,      0.0,      0.0),
              (0.0,  Cos (A), -Sin (A)),
              (0.0,  Sin (A),  Cos (A)));
   end x_Rotation_from;




   function y_Rotation_from (Angle : in Radians) return Matrix_3x3
   is
      use Functions;
      A : Radians renames Angle;
   begin
      return (( Cos (A),  0.0,  Sin (A)),
              (     0.0,  1.0,      0.0),
              (-Sin (A),  0.0,  Cos (A)));
   end y_Rotation_from;



   function z_Rotation_from (Angle : in Radians) return Matrix_3x3
   is
      use Functions;
      A : Radians renames Angle;
   begin
      return ((Cos (A),  -Sin (A),  0.0),
              (Sin (A),   Cos (A),  0.0),
              (    0.0,       0.0,  1.0));
   end z_Rotation_from;



   function to_Rotation (Axis_x,
                         Axis_y,
                         Axis_z   : in Real;
                         Rotation : in Radians) return Matrix_3x3
   is
   begin
      return to_Matrix (to_Quaternion (Axis_x,
                                       Axis_y,
                                       Axis_z,
                                       Rotation));
   end to_Rotation;



   function to_Rotation (Axis  : in Vector_3;
                         Angle : in Real) return Matrix_3x3
   is
   begin
      return to_Rotation (Axis (1), Axis (2), Axis (3),
                          Angle);
   end to_Rotation;



   ---------
   --- Euler
   --

   function to_Rotation (Angles : in Euler) return Matrix_3x3
   is
      use Functions;
      A  : constant Real := Cos (Angles (1));
      B  : constant Real := Sin (Angles (1));
      C  : constant Real := Cos (Angles (2));
      D  : constant Real := Sin (Angles (2));
      E  : constant Real := Cos (Angles (3));
      F  : constant Real := Sin (Angles (3));
      AD : constant Real := A * D;
      BD : constant Real := B * D;
   begin
      return (( C  * E,          -C  * F,           D    ),
              ( BD * E + A * F,  -BD * F + A * E,  -B * C),
              (-AD * E + B * F,   AD * F + B * E,   A * C));
   end to_Rotation;



   function to_translation_Matrix (Translation : Vector_3) return Matrix_4x4
   is
   begin
      return ((            1.0,             0.0,             0.0, 0.0),
              (            0.0,             1.0,             0.0, 0.0),
              (            0.0,             0.0,             1.0, 0.0),
              (Translation (1), Translation (2), Translation (3), 1.0));
   end to_translation_Matrix;



   -----------
   --- General
   --

   function to_Perspective (FoVy : in Degrees;
                            Aspect,
                            zNear,
                            zFar : in Real) return Matrix_4x4
   is
      use Functions;

      deltaZ    : constant Real    := zFar - zNear;
      Rads      : constant Radians := to_Radians (FoVy / 2.0);
      Sine      : constant Real    := Sin (Rads);
      Cotangent :          Real;

   begin
      if deltaZ = 0.0 or Sine = 0.0 or Aspect = 0.0
      then
         raise Constraint_Error;   -- tbd: 'mesa' simnply returns here ... ?
      end if;

      Cotangent := cos (Rads) / Sine;

      return ((Cotangent / Aspect,        0.0,                           0.0,   0.0),
              (               0.0,  Cotangent,                           0.0,   0.0),
              (               0.0,        0.0,      -(zFar + zNear) / deltaZ,  -1.0),
              (               0.0,        0.0,  -2.0 * zNear * zFar / deltaZ,   0.0));
   end to_Perspective;



   function to_viewport_Transform (Origin : Vector_2;   Extent : Vector_2) return Matrix_4x4
   is
      SX : constant Real := Extent (1) / 2.0;
      SY : constant Real := Extent (2) / 2.0;
   begin
      return (1 => (            SX,              0.0, 0.0, 0.0),
              2 => (            0.0,              SY, 0.0, 0.0),
              3 => (            0.0,             0.0, 1.0, 0.0),
              4 => (SX + Origin (1), SY + Origin (2), 0.0, 1.0));
   end to_viewport_Transform;



   function Look_at (Eye, Center, Up : Vector_3) return Matrix_4x4
   is
      Forward   : constant Vector_3 := Normalised ((Center (1) - Eye (1),
                                                    Center (2) - Eye (2),
                                                    Center (3) - Eye (3)));
      Side      : constant Vector_3 := Forward * Up;
      new_Up    : constant Vector_3 := Side * Forward;
   begin
      return  ((    Side (1),      Side (2),      Side (3),  0.0),
               (  new_Up (1),    new_Up (2),    new_Up (3),  0.0),
               (-Forward (1),  -Forward (2),  -Forward (3),  0.0),
               (    -Eye (1),      -Eye (2),      -Eye (3),  1.0));
   end Look_at;



   -----------------
   --- Transform_3d
   --

   function to_Transform (Matrix : in Matrix_4x4) return Transform_3d
   is
   begin
      return (Rotation    => get_Rotation    (Matrix),
              Translation => get_Translation (Matrix));
   end to_Transform;



   function "*" (Left : in Transform_3d;   Right : in Vector_3) return Vector_3
   is
   begin
      return (Row (Left.Rotation, 1) * Right  +  Left.Translation (1),
              Row (Left.Rotation, 2) * Right  +  Left.Translation (2),
              Row (Left.Rotation, 3) * Right  +  Left.Translation (3));
   end "*";



   function "*" (Left : in Vector_3;   Right : in Transform_3d) return Vector_3
   is
   begin
      return Right * Left;
   end "*";



   function "*" (Left : in Transform_3d;   Right : in Transform_3d) return Transform_3d
   is
   begin
      return (Rotation    => Left.Rotation * Right.Rotation,
              Translation => Left          * Right.Translation);

   end "*";



   function "*" (Left : in Vector_3;   Right : in Matrix_4x4) return Vector_3
   is
      use Vectors;
      Result : constant Vector := Vector (Left & 1.0) * Matrix (Right);
   begin
      return Vector_3 (Result (1 .. 3));
   end "*";



   function Invert (Transform : in Transform_3d) return Transform_3d
   is
      Rotation : constant Matrix_3x3 := Transpose (Transform.Rotation);
   begin
      return (Translation => Rotation * (-Transform.Translation),
              Rotation    => Rotation);
   end Invert;



   function inverse_Transform (Transform : in Transform_3d;   Vector : in Vector_3) return Vector_3
   is
      V : constant Vector_3 := Vector - Transform.Translation;
   begin
      return Transpose (Transform.Rotation) * V;
   end inverse_Transform;



   ---------------
   --- Quaternions
   --

   function to_Quaternion (Matrix : in Matrix_3x3) return Quaternion
   is
      use Functions;
      TR     : constant Real := Matrix (1, 1)  +  Matrix (2, 2)  +  Matrix (3, 3);
      S      :          Real;
      Result :          Quaternion;

   begin
      if TR >= 0.0
      then
         S        := SqRt (TR + 1.0);
         Result.R := 0.5 * S;

         S            := 0.5 * (1.0 / S);
         Result.V (1) := (Matrix (3, 2)  -  Matrix (2, 3)) * S;
         Result.V (2) := (Matrix (1, 3)  -  Matrix (3, 1)) * S;
         Result.V (3) := (Matrix (2, 1)  -  Matrix (1, 2)) * S;

         return Result;
      end if;

      --  Otherwise, find the largest diagonal element and apply the appropriate case.
      --
      declare
         function case_1_Result return Quaternion
         is
         begin
            S            := SqRt (Matrix (1, 1)  -  (Matrix (2, 2)  +  Matrix (3, 3))  +  1.0);
            Result.V (1) := 0.5 * S;

            S            := 0.5 * (1.0 / S);
            Result.V (2) := (Matrix (1, 2) + Matrix (2, 1)) * S;
            Result.V (3) := (Matrix (3, 1) + Matrix (1, 3)) * S;
            Result.R     := (Matrix (3, 2) - Matrix (2, 3)) * S;

            return Result;
         end case_1_Result;

         function case_2_Result return Quaternion
         is
         begin
            S            := SqRt (Matrix (2, 2)  -  (Matrix (3, 3)  +  Matrix (1, 1))  +  1.0);
            Result.V (2) := 0.5 * S;

            S            := 0.5 * (1.0 / S);
            Result.V (3) := (Matrix (2, 3) + Matrix (3, 2)) * S;
            Result.V (1) := (Matrix (1, 2) + Matrix (2, 1)) * S;
            Result.R     := (Matrix (1, 3) - Matrix (3, 1)) * S;

            return Result;
         end case_2_Result;

         function case_3_Result return Quaternion
         is
         begin
            S            := SqRt (Matrix (3, 3)  -  (Matrix (1, 1) + Matrix (2, 2))  +  1.0);
            Result.V (3) := 0.5 * S;

            S            := 0.5 * (1.0 / S);
            Result.V (1) := (Matrix (3, 1) + Matrix (1, 3)) * S;
            Result.V (2) := (Matrix (2, 3) + Matrix (3, 2)) * S;
            Result.R     := (Matrix (2, 1) - Matrix (1, 2)) * S;

            return Result;
         end case_3_Result;

         pragma Inline (case_1_Result);
         pragma Inline (case_2_Result);
         pragma Inline (case_3_Result);

      begin
         if Matrix (2, 2) > Matrix (1, 1)
         then
            if Matrix (3, 3) > Matrix (2, 2)
            then
               return case_3_Result;
            end if;

            return case_2_Result;
         end if;

         if Matrix (3, 3) > Matrix (1, 1)
         then
            return case_3_Result;
         end if;

         return case_1_Result;
      end;
   end to_Quaternion;



   procedure set_from_Matrix_3x3 (Quat : out Quaternion;   Matrix : in Matrix_3x3)
   is
      use Functions;
      S  :          Real;
      TR : constant Real := 1.0 + Matrix (1, 1)
                                + Matrix (2, 2)
                                + Matrix (3, 3);
   begin
      if TR > 1.0e-9
      then
         S          := SqRt (TR);
         Quat.R     := 0.5 * S;
         S          := 0.5 / S;
         Quat.V (1) := (Matrix (2, 3) - Matrix (3, 2)) * S;
         Quat.V (2) := (Matrix (3, 1) - Matrix (1, 3)) * S;
         Quat.V (3) := (Matrix (1, 2) - Matrix (2, 1)) * S;

      else
         declare
            I : Index := 0;
         begin
            if Matrix (2, 2) > Matrix (1, 1)
            then
               I := 1;
            end if;

            if Matrix (3, 3) > Matrix (I, I)
            then
               I := 2;
            end if;

            case I
            is
               when 0 =>
                  S          := SqRt ((Matrix (1, 1) - (Matrix (2, 2) + Matrix (3, 3))) + 1.0);
                  Quat.V (1) := 0.5 * S;
                  S          := 0.5 / S;
                  Quat.V (2) := (Matrix (2, 1) + Matrix (1, 2)) * S;
                  Quat.V (3) := (Matrix (1, 3) + Matrix (3, 1)) * S;
                  Quat.R     := (Matrix (2, 3) - Matrix (3, 2)) * S;

               when 1 =>
                  S          := SqRt ((Matrix (2, 2) - (Matrix (3, 3) + Matrix (1, 1))) + 1.0);
                  Quat.V (2) := 0.5 * S;
                  S          := 0.5 / S;
                  Quat.V (3) := (Matrix (3, 2) + Matrix (2, 3)) * S;
                  Quat.V (1) := (Matrix (2, 1) + Matrix (1, 2)) * S;
                  Quat.R     := (Matrix (3, 1) - Matrix (1, 3)) * S;

               when 2 =>
                  S          := SqRt ((Matrix (3, 3) - (Matrix (1, 1) + Matrix (2, 2))) + 1.0);
                  Quat.V (3) := 0.5 * S;
                  S          := 0.5 / S;
                  Quat.V (1) := (Matrix (1, 3) + Matrix (3, 1)) * S;
                  Quat.V (2) := (Matrix (3, 2) + Matrix (2, 3)) * S;
                  Quat.R     := (Matrix (1, 2) - Matrix (2, 1)) * S;

               when others =>
                  raise Program_Error;
            end case;
         end;
      end if;
   end set_from_Matrix_3x3;



   function to_Matrix (Quat : in Quaternion) return Matrix_3x3
   is
      Result : Matrix_3x3;

      qq2 : constant Real := 2.0  *  Quat.V (1)  *  Quat.V (1);
      qq3 : constant Real := 2.0  *  Quat.V (2)  *  Quat.V (2);
      qq4 : constant Real := 2.0  *  Quat.V (3)  *  Quat.V (3);
   begin
      Result (1, 1) := 1.0 - qq3 - qq4;
      Result (1, 2) := 2.0 * (Quat.V (1) * Quat.V (2) - Quat.R * Quat.V (3));
      Result (1, 3) := 2.0 * (Quat.V (1) * Quat.V (3) + Quat.R * Quat.V (2));

      Result (2, 1) := 2.0 * (Quat.V (1) * Quat.V (2) + Quat.R * Quat.V (3));
      Result (2, 2) := 1.0 - qq2 - qq4;
      Result (2, 3) := 2.0 * (Quat.V (2) * Quat.V (3) - Quat.R * Quat.V (1));

      Result (3, 1) := 2.0 * (Quat.V (1) * Quat.V (3) - Quat.R * Quat.V (2));
      Result (3, 2) := 2.0 * (Quat.V (2) * Quat.V (3) + Quat.R * Quat.V (1));
      Result (3, 3) := 1.0 - qq2 - qq3;

      return Result;
   end to_Matrix;



   function "+" (Left, Right : in Quaternion) return Quaternion
   is
   begin
      return
        (R =>  Left.R     + Right.R,
         V => (Left.V (1) + Right.V (1),
               Left.V (2) + Right.V (2),
               Left.V (3) + Right.V (3)));
   end "+";



   function "-" (Left, Right : in Quaternion) return Quaternion
   is
   begin
      return
        (R =>  Left.R     - Right.R,
         V => (Left.V (1) - Right.V (1),
               Left.V (2) - Right.V (2),
               Left.V (3) - Right.V (3)));
   end "-";



   function "-" (Quat : in Quaternion) return Quaternion
   is
   begin
      return (-Quat.R,  -Quat.V);
   end "-";



   function "*" (Left, Right : in Quaternion) return Real
   is
   begin
      return
           Left.R     * Right.R
        +  Left.V (1) * Right.V (1)
        +  Left.V (2) * Right.V (2)
        +  Left.V (3) * Right.V (3);
   end "*";



   function "*" (Left, Right : in Quaternion) return Quaternion
   is
   begin
      return
        (V => (Left.R * Right.V (1)  +  Left.V (1) * Right.R  +  Left.V (2) * Right.V (3)  -  Left.V (3) * Right.V (2),
               Left.R * Right.V (2)  +  Left.V (2) * Right.R  +  Left.V (3) * Right.V (1)  -  Left.V (1) * Right.V (3),
               Left.R * Right.V (3)  +  Left.V (3) * Right.R  +  Left.V (1) * Right.V (2)  -  Left.V (2) * Right.V (1)),
         R =>    Left.R     * Right.R
              -  Left.V (1) * Right.V (1)
              -  Left.V (2) * Right.V (2)
              -  Left.V (3) * Right.V (3));
   end "*";



   function Invert (Quat : in Quaternion) return Quaternion
   is
   begin
      return (Quat.R,  -Quat.V);
   end Invert;



   function Angle (Quat : in Quaternion) return Real
   is
      use Functions;
      Q : Quaternion := Quat;
   begin
      if Q.R > 1.0
      then
         normalise (Q);   -- If R > 1.0, arCos and SqRt will produce errors, this cant happen if quaternion is normalised.
      end if;

      return 2.0 * arcCos (Q.R);
   end Angle;



   function Axis (Quat : in Quaternion) return Vector_3
   is
   begin
      return Quat.V;
   end Axis;



   function "*" (Left : in Quaternion;   Right : in Vector_3) return Quaternion
   is
   begin
      return
        ( Left.R     * Right (1)  +  Left.V (2) * Right (3)  -  Left.V (3) * Right (2),
         (Left.R     * Right (2)  +  Left.V (3) * Right (1)  -  Left.V (1) * Right (3),
          Left.R     * Right (3)  +  Left.V (1) * Right (2)  -  Left.V (2) * Right (1),
         -Left.V (1) * Right (1)  -  Left.V (2) * Right (2)  -  Left.V (3) * Right (3)));
   end "*";



   function "*" (Left : in Vector_3;   Right : in Quaternion) return Quaternion
   is
   begin
      return
        (-Left (1) * Right.V (1) -  Left (2) * Right.V (2)  -  Left (3) * Right.V (3),
         (Left (1) * Right.R     +  Left (2) * Right.V (3)  -  Left (3) * Right.V (2),
          Left (2) * Right.R     +  Left (3) * Right.V (1)  -  Left (1) * Right.V (3),
          Left (3) * Right.R     +  Left (1) * Right.V (2)  -  Left (2) * Right.V (1)));
   end "*";



   function "*" (Left : in Quaternion;   Right : in Real) return Quaternion
   is
   begin
      return
        (R =>  Left.R     * Right,
         V => (Left.V (1) * Right,
               Left.V (2) * Right,
               Left.V (3) * Right));
   end "*";



   function Norm (Quat : in Quaternion) return Real
   is
      use Functions;
   begin
      return SqRt (  Quat.R     * Quat.R
                   + Quat.V (1) * Quat.V (1)
                   + Quat.V (2) * Quat.V (2)
                   + Quat.V (3) * Quat.V (3));
   end Norm;



   function Versor (Quat : in Quaternion) return Quaternion
   is
   begin
      return Quat / Norm (Quat);
   end Versor;



   function Farthest (Quat : in Quaternion;   qd : in Quaternion) return Quaternion
   is
      Diff : constant Quaternion := Quat - qd;
      Sum  : constant Quaternion := Quat + qd;
   begin
      if Diff * Diff  >  Sum * Sum
      then
         return qd;
      end if;

      return -qd;
   end Farthest;



   function Interpolated (From,
                          To      : in Quaternion;
                          Percent : in unit_Percentage) return Quaternion
   is
   begin
      if    Percent  =   0.0 then   return From;
      elsif Percent  = 100.0 then   return To;
      end if;

      declare
         --  Q1 and Q2 should be unit length or else something broken will happen.
         Q1  : Quaternion :=      From;
         Q2  : Quaternion renames To;

         Dot           : Real     := Q1 * Q2;
         Dot_Threshold : constant := 0.9995;

         P : constant Real := to_Real (Percent);
      begin
         if Dot < 0.0
         then     -- Ensure we take the short path.
            Q1  := -Q1;
            Dot := -Dot;
         end if;

         if Dot > Dot_Threshold
         then
            return Normalised (Q1 +  (Q2 - Q1) * P);
         end if;

         clamp (Dot, -1.0, 1.0);     -- Robustness: Stay within domain of arcCos.

         declare
            use Functions;
            theta_0 : constant Real       := arcCos (Dot);               -- theta_0 = Angle between input vectors.
            theta   : constant Real       := theta_0 * P;                -- theta   = Angle between Q1 and result.
            Q3      : constant Quaternion := Normalised (Q2 - Q1*Dot);
         begin
            return Q1 * Cos (theta)  +  Q3 * Sin (theta);
         end;
      end;
   end Interpolated;



   ------------
   --- Vector_4
   --

   function "/" (Left, Right : Vector_4) return Vector_4
   is
   begin
      return (Left (1) / Right (1),
              Left (2) / Right (2),
              Left (3) / Right (3),
              Left (4) / Right (4));
   end "/";



   function max_Axis (Vector : in Vector_4) return Integer
   is
      max_Index : Integer := -1;
      max_Val   : Real    := -1.0e30;
   begin
      if Vector (1) > max_Val
      then
         max_Index := 1;
         max_Val   := Vector (1);
      end if;

      if Vector (2) > max_Val
      then
         max_Index := 2;
         max_Val   := Vector (2);
      end if;

      if Vector (3) > max_Val
      then
         max_Index := 3;
         max_Val   := Vector (3);
      end if;

      if Vector (4) > max_Val
      then
         max_Index := 4;
         max_Val   := Vector (4);
      end if;

      return max_Index;
   end max_Axis;



   function closest_Axis (Vector : in Vector_4) return Integer
   is
   begin
      return max_Axis ((abs (Vector (1)),
                        abs (Vector (2)),
                        abs (Vector (3)),
                        abs (Vector (4))));
   end closest_Axis;



   --  From mesa.
   --
   function unProject (From       : in Vector_3;
                       Model      : in Matrix_4x4;
                       Projection : in Matrix_4x4;
                       Viewport   : in Rectangle) return Vector_3
   is
      final_Matrix    : constant Matrix_4x4 := Transpose (Inverse (Model * Projection));
      window_Position :          Vector_4   := (From (1), From (2), From (3), 1.0);
      world_Position  :          Vector_4;
   begin
      --  Map x and y from window coordinates.
      --
      window_Position (1) := (window_Position (1) - Real (Viewport.Min (1))) / Real (Viewport.Max (1));
      window_Position (2) := (window_Position (2) - Real (Viewport.Min (2))) / Real (Viewport.Max (2));

      window_Position (1) := window_Position (1) * 2.0 - 1.0;     -- Map to range -1.0 .. 1.0.
      window_Position (2) := window_Position (2) * 2.0 - 1.0;
      window_Position (3) := window_Position (3) * 2.0 - 1.0;

      world_Position      := final_Matrix * window_Position;

      if world_Position (4) = 0.0
      then
         raise Constraint_Error with "unProject: world_Position (4) = 0.0";
      else
         world_Position (1) := world_Position (1) / world_Position (4);
         world_Position (2) := world_Position (2) / world_Position (4);
         world_Position (3) := world_Position (3) / world_Position (4);
      end if;

      return Vector_3 (world_Position (1 .. 3));
   end unProject;



   --------------
   --- Matrix_4x4
   --

   function to_translate_Matrix (Translation : in Vector_3) return Matrix_4x4
   is
   begin
      return ((            1.0,             0.0,             0.0,   0.0),
              (            0.0,             1.0,             0.0,   0.0),
              (            0.0,             0.0,             1.0,   0.0),
              (Translation (1), Translation (2), Translation (3),   1.0));
   end to_translate_Matrix;



   function to_scale_Matrix (Scale : in Vector_3) return Matrix_4x4
   is
   begin
      return ((Scale (1),        0.0,        0.0,   0.0),
              (0.0,        Scale (2),        0.0,   0.0),
              (0.0,              0.0,  Scale (3),   0.0),
              (0.0,              0.0,        0.0,   1.0));
   end to_scale_Matrix;



   function to_rotate_Matrix (Rotation : in Matrix_3x3) return Matrix_4x4
   is
   begin
      return ((Rotation (1, 1),  Rotation (1, 2),  Rotation (1, 3),   0.0),
              (Rotation (2, 1),  Rotation (2, 2),  Rotation (2, 3),   0.0),
              (Rotation (3, 1),  Rotation (3, 2),  Rotation (3, 3),   0.0),
              (            0.0,              0.0,              0.0,   1.0));

   end to_rotate_Matrix;



   function to_transform_Matrix (Rotation : in Matrix_3x3;   Translation : in Vector_3) return Matrix_4x4
   is
   begin
      return ((Rotation (1, 1),  Rotation (1, 2),  Rotation (1, 3),   0.0),
              (Rotation (2, 1),  Rotation (2, 2),  Rotation (2, 3),   0.0),
              (Rotation (3, 1),  Rotation (3, 2),  Rotation (3, 3),   0.0),
              (Translation (1),  Translation (2),  Translation (3),   1.0));
   end to_transform_Matrix;



   --------------
   --- Transforms
   --

   function to_transform_Matrix (Transform : in Transform_3d) return Matrix_4x4
   is
   begin
      return   to_rotate_Matrix    (Transform.Rotation)
             * to_translate_Matrix (Transform.Translation);
   end to_transform_Matrix;



   function get_Rotation (Transform : in Matrix_4x4) return Matrix_3x3
   is
   begin
      return ((Transform (1, 1),  Transform (1, 2),  Transform (1, 3)),
              (Transform (2, 1),  Transform (2, 2),  Transform (2, 3)),
              (Transform (3, 1),  Transform (3, 2),  Transform (3, 3)));
   end get_Rotation;



   procedure set_Rotation (Transform : in out Matrix_4x4;   To : in Matrix_3x3)
   is
   begin
      Transform (1, 1) := To (1, 1);   Transform (1, 2) := To (1, 2);   Transform (1, 3) := To (1, 3);   Transform (1, 4) := 0.0;
      Transform (2, 1) := To (2, 1);   Transform (2, 2) := To (2, 2);   Transform (2, 3) := To (2, 3);   Transform (2, 4) := 0.0;
      Transform (3, 1) := To (3, 1);   Transform (3, 2) := To (3, 2);   Transform (3, 3) := To (3, 3);   Transform (3, 4) := 0.0;
   end set_Rotation;



   function get_Translation (Transform : in Matrix_4x4) return Vector_3
   is
   begin
      return (Transform (4, 1),  Transform (4, 2),  Transform (4, 3));
   end get_Translation;



   procedure set_Translation (Transform : in out Matrix_4x4;   To : in Vector_3)
   is
   begin
      Transform (4, 1) := To (1);
      Transform (4, 2) := To (2);
      Transform (4, 3) := To (3);
      Transform (4, 4) := 1.0;
   end set_Translation;



   function inverse_Rotation (Rotation : in Matrix_3x3) return Matrix_3x3
   is
--        pragma Suppress (all_Checks);          -- For speed.
      m : Matrix_3x3 renames Rotation;
   begin
      return ((m (1,1),  m (2,1),  m (3,1)),
              (m (1,2),  m (2,2),  m (3,2)),
              (m (1,3),  m (2,3),  m (3,3)));
   end inverse_Rotation;



   function inverse_Transform (Transform : in Matrix_4x4) return Matrix_4x4
   is
--        pragma Suppress (all_Checks);          -- For speed.

      m :          Matrix_4x4 renames Transform;

      R : constant Matrix_3x3 := (( m (1,1),  m (2,1),  m (3,1)),
                                  ( m (1,2),  m (2,2),  m (3,2)),
                                  ( m (1,3),  m (2,3),  m (3,3)));

      T : constant Vector_3   := Vector_3' (m (4,1), m (4,2), m (4,3)) * R;
   begin
      return (( m (1,1),  m (2,1),  m (3,1), 0.0),
              ( m (1,2),  m (2,2),  m (3,2), 0.0),
              ( m (1,3),  m (2,3),  m (3,3), 0.0),
              (  -T (1),   -T (2),   -T (3), 1.0));
   end inverse_Transform;


end any_math.any_Algebra.any_linear.any_d3;
