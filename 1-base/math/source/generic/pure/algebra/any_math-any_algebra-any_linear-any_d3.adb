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
      Cos_Theta : constant Real     := Vector_1 * Vector_2;

   begin
      if cos_Theta >= 1.0
      then
         return ada.numerics.Pi;
      else
         return arcCos (cos_Theta);
      end if;
   end Angle;



   function angle_Between_preNorm (U : in Vector_3;   V : in Vector_3) return Real
   is
      use Functions;
      Val : Real := U * V;   -- Dot product.
   begin
      if    val < -1.0 then   val := -1.0;   -- Clamp to avoid rounding errors; arcCos will
      elsif val >  1.0 then   val :=  1.0;   -- fail with values outside this range.
      end if;

      return arcCos (Val);
   end angle_Between_preNorm;



   function Midpoint (Self, Other : Vector_3) return Vector_3
   is
   begin
      return ((Self (1) + Other (1)) * 0.5,
              (Self (2) + Other (2)) * 0.5,
              (Self (3) + Other (3)) * 0.5);
   end Midpoint;



   function Distance (Self : in Vector_3;   To : in Vector_3) return Real
   is
      Pad : constant Vector_3 := Self - To;
   begin
      return abs (Pad);
   end Distance;



   function Interpolated (v0, v1 : in Vector_3;   rt : in Real) return Vector_3
   is
      s : constant Real := 1.0 - rt;
   begin
      return (s * v0 (1) + rt * v1 (1),
              s * v0 (2) + rt * v1 (2),
              s * v0 (3) + rt * v1 (3));
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



   function forward_Direction (Self : in Matrix_3x3) return Vector_3
   is
   begin
      return Normalised (Row (Self, 3));
   end forward_Direction;



   function up_Direction (Self : in Matrix_3x3) return Vector_3
   is
   begin
      return Normalised (Row (Self, 2));
   end up_Direction;



   function right_Direction (Self : in Matrix_3x3) return Vector_3
   is
   begin
      return Normalised (Row (Self, 1));
   end right_Direction;




   --  Following procedure is from Project Spandex, by Paul Nettle.
   --
   procedure Re_Orthonormalize (Self : in out Matrix_3x3)
   is
      use Functions;

      M : Matrix_3x3 renames Self;

      dot1,
      dot2,
      vlen : Real;

   begin
      dot1 := m (1, 1) * m (2, 1) + m (1, 2) * m (2, 2) + m (1, 3) * m (2, 3);
      dot2 := m (1, 1) * m (3, 1) + m (1, 2) * m (3, 2) + m (1, 3) * m (3, 3);

      m (1, 1) := m (1, 1) - dot1 * m (2, 1) - dot2 * m (3, 1);
      m (1, 2) := m (1, 2) - dot1 * m (2, 2) - dot2 * m (3, 2);
      m (1, 3) := m (1, 3) - dot1 * m (2, 3) - dot2 * m (3, 3);

      vlen := 1.0 / sqRt (m (1, 1) * m (1, 1) +
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

      vlen := 1.0 / Sqrt (m (2, 1) * m (2, 1) +
                        m (2, 2) * m (2, 2) +
                        m (2, 3) * m (2, 3));

      m (2, 1) := m (2, 1) * vlen;
      m (2, 2) := m (2, 2) * vlen;
      m (2, 3) := m (2, 3) * vlen;

      m (3, 1) := m (1, 2) * m (2, 3) - m (1, 3) * m (2, 2);
      m (3, 2) := m (1, 3) * m (2, 1) - m (1, 1) * m (2, 3);
      m (3, 3) := m (1, 1) * m (2, 2) - m (1, 2) * m (2, 1);
   end Re_Orthonormalize;




   -------------
   --- Rotations
   --

   --  Replacement '*' to avoid memory leak in generic_real_array version.
   --
   function "*"(A, B : Matrix) return Matrix
   is
      r  : Real;
      AB : Matrix (A'Range (1),
                   A'Range (2));
   begin
      for i in A'Range (1)
      loop
         for j in A'Range (2)
         loop
            r := 0.0;

            for k in A'Range (1)
            loop
               r := r  +  (A (i, k)  *  B (k, j));
            end loop;

            AB (i, j) := r;
         end loop;
      end loop;

      return AB;
   end "*";




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




   function x_Rotation_from (the_Angle : in Radians) return Matrix_3x3
   is
      use Functions;
      A : Radians renames the_Angle;
   begin
      return ((1.0,      0.0,      0.0),
              (0.0,  cos (A), -sin (A)),
              (0.0,  sin (A),  cos (A)));
   end x_Rotation_from;




   function y_Rotation_from (the_Angle : in Radians) return Matrix_3x3
   is
      use Functions;
      A : Radians renames the_Angle;
   begin
      return (( cos (A),  0.0,  sin (A)),
              (     0.0,  1.0,      0.0),
              (-sin (A),  0.0,  cos (A)));
   end y_Rotation_from;




   function z_Rotation_from (the_Angle : in Radians) return Matrix_3x3
   is
      use Functions;
      A : Radians renames the_Angle;
   begin
      return ((cos (A),  -sin (A),  0.0),
              (sin (A),   cos (A),  0.0),
              (    0.0,       0.0,  1.0));
   end z_Rotation_from;




   function to_Rotation (Axis_x, Axis_y, Axis_z   : in     Real;
                         rotation_Angle           : in     Real) return Matrix_3x3
   is
   begin
      return to_Matrix (to_Quaternion (Axis_x,
                                       Axis_y,
                                       Axis_z,
                                       rotation_Angle));
   end to_Rotation;


   function to_Rotation     (Axis           : in     Vector_3;
                             Angle          : in     Real) return Matrix_3x3
   is
   begin
      return to_Rotation (Axis (1), Axis (2), Axis (3),
                          Angle);
   end to_Rotation;




   ---------
   --- Euler
   --

   function to_Rotation (Self : in Euler) return Matrix_3x3    -- From matrix_faq.
   is
      use Functions;
      A  : constant Real := cos (Self (1));
      B  : constant Real := sin (Self (1));
      C  : constant Real := cos (Self (2));
      D  : constant Real := sin (Self (2));
      E  : constant Real := cos (Self (3));
      F  : constant Real := sin (Self (3));
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

   function to_Perspective (fovy, aspect, zNear, zFar : Real) return Matrix_4x4
   is
      use Functions;
      deltaZ          : constant Real := zFar - zNear;
      radians         : constant Real := fovy / 2.0  *  Pi / 180.0;
      sine            : constant Real := sin (radians);
      cotangent       :          Real;

   begin
      if deltaZ = 0.0 or sine = 0.0 or aspect = 0.0
      then
         raise Constraint_Error;   -- tbd: 'mesa' simnply returns here ... ?
      end if;

      cotangent := cos (radians) / sine;

      return ((cotangent / aspect,        0.0,                           0.0,   0.0),
              (               0.0,  cotangent,                           0.0,   0.0),
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




   function Look_at (eye, center, up : Vector_3) return Matrix_4x4
   is
      forward   : constant Vector_3 := Normalised ((center (1) - eye (1),
                                                    center (2) - eye (2),
                                                    center (3) - eye (3)));
      side      : constant Vector_3 := forward * up;
      new_up    : constant Vector_3 := side * forward;
   begin
      return  (( side    (1),   side    (2),   side    (3),  0.0),
               ( new_up  (1),   new_up  (2),   new_up  (3),  0.0),
               (-forward (1),  -forward (2),  -forward (3),  0.0),
               (    -eye (1),      -eye (2),      -eye (3),  1.0));
   end Look_at;




   -----------------
   --- Transform_3d
   --

   function to_Transform (From : in Matrix_4x4) return Transform_3d
   is
   begin
      return (rotation    => get_Rotation    (From),
              translation => get_Translation (From));
   end to_Transform;



   function "*" (Left : in Transform_3d;   Right : in Vector_3) return Vector_3
   is
   begin
      return (Row (left.Rotation, 1) * Right  +  left.Translation (1),
              Row (left.Rotation, 2) * Right  +  left.Translation (2),
              Row (left.Rotation, 3) * Right  +  left.Translation (3));
   end "*";



   function "*" (Left : in Vector_3;   Right : in Transform_3d) return Vector_3
   is
   begin
      return Right * Left;
   end "*";



   function "*" (Left : in Transform_3d;   Right : in Transform_3d) return Transform_3d
   is
   begin
      return (rotation    => left.Rotation * right.Rotation,
              translation => Left          * right.Translation);

   end "*";




   function "*" (Left : in Vector_3;   Right : in Matrix_4x4) return Vector_3
   is
      use Vectors;

      Result : constant Vector := Vector (Left & 1.0) * Matrix (Right);
   begin
      return Vector_3 (Result (1 .. 3));
   end "*";




   function Invert (Self : in Transform_3d) return Transform_3d
   is
      inv : constant Matrix_3x3 := Transpose (Self.Rotation);
   begin
      return (translation => inv * (-Self.Translation),
              rotation    => inv);
   end Invert;




   function invXform (Self : in Transform_3d;   inVec : Vector_3) return Vector_3
   is
      v : constant Vector_3 := inVec - Self.Translation;
   begin
      return Transpose (Self.rotation) * v;
   end invXform;



   ---------------
   --- Quaternions
   --

   function to_Quaternion (Self : in Matrix_3x3) return Quaternion
   is
      use Functions;

      TR             : Real;
      S              : Real;
      the_Quaternion : Quaternion;

   begin
      TR := Self (1, 1)  +  Self (2, 2)  +  Self (3, 3);

      if TR >= 0.0
      then
         S                    := sqRt (TR + 1.0);
         the_Quaternion.R     := 0.5 * S;

         S                    := 0.5 * (1.0 / S);
         the_Quaternion.V (1) := (Self (3, 2)  -  Self (2, 3)) * S;
         the_Quaternion.V (2) := (Self (1, 3)  -  Self (3, 1)) * S;
         the_Quaternion.V (3) := (Self (2, 1)  -  Self (1, 2)) * S;

         return the_Quaternion;
      end if;


      --  Otherwise, find the largest diagonal element and apply the appropriate case.
      --
      declare
         function case_1_Result return Quaternion
         is
         begin
            S                    := Sqrt (Self (1, 1)  -  (Self (2, 2)  +  Self (3, 3))  +  1.0);
            the_Quaternion.V (1) := 0.5 * S;

            S                    := 0.5 * (1.0 / S);
            the_Quaternion.V (2) := (Self (1, 2) + Self (2, 1)) * S;
            the_Quaternion.V (3) := (Self (3, 1) + Self (1, 3)) * S;
            the_Quaternion.R     := (Self (3, 2) - Self (2, 3)) * S;

            return the_Quaternion;
         end case_1_Result;

         function case_2_Result return Quaternion
         is
         begin
            S                    := Sqrt (Self (2, 2)  -  (Self (3, 3)  +  Self (1, 1))  +  1.0);
            the_Quaternion.V (2) := 0.5 * S;

            S                    := 0.5 * (1.0 / S);
            the_Quaternion.V (3) := (Self (2, 3) + Self (3, 2)) * S;
            the_Quaternion.V (1) := (Self (1, 2) + Self (2, 1)) * S;
            the_Quaternion.R     := (Self (1, 3) - Self (3, 1)) * S;

            return the_Quaternion;
         end case_2_Result;

         function case_3_Result return Quaternion
         is
         begin
            S                    := Sqrt (Self (3, 3)  -  (Self (1, 1) + Self (2, 2))  +  1.0);
            the_Quaternion.V (3) := 0.5 * S;

            S                    := 0.5 * (1.0 / S);
            the_Quaternion.V (1) := (Self (3, 1) + Self (1, 3)) * S;
            the_Quaternion.V (2) := (Self (2, 3) + Self (3, 2)) * S;
            the_Quaternion.R     := (Self (2, 1) - Self (1, 2)) * S;

            return the_Quaternion;
         end case_3_Result;

         pragma Inline (case_1_Result);
         pragma Inline (case_2_Result);
         pragma Inline (case_3_Result);

      begin
         if Self (2, 2) > Self (1, 1)
         then
            if Self (3, 3) > Self (2, 2)
            then
               return case_3_Result;
            end if;

            return case_2_Result;
         end if;

         if Self (3, 3) > Self (1, 1)
         then
            return case_3_Result;
         end if;

         return case_1_Result;
      end;
   end to_Quaternion;




   procedure setFromMatrix3x3T (Self :    out Quaternion;   the_Matrix : in Matrix_3x3)
   is
      use Functions;

      s  :          Real;
      tr : constant Real := 1.0 + the_Matrix (1, 1)
                                + the_Matrix (2, 2)
                                + the_Matrix (3, 3);
   begin
      if tr > 1.0e-9
      then
         s          := sqRt (tr);
         Self.R     := 0.5 * s;
         s          := 0.5 / s;
         Self.V (1) := (the_Matrix (2, 3) - the_Matrix (3, 2)) * s;
         Self.V (2) := (the_Matrix (3, 1) - the_Matrix (1, 3)) * s;
         Self.V (3) := (the_Matrix (1, 2) - the_Matrix (2, 1)) * s;

      else
         declare
            I : Index := 0;
         begin
            if the_Matrix (2, 2) > the_Matrix (1, 1)
            then
               I := 1;
            end if;

            if the_Matrix (3, 3) > the_Matrix (i, i)
            then
               I := 2;
            end if;

            case I
            is
               when 0 =>
                  s          := sqrt ((the_Matrix (1, 1) - (the_Matrix (2, 2) + the_Matrix (3, 3))) + 1.0);
                  Self.V (1) := 0.5 * s;
                  s          := 0.5 / s;
                  Self.V (2) := (the_Matrix (2, 1) + the_Matrix (1, 2)) * s;
                  Self.V (3) := (the_Matrix (1, 3) + the_Matrix (3, 1)) * s;
                  Self.R     := (the_Matrix (2, 3) - the_Matrix (3, 2)) * s;

               when 1 =>
                  s          := sqrt ((the_Matrix (2, 2) - (the_Matrix (3, 3) + the_Matrix (1, 1))) + 1.0);
                  Self.V (2) := 0.5 * s;
                  s          := 0.5 / s;
                  Self.V (3) := (the_Matrix (3, 2) + the_Matrix (2, 3)) * s;
                  Self.V (1) := (the_Matrix (2, 1) + the_Matrix (1, 2)) * s;
                  Self.R     := (the_Matrix (3, 1) - the_Matrix (1, 3)) * s;

               when 2 =>
                  s          := sqrt ((the_Matrix (3, 3) - (the_Matrix (1, 1) + the_Matrix (2, 2))) + 1.0);
                  Self.V (3) := 0.5 * s;
                  s          := 0.5 / s;
                  Self.V (1) := (the_Matrix (1, 3) + the_Matrix (3, 1)) * s;
                  Self.V (2) := (the_Matrix (3, 2) + the_Matrix (2, 3)) * s;
                  Self.R     := (the_Matrix (1, 2) - the_Matrix (2, 1)) * s;

               when others =>
                  raise Program_Error;
            end case;
         end;
      end if;
   end setFromMatrix3x3T;




   function to_Matrix (Self : in Quaternion) return Matrix_3x3
   is
      the_Matrix : Matrix_3x3;

      qq2 : constant Real := 2.0  *  Self.V (1)  *  Self.V (1);
      qq3 : constant Real := 2.0  *  Self.V (2)  *  Self.V (2);
      qq4 : constant Real := 2.0  *  Self.V (3)  *  Self.V (3);
   begin
      the_Matrix (1, 1) := 1.0 - qq3 - qq4;
      the_Matrix (1, 2) := 2.0 * (Self.V (1) * Self.V (2) - Self.R * Self.V (3));
      the_Matrix (1, 3) := 2.0 * (Self.V (1) * Self.V (3) + Self.R * Self.V (2));

      the_Matrix (2, 1) := 2.0 * (Self.V (1) * Self.V (2) + Self.R * Self.V (3));
      the_Matrix (2, 2) := 1.0 - qq2 - qq4;
      the_Matrix (2, 3) := 2.0 * (Self.V (2) * Self.V (3) - Self.R * Self.V (1));

      the_Matrix (3, 1) := 2.0 * (Self.V (1) * Self.V (3) - Self.R * Self.V (2));
      the_Matrix (3, 2) := 2.0 * (Self.V (2) * Self.V (3) + Self.R * Self.V (1));
      the_Matrix (3, 3) := 1.0 - qq2 - qq3;

      return the_Matrix;
   end to_Matrix;



   function "+" (Left, Right : in Quaternion) return Quaternion
   is
   begin
      return
        (R => Left.R   + Right.R,
         V => (Left.V (1) + Right.V (1),
               Left.V (2) + Right.V (2),
               Left.V (3) + Right.V (3)));
   end "+";



   function "-" (Left, Right : in Quaternion) return Quaternion
   is
   begin
      return
        (R => Left.R   - Right.R,
         V => (Left.V (1) - Right.V (1),
               Left.V (2) - Right.V (2),
               Left.V (3) - Right.V (3)));
   end "-";



   function "-" (Self : in Quaternion) return Quaternion
   is
   begin
      return (-Self.R,  -Self.V);
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



   function Invert (Self : in Quaternion) return Quaternion
   is
   begin
      return (Self.R,  -Self.V);
   end Invert;



   function Angle (Self : in Quaternion) return Real
   is
      use Functions;
      Q : Quaternion := Self;

   begin
      if Q.R > 1.0
      then
         normalise (Q); -- If R > 1.0, arCos and SqRt will produce errors, this cant happen if quaternion is normalised.
      end if;

      return 2.0 * arcCos (Q.R);
   end Angle;



   function Axis  (Self : in Quaternion) return Vector_3
   is
   begin
      return Self.V;
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



   function "*" (Left : in Vector_3;     Right : in Quaternion) return Quaternion
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
        (R => Left.R * Right,
         V => (Left.V (1) * Right,
               Left.V (2) * Right,
               Left.V (3) * Right));
   end "*";



   function Norm (Self : in Quaternion) return Real
   is
      use Functions;
   begin
      return SqRt (  Self.R     * Self.R
                   + Self.V (1) * Self.V (1)
                   + Self.V (2) * Self.V (2)
                   + Self.V (3) * Self.V (3));
   end Norm;



   function  Versor (Self : in Quaternion) return Quaternion
   is
   begin
      return Self / Norm (Self);
   end Versor;



   function farthest (Self : in Quaternion;   qd : in Quaternion) return Quaternion
   is
      diff : constant Quaternion := Self - qd;
      sum  : constant Quaternion := Self + qd;
   begin
      if diff * diff  >  sum * sum
      then
         return qd;
      end if;

      return -qd;
   end farthest;



   function Slerp (Initial, Desired : in Quaternion;   Time : in Real) return Quaternion
   is
   begin
      if    Time  = 0.0 then   return Initial;
      elsif Time >= 1.0 then   return Desired;
      end if;

      declare
         --  Q1 and Q2 should be unit length or else something broken will happen.
         Q1  : Quaternion :=      Initial;
         Q2  : Quaternion renames Desired;

         Dot           : Real     := Q1 * Q2;
         DOT_THRESHOLD : constant := 0.9995;

      begin
         if Dot < 0.0
         then   -- Ensure we take the short path.
            Q1  := -Q1;
            Dot := -Dot;
         end if;

         if Dot > DOT_THRESHOLD
         then
            return Normalised (Q1 +  (Q2 - Q1) * Time);
         end if;

         clamp (Dot, -1.0, 1.0);           -- Robustness: Stay within domain of acos()

         declare
            use Functions;
            theta_0 : constant Real       := arcCos (Dot);               -- theta_0 = angle between input vectors
            theta   : constant Real       := theta_0 * Time;             -- theta   = angle between Q1 and result
            Q3      : constant Quaternion := Normalised (Q2 - Q1*Dot);
         begin
            return Q1 * cos (theta)  +  Q3 * sin (theta);
         end;
      end;
   end slerp;



   ------------
   --- Vector_4
   --

   function "/" (Left : Vector_4;     Right : Vector_4) return Vector_4
   is
   begin
      return (Left (1) / Right (1),
              Left (2) / Right (2),
              Left (3) / Right (3),
              Left (4) / Right (4));
   end "/";



   function maxAxis4 (Self : in Vector_4) return Integer
   is
      maxIndex : Integer := -1;
      maxVal   : Real    := -1.0e30;
   begin
      if Self (1) > maxVal
      then
         maxIndex := 1;
         maxVal   := Self (1);
      end if;

      if Self (2) > maxVal
      then
         maxIndex := 2;
         maxVal   := Self (2);
      end if;

      if Self (3) > maxVal
      then
         maxIndex := 3;
         maxVal   := Self (3);
      end if;

      if Self (4) > maxVal
      then
         maxIndex := 4;
         maxVal   := Self (4);
      end if;

      return maxIndex;
   end maxAxis4;



   function closestAxis4 (Self : in Vector_4) return Integer
   is
   begin
      return maxAxis4 ((abs (Self (1)),
                        abs (Self (2)),
                        abs (Self (3)),
                        abs (Self (4))));
   end closestAxis4;




   --  From mesa.
   --
   function unProject (win         : in Vector_3;
                       modelMatrix : in Matrix_4x4;
                       projMatrix  : in Matrix_4x4;
                       viewport    : in Rectangle) return Vector_3
   is
      finalMatrix     : constant Matrix_4x4 := Transpose (Inverse (modelMatrix * projMatrix));
      window_Position :          Vector_4   := (win (1), win (2), win (3), 1.0);
      world_Position  :          Vector_4;
   begin
      --  Map x and y from window coordinates.
      --
      window_Position (1) := (window_Position (1) - Real (viewport.Min (1))) / Real (viewport.Max (1));
      window_Position (2) := (window_Position (2) - Real (viewport.Min (2))) / Real (viewport.Max (2));

      window_Position (1) := window_Position (1) * 2.0 - 1.0;               -- Map to range -1 to 1
      window_Position (2) := window_Position (2) * 2.0 - 1.0;
      window_Position (3) := window_Position (3) * 2.0 - 1.0;

      world_Position      := finalMatrix * window_Position;

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

   function to_transform_Matrix (Self : in Transform_3d) return Matrix_4x4
   is
   begin
      return   to_rotate_Matrix (Self.Rotation)
             * to_translate_Matrix (Self.Translation);
   end to_transform_Matrix;



   function get_Rotation (Self : in Matrix_4x4) return Matrix_3x3
   is
   begin
      return ((Self (1, 1),  Self (1, 2),  Self (1, 3)),
              (Self (2, 1),  Self (2, 2),  Self (2, 3)),
              (Self (3, 1),  Self (3, 2),  Self (3, 3)));
   end get_Rotation;



   procedure set_Rotation (Self : in out Matrix_4x4;   To : in Matrix_3x3)
   is
   begin
      Self (1, 1) := To (1, 1);
      Self (1, 2) := To (1, 2);
      Self (1, 3) := To (1, 3);
      Self (1, 4) := 0.0;

      Self (2, 1) := To (2, 1);
      Self (2, 2) := To (2, 2);
      Self (2, 3) := To (2, 3);
      Self (2, 4) := 0.0;

      Self (3, 1) := To (3, 1);
      Self (3, 2) := To (3, 2);
      Self (3, 3) := To (3, 3);
      Self (3, 4) := 0.0;
   end set_Rotation;



   function get_Translation (Self : in Matrix_4x4) return Vector_3
   is
   begin
      return (Self (4, 1),  Self (4, 2),  Self (4, 3));
   end get_Translation;



   procedure set_Translation (Self : in out Matrix_4x4;   To : in Vector_3)
   is
   begin
      Self (4, 1) := To (1);
      Self (4, 2) := To (2);
      Self (4, 3) := To (3);
      Self (4, 4) := 1.0;
   end set_Translation;



   function  inverse_Rotation  (rotation_Matrix : in Matrix_3x3) return Matrix_3x3
   is
--        pragma Suppress (all_Checks);          -- For speed.
      m : Matrix_3x3 renames rotation_Matrix;
   begin
      return ((m (1,1),  m (2,1),  m (3,1)),
              (m (1,2),  m (2,2),  m (3,2)),
              (m (1,3),  m (2,3),  m (3,3)));
   end inverse_Rotation;



   function inverse_Transform (transform_Matrix : in Matrix_4x4) return Matrix_4x4
   is
--        pragma Suppress (all_Checks);          -- For speed.

      m :          Matrix_4x4 renames transform_Matrix;

      R : constant Matrix_3x3 := (( m (1,1),  m (2,1),  m (3,1)),
                                  ( m (1,2),  m (2,2),  m (3,2)),
                                  ( m (1,3),  m (2,3),  m (3,3)));

      T : constant Vector_3   := Vector_3' (m (4,1), m (4,2), m (4,3)) * R;
   begin
      return (( m (1,1),  m (2,1),  m (3,1), 0.0),
              ( m (1,2),  m (2,2),  m (3,2), 0.0),
              ( m (1,3),  m (2,3),  m (3,3), 0.0),
              ( -T (1),   -T (2),   -T (3),  1.0));
   end inverse_Transform;


end any_math.any_Algebra.any_linear.any_d3;
