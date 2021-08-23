with
     ada.Characters.latin_1;


package body any_Math.any_Algebra.any_linear
is

   -----------
   --- Vectors
   --

   function Norm_squared (Self : in Vector) return Real
   is
      Norm_2 : Real := 0.0;
   begin
      for Each in Self'Range
      loop
         Norm_2 := Norm_2  +  Self (Each) * Self (Each);
      end loop;

      return Norm_2;
   end Norm_squared;



   procedure normalise (Self : in out Vector)
   is
      use Vectors;
      inverse_Norm : constant Real := 1.0 / abs Self;
   begin
      for Each in Self'Range
      loop
         Self (Each) := Self (Each) * inverse_Norm;
      end loop;
   end normalise;



   function Normalised (Self : in Vector) return Vector
   is
      Result : Vector := Self;
   begin
      normalise (Result);
      return Result;
   end Normalised;



   procedure normalise (Self : in out Vector_2)
   is
      inverse_Norm : constant Real := 1.0 / abs Self;
   begin
      Self := Self * inverse_Norm;
   end normalise;



   function Normalised (Self : in Vector_2) return Vector_2
   is
      inverse_Norm : constant Real := 1.0 / abs Self;
   begin
      return Self * inverse_Norm;
   end Normalised;



   procedure normalise (Self : in out Vector_3)
   is
      inverse_Norm : constant Real := 1.0 / abs Self;
   begin
      Self := Self * inverse_Norm;
   end normalise;



   function Normalised (Self : in Vector_3) return Vector_3
   is
      inverse_Norm : constant Real := 1.0 / abs Self;
   begin
      return Self * inverse_Norm;
   end Normalised;



   function Min (Left, Right : in Vector) return Vector
   is
      Min : Vector (Left'Range);
   begin
      pragma Assert (Left'Length = Right'Length);

      for Each in Min'Range
      loop
         Min (Each) := Real'Min (Left  (Each),
                                 Right (Each));
      end loop;

      return Min;
   end Min;



   function Max (Left, Right : in Vector) return Vector
   is
      Max : Vector (Left'Range);
   begin
      pragma Assert (Left'Length = Right'Length);

      for Each in Max'Range
      loop
         Max (Each) := Real'Max (Left  (Each),
                                 Right (Each));
      end loop;

      return Max;
   end Max;



   function scaled (Self : in Vector;   By : in Vector) return Vector
   is
      Result : Vector (Self'Range);
   begin
      for Each in Result'Range
      loop
         Result (Each) := Self (Each) * By (Each);
      end loop;

      return Result;
   end scaled;



   ------------
   --- Matrices
   --

   function to_Matrix (Row_1, Row_2, Row_3 : in Vector_3) return Matrix_3x3
   is
   begin
      return ((Row_1 (1), Row_1 (2), Row_1 (3)),
              (Row_2 (1), Row_2 (2), Row_2 (3)),
              (Row_3 (1), Row_3 (2), Row_3 (3)));

   end to_Matrix;



   function Min (Self : in Matrix) return Real
   is
      Min : Real := Real'Last;
   begin
      for each_Row in Self'Range (1)
      loop
         for each_Col in Self'Range (2)
         loop
            Min := Real'Min (Min,
                             Self (each_Row, each_Col));
         end loop;
      end loop;

      return Min;
   end Min;



   function Max (Self : in Matrix) return Real
   is
      Max : Real := Real'First;
   begin
      for each_Row in Self'Range (1)
      loop
         for each_Col in Self'Range (2)
         loop
            Max := Real'Max (Max,
                             Self (each_Row, each_Col));
         end loop;
      end loop;

      return Max;
   end Max;



   function Image (Self : in Matrix) return String
   is
      Image : String (1 .. 1024 * 1024);         -- Handles one megabyte image, excess is truncated.
      Count : Standard.Natural         := 0;

      procedure add (Text : in String)
      is
      begin
         Image (Count + 1 .. Count + text'Length) := Text;
         Count                                    := Count + text'Length;
      end add;

   begin
      add ("(");

      for Row in self'Range (1)
      loop
         add ((1 => ada.Characters.latin_1.LF));

         if Row /= self'First (1)
         then
            add (", ");
         end if;

         for Col in self'Range (2)
         loop
            if Col /= self'First (2)
            then
               add (", ");
            end if;

            add (Real'Image (Self (Row, Col)));
         end loop;
      end loop;

      add (")");

      return Image (1 .. Count);

   exception
      when others =>
         return Image (1 .. Count);
   end Image;



   function is_Square (Self : in Matrix) return Boolean
   is
   begin
      return Self'Length (1) = Self'Length (2);
   end is_Square;



   function sub_Matrix (Self : in Matrix;   start_Row, end_Row : in Index;
                                            start_Col, end_Col : in Index) return Matrix
   is
      sub_Matrix : Matrix (1 .. end_Row - start_Row + 1,
                           1 .. end_Col - start_Col + 1);
   begin
      for each_Row in sub_Matrix'Range (1)
      loop
         for each_Col in sub_Matrix'Range (2)
         loop
            sub_Matrix (each_Row, each_Col) := Self (each_Row + start_Row - 1,
                                                     each_Col + start_Col - 1);
         end loop;
      end loop;

      return sub_Matrix;
   end sub_Matrix;



   function Identity (Size : in Index := 3) return Matrix
   is
      Result : Matrix (1 .. Size, 1 .. Size);
   begin
      for Row in 1 .. Size
      loop
         for Col in 1 .. Size
         loop
            if Row = Col
            then   Result (Row, Col) := 1.0;
            else   Result (Row, Col) := 0.0;
            end if;
         end loop;
      end loop;

      return Result;
   end Identity;



   procedure invert (Self : in out Matrix)
   is
      use Vectors;
   begin
      Self := Inverse (Self);
   end invert;



   ---------------
   --- Quaternions
   --

   function to_Quaternion (axis_X,
                           axis_Y,
                           axis_Z : in Real;
                           Angle  : in Real) return Quaternion
   is
      Result : Quaternion;
      L      : Real      := axis_X * axis_X  +  axis_Y * axis_Y  +  axis_Z * axis_Z;
   begin
      if L > 0.0
      then
         declare
            use Functions;
            half_Angle : constant Real := Angle * 0.5;
         begin
            Result.R     := Cos (half_Angle);
            L            := Sin (half_Angle) * (1.0 / SqRt (L));
            Result.V (1) := axis_X * L;
            Result.V (2) := axis_Y * L;
            Result.V (3) := axis_Z * L;
         end;
      else
         Result.R     := L;
         Result.V (1) := 0.0;
         Result.V (2) := 0.0;
         Result.V (3) := 0.0;
      end if;

      return Result;
   end to_Quaternion;



   function to_Quaternion (Axis  : in Vector_3;
                           Angle : in Real) return Quaternion
   is
      Result : Quaternion;
      L      : Real      := Axis * Axis;
   begin
      if L > 0.0
      then
         declare
            use Functions;
            half_Angle : constant Real := Angle * 0.5;
         begin
            Result.R := Cos (half_Angle);
            L        := Sin (half_Angle) * (1.0 / SqRt (L));
            Result.V := Axis * L;
         end;
      else
         Result.R := L;
         Result.V := (0.0, 0.0, 0.0);
      end if;

      return Result;
   end to_Quaternion;



   function "*" (Self : in Quaternion;
                 By   : in Quaternion) return Quaternion
   is
      x    : constant := 1;
      y    : constant := 2;
      z    : constant := 3;

      A    : Quaternion renames Self;
      B    : Quaternion renames By;

      AtBt : constant Real := A.R     * B.R;
      AxBx : constant Real := A.V (x) * B.V (x);
      AyBy : constant Real := A.V (y) * B.V (y);
      AzBz : constant Real := A.V (z) * B.V (z);

      AtBx : constant Real := A.R     * B.V (x);
      AxBt : constant Real := A.V (x) * B.R;
      AyBz : constant Real := A.V (y) * B.V (z);
      AzBy : constant Real := A.V (z) * B.V (y);

      AtBy : constant Real := A.R     * B.V (y);
      AxBz : constant Real := A.V (x) * B.V (z);
      AyBt : constant Real := A.V (y) * B.R;
      AzBx : constant Real := A.V (z) * B.V (x);

      AtBz : constant Real := A.R     * B.V (z);
      AxBy : constant Real := A.V (x) * B.V (y);
      AyBx : constant Real := A.V (y) * B.V (x);
      AzBt : constant Real := A.V (z) * B.R;

   begin
      return (R =>  AtBt - AxBx - AyBy - AzBz,
              V => (AtBx + AxBt + AyBz - AzBy,
                    AtBy - AxBz + AyBt + AzBx,
                    AtBz + AxBy - AyBx + AzBt));
   end "*";



   function Unit (Self : in Quaternion) return Quaternion
   is
   begin
      return to_Quaternion (      to_Vector (Self)
                            / abs to_Vector (Self));
   end Unit;



   function infinitesimal_Rotation_from (Self             : in Quaternion;
                                         angular_Velocity : in Vector_3) return Quaternion
   is
      i_Rotation : Quaternion;
   begin
      i_Rotation.R     := 0.5 * (- angular_Velocity (1) * Self.V (1)
                                 - angular_Velocity (2) * Self.V (2)
                                 - angular_Velocity (3) * Self.V (3));

      i_Rotation.V (1) := 0.5 * (  angular_Velocity (1) * Self.R
                                 + angular_Velocity (2) * Self.V (3)
                                 - angular_Velocity (3) * Self.V (2));

      i_Rotation.V (2) := 0.5 * (- angular_Velocity (1) * Self.V (3)
                                 + angular_Velocity (2) * Self.R
                                 + angular_Velocity (3) * Self.V (1));

      i_Rotation.V (3) := 0.5 * (  angular_Velocity (1) * Self.V (2)
                                 - angular_Velocity (2) * Self.V (1)
                                 + angular_Velocity (3) * Self.R);
      return i_Rotation;
   end infinitesimal_Rotation_from;



   function euler_Angles (Self : in Quaternion) return Vector_3     -- 'Self' can be a non-normalised quaternion.
   is
      use Functions;

      w : Real renames Self.R;
      x : Real renames Self.V (1);
      y : Real renames Self.V (2);
      z : Real renames Self.V (3);

      the_Angles : Vector_3;
      Bank       : Real renames the_Angles (1);
      Heading    : Real renames the_Angles (2);
      Attitude   : Real renames the_Angles (3);

      sqw : constant Real := w * w;
      sqx : constant Real := x * x;
      sqy : constant Real := y * y;
      sqz : constant Real := z * z;

      unit : constant Real := sqx + sqy + sqz + sqw;      -- If normalised then is 1.0 else is a correction factor.
      test : constant Real := x * y  +  z * w;

   begin
      if test > 0.499 * unit
      then   -- Singularity at north pole.
         Heading  := 2.0 * arcTan (x, w);
         Attitude := Pi / 2.0;
         Bank     := 0.0;
         return the_Angles;
      end if;

      if test < -0.499 * unit
      then   -- Singularity at south pole.
         Heading  := -2.0 * arcTan (x, w);
         Attitude := -Pi / 2.0;
         Bank     := 0.0;
         return the_Angles;
      end if;

      Heading  := arcTan (2.0 * y * w  -  2.0 * x * z,   sqx - sqy - sqz + sqw);
      Bank     := arcTan (2.0 * x * w  -  2.0 * y * z,  -sqx + sqy - sqz + sqw);
      Attitude := arcSin (2.0 * test / unit);

      return the_Angles;
   end euler_Angles;



   function to_Quaternion (Self : in Matrix_3x3) return Quaternion
   is
      use Functions;

      TR : Real;
      S  : Real;

      Result : Quaternion;

   begin
      TR := Self (1, 1)  +  Self (2, 2)  +  Self (3, 3);

      if TR >= 0.0
      then
         S        := SqRt (TR + 1.0);
         Result.R := 0.5 * S;

         S            := 0.5 * (1.0 / S);
         Result.V (1) := (Self (3, 2)  -  Self (2, 3)) * S;
         Result.V (2) := (Self (1, 3)  -  Self (3, 1)) * S;
         Result.V (3) := (Self (2, 1)  -  Self (1, 2)) * S;

         return Result;
      end if;

      --  Otherwise, find the largest diagonal element and apply the appropriate case.
      --
      declare
         function case_1_Result return Quaternion
         is
         begin
            S            := SqRt (Self (1, 1)  -  (Self (2, 2)  +  Self (3, 3))  +  1.0);
            Result.V (1) := 0.5 * S;

            S                    := 0.5 * (1.0 / S);
            Result.V (2) := (Self (1, 2) + Self (2, 1)) * S;
            Result.V (3) := (Self (3, 1) + Self (1, 3)) * S;
            Result.R     := (Self (3, 2) - Self (2, 3)) * S;

            return Result;
         end case_1_Result;

         function case_2_Result return Quaternion
         is
         begin
            S            := SqRt (Self (2, 2)  -  (Self (3, 3)  +  Self (1, 1))  +  1.0);
            Result.V (2) := 0.5 * S;

            S            := 0.5 * (1.0 / S);
            Result.V (3) := (Self (2, 3) + Self (3, 2)) * S;
            Result.V (1) := (Self (1, 2) + Self (2, 1)) * S;
            Result.R     := (Self (1, 3) - Self (3, 1)) * S;

            return Result;
         end case_2_Result;

         function case_3_Result return Quaternion
         is
         begin
            S            := SqRt (Self (3, 3)  -  (Self (1, 1) + Self (2, 2))  +  1.0);
            Result.V (3) := 0.5 * S;

            S                    := 0.5 * (1.0 / S);
            Result.V (1) := (Self (3, 1) + Self (1, 3)) * S;
            Result.V (2) := (Self (2, 3) + Self (3, 2)) * S;
            Result.R     := (Self (2, 1) - Self (1, 2)) * S;

            return Result;
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



   function Conjugate (Self : in Quaternion) return Quaternion
   is
   begin
      return (Self.R, -Self.V);
   end conjugate;



   procedure normalise (Self : in out Quaternion)
   is
   begin
      Self := Normalised (Self);
   end normalise;



   function Normalised (Self : in Quaternion) return Quaternion
   is
   begin
      return to_Quaternion (Vector_4 (Normalised (Vector (to_Vector (Self)))));
   end Normalised;


end any_Math.any_Algebra.any_linear;
