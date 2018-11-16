with
     impact.d3.Quaternions;
with interfaces.c.Pointers;
with Interfaces.C;
with impact.d3.Scalar;



package body impact.d3.Matrix
is


--     function to_Matrix (Q : in Quaternion) return Matrix_3x3
--     is
--        the_Matrix : Matrix_3x3;
--     begin
--        setRotation (the_Matrix, Q);
--        return the_Matrix;
--     end;



   function to_Matrix (xx, xy, xz,
                       yx, yy, yz,
                       zx, zy, zz : in Real) return Matrix_3x3
   is
   begin
      return (1 => (xx, xy, xz),
              2 => (yx, yy, yz),
              3 => (zx, zy, zz));
   end to_Matrix;




   procedure setRotation (Self : out Matrix_3x3;   From : in Quaternion)
   is
      use impact.d3.Quaternions;

      q  : Quaternion renames From;

      d  : constant Real := length2 (q);     pragma Assert (d /= 0.0);
      s  : constant Real := 2.0 / d;

      xs : constant Real := x (q) * s;
      ys : constant Real := y (q) * s;
      zs : constant Real := z (q) * s;

      wx : constant Real := w (q) * xs;
      wy : constant Real := w (q) * ys;
      wz : constant Real := w (q) * zs;

      xx : constant Real := x (q) * xs;
      xy : constant Real := x (q) * ys;
      xz : constant Real := x (q) * zs;

      yy : constant Real := y (q) * ys;
      yz : constant Real := y (q) * zs;
      zz : constant Real := z (q) * zs;
   begin
      Self := (1 => (1.0 - (yy + zz),   xy - wz,           xz + wy),
               2 => (xy + wz,           1.0 - (xx + zz),   yz - wx),
               3 => (xz - wy,           yz + wx,           1.0 - (xx + yy)));
   end setRotation;




   function getColumn (Self : in Matrix_3x3;   Which : in math.Index) return Vector_3
   is
      pragma Assert (Which >= 1 and then Which <= 3);
   begin
      return (Self (1, which),
              Self (2, which),
              Self (3, which));
   end getColumn;


   function getRow (Self : in Matrix_3x3;   Which : in math.Index) return Vector_3
   is
      pragma Assert (Which >= 1 and then Which <= 3);
   begin
      return (Self (which, 1),
              Self (which, 2),
              Self (which, 3));
   end getRow;



   function Row (Self : access Matrix_3x3;   Which : in math.Index) return access Vector_3
   is
      pragma Assert (Which >= 1 and then Which <= 3);

      the_Row : aliased Vector_3;
      for the_Row'Address use Self (Which, 1)'Address;
   begin
      return the_Row'Unchecked_Access;
   end Row;





   type Scalars is array (Natural range <>) of aliased Real;

   package scalar_Pointers is new interfaces.c.Pointers (Natural, Real, Scalars, 0.0);



   procedure setFromOpenGLSubMatrix (Self : in out Matrix_3x3;   m : access Real)
   is
      the_Array : Scalars := scalar_Pointers.Value (m.all'Access);
   begin
      Self := (1 => (the_Array (0), the_Array (4), the_Array (8)),
               2 => (the_Array (1), the_Array (5), the_Array (9)),
               3 => (the_Array (2), the_Array (6), the_Array (10)));
   end setFromOpenGLSubMatrix;






   procedure setValue (Self : out Matrix_3x3;   xx, xy, xz,
                                                yx, yy, yz,
                                                zx, zy, zz : in Real)
   is
   begin
      Self := (1 => (xx, xy, xz),
               2 => (yx, yy, yz),
               3 => (zx, zy, zz));
   end setValue;





   procedure setEulerYPR (Self : out Matrix_3x3;   yaw,
                                                   pitch,
                                                   roll : in Real)
   is
   begin
      setEulerZYX (Self,  roll, pitch, yaw);
   end setEulerYPR;





   procedure setEulerZYX (Self : out Matrix_3x3;   eulerX,
                                                   eulerY,
                                                   eulerZ : in Real)
   is
      use math.Functions;

      ci : constant Real := Cos (eulerX);
      cj : constant Real := Cos (eulerY);
      ch : constant Real := Cos (eulerZ);
      si : constant Real := Sin (eulerX);
      sj : constant Real := Sin (eulerY);
      sh : constant Real := Sin (eulerZ);
      cc : constant Real := ci * ch;
      cs : constant Real := ci * sh;
      sc : constant Real := si * ch;
      ss : constant Real := si * sh;
   begin
      --  todo: proposed to reverse this since it's labeled zyx but takes arguments xyz and it will match all other parts of the code

      setValue (Self,   cj * ch,   sj * sc - cs,   sj * cc + ss,
                        cj * sh,   sj * ss + cc,   sj * cs - sc,
                            -sj,        cj * si,   cj * ci   );
   end setEulerZYX;



   procedure setIdentity (Self : out Matrix_3x3)
   is
   begin
      Self := getIdentity;
   end setIdentity;




   function getIdentity return Matrix_3x3
   is
   begin
      return ((1.0, 0.0, 0.0),
              (0.0, 1.0, 0.0),
              (0.0, 0.0, 1.0));
   end getIdentity;




   procedure getOpenGLSubMatrix (Self : in Matrix_3x3;   matrix : access Real)
   is
      use scalar_Pointers,  Interfaces;

      m : constant scalar_Pointers.Pointer := matrix.all'Access;

      procedure set (Offset : in c.ptrdiff_t;   Value : in Real)
      is
         Target : constant scalar_Pointers.Pointer := m + Offset;
      begin
         Target.all := Value;
      end set;

   begin
      set (0,  Self (1, 1));
      set (1,  Self (2, 1));
      set (2,  Self (3, 1));
      set (3,          0.0);

      set (4,  Self (1, 2));
      set (5,  Self (2, 2));
      set (6,  Self (3, 2));
      set (7,          0.0);

      set (8,  Self (1, 3));
      set (9,  Self (2, 3));
      set (10,  Self (3, 3));
      set (11,          0.0);
   end getOpenGLSubMatrix;




   procedure getRotation (Self : in Matrix_3x3;   q : out Quaternion)
   is
      use math.Functions;

      trace : constant Real                := Self (1, 1) + Self (2, 2) + Self (3, 3);
      temp  : array (1 .. 4) of Real;
   begin

      if trace > 0.0 then
         declare
            s : Real := sqRt (trace + 1.0);
         begin
            temp (4) := (s * 0.5);
            s        := 0.5 / s;

            temp (1) := (Self (3, 2) - Self (2, 3)) * s;
            temp (2) := (Self (1, 3) - Self (3, 1)) * s;
            temp (3) := (Self (2, 1) - Self (1, 2)) * s;
         end;
      else
         declare
            function get_i return Integer
            is
            begin
               if Self (1, 1) < Self (2, 2) then

                  if Self (2, 2) < Self (3, 3) then
                     return 3;
                  else
                     return 2;
                  end if;

               else

                  if Self (1, 1) < Self (3, 3) then
                     return 3;
                  else
                     return 1;
                  end if;

               end if;
            end get_i;

            i : constant Integer := get_i;


            function get_j return Integer
            is
            begin
               if i = 3 then   return 1;
               else            return i + 1;
               end if;
            end get_j;

            function get_k return Integer
            is
            begin
               if    i = 2 then   return 1;
               elsif i = 3 then   return 2;
               elsif i = 1 then   return 3;
               else               raise Program_Error;
               end if;
            end get_k;

            j : constant Integer := get_j;
            k : constant Integer := get_k;

            s : Real  := sqRt (Self (i, i) - Self (j, j) - Self (k, k) + 1.0);
         begin
            temp (i) := s * 0.5;
            s        := 0.5 / s;

            temp (4) := (Self (k, j) - Self (j, k)) * s;
            temp (j) := (Self (j, i) + Self (i, j)) * s;
            temp (k) := (Self (k, i) + Self (i, k)) * s;
         end;
      end if;

      q := (V => (temp (1), temp (2), temp (3)),
            R => temp (4));
   end getRotation;





   procedure getEulerYPR (Self : in Matrix_3x3;   yaw,
                                                  pitch,
                                                  roll : out Real)
   is
      use impact.d3.Scalar, math.Functions;
   begin
      --  first use the normal calculus
      --
      yaw   := arcTan (Self (2, 1),  Self (1, 1));
      pitch := arcSin (-Self (3, 1));
      roll  := arcTan (Self (3, 2),  Self (3, 3));

      --  on pitch = +/-HalfPI
      --
      if abs (pitch) = SIMD_HALF_PI then

         if yaw > 0.0 then
            yaw := yaw - SIMD_PI;
         else
            yaw := yaw + SIMD_PI;
         end if;

         if roll > 0.0 then
            roll := roll - SIMD_PI;
         else
            roll := roll + SIMD_PI;
         end if;

      end if;

   end getEulerYPR;






   procedure getEulerZYX (Self : in Matrix_3x3;   yaw,
                                                  pitch,
                                                  roll            :     out Real;
                                                  solution_number : in      Integer := 1)
   is
      use impact.d3.Scalar, math.Functions;

      type Euler is
         record
            yaw,
            pitch,
            roll  : Real;
         end record;

      euler_out,
      euler_out2 : Euler; -- second solution

      the_Delta  : Real;
   begin
      --  get the pointer to the raw data

      --  Check that pitch is not at a singularity
      if abs (Self (3, 1)) >= 1.0 then
         euler_out.yaw  := 0.0;
         euler_out2.yaw := 0.0;

         --  From difference of angles formula
         the_Delta := arcTan (Self (1, 1),  Self (1, 3));

         if Self (3, 1) > 0.0 then                            -- gimbal locked up
            euler_out.pitch := SIMD_PI / 2.0;
            euler_out2.pitch := SIMD_PI / 2.0;
            euler_out.roll := euler_out.pitch + the_Delta;
            euler_out2.roll := euler_out.pitch + the_Delta;
         else                                               -- gimbal locked down
            euler_out.pitch  := -SIMD_PI / 2.0;
            euler_out2.pitch := -SIMD_PI / 2.0;
            euler_out.roll   := -euler_out.pitch + the_Delta;
            euler_out2.roll  := -euler_out.pitch + the_Delta;
         end if;
      else
         euler_out.pitch  := -arcSin (Self (3, 1));
         euler_out2.pitch := SIMD_PI - euler_out.pitch;

         euler_out.roll  := arcTan (Self (3, 2) / Cos (euler_out.pitch),
                                    Self (3, 3) / Cos (euler_out.pitch));
         euler_out2.roll := arcTan (Self (3, 2) / Cos (euler_out2.pitch),
                                    Self (3, 3) / Cos (euler_out2.pitch));

         euler_out.yaw   := arcTan (Self (2, 1) / Cos (euler_out.pitch),
                                    Self (1, 1) / Cos (euler_out.pitch));
         euler_out2.yaw  := arcTan (Self (2, 1) / Cos (euler_out2.pitch),
                                    Self (1, 1) / Cos (euler_out2.pitch));
      end if;

      if solution_number = 1 then
         yaw   := euler_out.yaw;
         pitch := euler_out.pitch;
         roll  := euler_out.roll;
      else
         yaw   := euler_out2.yaw;
         pitch := euler_out2.pitch;
         roll  := euler_out2.roll;
      end if;

   end getEulerZYX;




   function Scaled (Self : in Matrix_3x3;   s : in Vector_3) return Matrix_3x3
   is
   begin
      return ((Self (1, 1) * s (1),  Self (1, 2) * s (2),  Self (1, 3) * s (3)),
              (Self (2, 1) * s (1),  Self (2, 2) * s (2),  Self (2, 3) * s (3)),
              (Self (3, 1) * s (1),  Self (3, 2) * s (2),  Self (3, 3) * s (3)));
   end Scaled;




   function adjoint (Self : in Matrix_3x3) return Matrix_3x3
   is
   begin
      return ((cofac (Self,  2, 2, 3, 3),  cofac (Self,  1, 3, 3, 2),  cofac (Self,  1, 2, 2, 3)),
              (cofac (Self,  2, 3, 3, 1),  cofac (Self,  1, 1, 3, 3),  cofac (Self,  1, 3, 2, 1)),
              (cofac (Self,  2, 1, 3, 2),  cofac (Self,  1, 2, 3, 1),  cofac (Self,  1, 1, 2, 2)));
   end adjoint;





   function cofac (Self : in Matrix_3x3;   r1, c1,
                                           r2, c2 : in Integer) return Real
   is
   begin
      return    Self (r1, c1) * Self (r2, c2)
             -  Self (r1, c2) * Self (r2, c1);
   end cofac;






   function absolute (Self : in Matrix_3x3) return Matrix_3x3
   is
   begin
      return ((abs (Self (1, 1)),  abs (Self (1, 2)),  abs (Self (1, 3))),
              (abs (Self (2, 1)),  abs (Self (2, 2)),  abs (Self (2, 3))),
              (abs (Self (3, 1)),  abs (Self (3, 2)),  abs (Self (3, 3))));
   end absolute;






   function transposeTimes (Self : in Matrix_3x3;   m : in Matrix_3x3) return Matrix_3x3
   is
   begin
      return
        ((Self (1, 1) * m (1, 1)  +  Self (2, 1) * m (2, 1)  +  Self (3, 1) * m (3, 1),
          Self (1, 1) * m (1, 2)  +  Self (2, 1) * m (2, 2)  +  Self (3, 1) * m (3, 2),
          Self (1, 1) * m (1, 3)  +  Self (2, 1) * m (2, 3)  +  Self (3, 1) * m (3, 3)),

         (Self (1, 2) * m (1, 1)  +  Self (2, 2) * m (2, 1)  +  Self (3, 2) * m (3, 1),
          Self (1, 2) * m (1, 2)  +  Self (2, 2) * m (2, 2)  +  Self (3, 2) * m (3, 2),
          Self (1, 2) * m (1, 3)  +  Self (2, 2) * m (2, 3)  +  Self (3, 2) * m (3, 3)),

         (Self (1, 3) * m (1, 1)  +  Self (2, 3) * m (2, 1)  +  Self (3, 3) * m (3, 1),
          Self (1, 3) * m (1, 2)  +  Self (2, 3) * m (2, 2)  +  Self (3, 3) * m (3, 2),
          Self (1, 3) * m (1, 3)  +  Self (2, 3) * m (2, 3)  +  Self (3, 3) * m (3, 3)));
   end transposeTimes;





   function timesTranspose (Self : in Matrix_3x3;   m : in Matrix_3x3) return Matrix_3x3
   is
      use math.Vectors;
   begin
      return ((getRow (Self, 1) * getRow (m, 1),  getRow (Self, 1) * getRow (m, 2),  getRow (Self, 1) * getRow (m, 3)),
              (getRow (Self, 2) * getRow (m, 1),  getRow (Self, 2) * getRow (m, 2),  getRow (Self, 2) * getRow (m, 3)),
              (getRow (Self, 3) * getRow (m, 1),  getRow (Self, 3) * getRow (m, 2),  getRow (Self, 3) * getRow (m, 3)));

   end timesTranspose;





   function tdotx (Self : in Matrix_3x3;   v : in Vector_3) return Real
   is
   begin
      return   Self (1, 1) * v (1)
             + Self (2, 1) * v (2)
             + Self (3, 1) * v (3);
   end tdotx;



   function tdoty (Self : in Matrix_3x3;   v : in Vector_3) return Real
   is
   begin
      return   Self (1, 2) * v (1)
             + Self (2, 2) * v (2)
             + Self (3, 2) * v (3);
   end tdoty;



   function tdotz (Self : in Matrix_3x3;   v : in Vector_3) return Real
   is
   begin
      return   Self (1, 3) * v (1)
             + Self (2, 3) * v (2)
             + Self (3, 3) * v (3);
   end tdotz;






   procedure diagonalize (Self : in out Matrix_3x3;   rot       : access Matrix_3x3;
                                                      threshold : in     Real;
                                                      maxSteps  : in     Integer)
   is
      use impact.d3.Scalar;

      Step : Integer := maxSteps;
   begin
      setIdentity (rot.all);

      while Step > 0 loop
         --  Find off-diagonal element [p][q] with largest magnitude
         --
         declare
            p : Integer := 1;
            q : Integer := 2;
            r : Integer := 3;

            max : Real := abs (Self (1, 2));
            v   : Real := abs (Self (1, 3));
            t   : Real;
         begin
            if v > max then
               q   := 3;
               r   := 2;
               max := v;
            end if;

            v := abs (Self (2, 3));

            if v > max then
               p   := 2;
               q   := 3;
               r   := 1;
               max := v;
            end if;

            t := threshold * (abs (Self (1, 1))  +  abs (Self (3, 2))  +  abs (Self (3, 3)));

            if max <= t then
               if max <= SIMD_EPSILON * t then
                  return;
               end if;

               step := 1;
            end if;

            --  Compute Jacobi rotation J which leads to a zero for element [p][q]
            --
            declare
               use math.Functions;

               mpq    : constant Real := Self (p, q);

               theta  : constant Real := (Self (q, q) - Self (p, p)) / (2.0 * mpq);
               theta2 : constant Real := theta * theta;

               cos    : Real;
               sin    : Real;

               mrp,
               mrq    : Real;
            begin

               if theta2 * theta2 < 10.0 / SIMD_EPSILON then

                  if theta >= 0.0 then
                     t := 1.0 / (theta + sqRt (1.0 + theta2));
                  else
                     t := 1.0 / (theta - sqRt (1.0 + theta2));
                  end if;

                  cos := 1.0 / sqRt (1.0 + t * t);
                  sin := cos * t;
               else   -- Approximation for large theta-value (ie, a nearly diagonal matrix).
                  t   := 1.0 / (theta * (2.0  +  0.5 / theta2));
                  cos := 1.0  -  0.5 * t * t;
                  sin := cos * t;
               end if;

               --  Apply rotation to matrix (this = J^T * this * J)
               --
               Self (p, q) := 0.0;
               Self (q, p) := 0.0;
               Self (p, p) := Self (p, p) - t * mpq;
               Self (q, q) := Self (q, q) + t * mpq;

               mrp := Self (r, p);
               mrq := Self (r, q);

               Self (r, p) := cos * mrp - sin * mrq;
               Self (p, r) := Self (r, p);


               Self (r, q) := cos * mrq + sin * mrp;
               Self (q, r) := Self (r, q);


               --  Apply rotation to rot (rot = rot * J)
               --
               for i in 1 .. 3 loop
                  declare
                     the_Row : constant access Vector_3 := Row (rot, i);
                  begin
                     mrp := the_Row (p);
                     mrq := the_Row (q);

                     the_Row (p) := cos * mrp - sin * mrq;
                     the_Row (q) := cos * mrq + sin * mrp;
                  end;
               end loop;
            end;
         end;

         step := step - 1;
      end loop;

   end diagonalize;


end impact.d3.Matrix;
