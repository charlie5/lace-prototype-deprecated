with impact.d3.Scalar,
     impact.d3.Vector;




package body impact.d3.Quaternions
is

   function to_Quaternion (x, y, z, w : in Real) return Quaternion
   is
   begin
      return (x, (y, z, w));
   end to_Quaternion;



   function to_Quaternion (Axis : in Vector_3;   Angle : in Real) return Quaternion
   is
      Self : Quaternion;
   begin
      setRotation (Self, Axis, Angle);
      return Self;
   end to_Quaternion;





   function x (Self : in Quaternion) return Real
   is
   begin
      return Self.V (1);
   end x;



   function y (Self : in Quaternion) return Real
   is
   begin
      return Self.V (2);
   end y;


   function z (Self : in Quaternion) return Real
   is
   begin
      return Self.V (3);
   end z;


   function w (Self : in Quaternion) return Real
   is
   begin
      return Self.R;
   end w;





   procedure setRotation (Self : out Quaternion;   Axis  : in Vector_3;
                                                   Angle : in Real)
   is
      use impact.d3.Vector, math.Functions;
      d : constant Real := length (axis);             pragma Assert (d /= 0.0);
      s : constant Real := Sin (angle * 0.5) / d;
   begin
      Self := (V => (x (axis) * s,  y (axis) * s,  z (axis) * s),
               R => Cos (angle * 0.5));

   end setRotation;




   function to_Quaternion (Yaw, Pitch, Roll : in Real         ) return Quaternion
   is
      Self : Quaternion;
   begin
      if BT_EULER_DEFAULT_ZYX then
         setEulerZYX (Self,  Yaw, Pitch, Roll);
      else
         setEuler (Self,  Yaw, Pitch, Roll);
      end if;

      return Self;
   end to_Quaternion;




   procedure setEuler (Self : out Quaternion;   Yaw, Pitch, Roll : in Real)
   is
      use math.Functions;

      halfYaw   : constant Real := yaw   * 0.5;
      halfPitch : constant Real := pitch * 0.5;
      halfRoll  : constant Real := roll  * 0.5;

      cosYaw    : constant Real := Cos (halfYaw);
      sinYaw    : constant Real := Sin (halfYaw);
      cosPitch  : constant Real := Cos (halfPitch);
      sinPitch  : constant Real := Sin (halfPitch);
      cosRoll   : constant Real := Cos (halfRoll);
      sinRoll   : constant Real := Sin (halfRoll);
   begin
      Self := (V => (cosRoll * sinPitch * cosYaw  +  sinRoll * cosPitch * sinYaw,
                     cosRoll * cosPitch * sinYaw  -  sinRoll * sinPitch * cosYaw,
                     sinRoll * cosPitch * cosYaw  -  cosRoll * sinPitch * sinYaw),
               R => cosRoll * cosPitch * cosYaw  +  sinRoll * sinPitch * sinYaw);
   end setEuler;




   procedure setEulerZYX (Self : out Quaternion;   Yaw, Pitch, Roll : in Real)
   is
      use math.Functions;

      halfYaw   : constant Real := yaw   * 0.5;
      halfPitch : constant Real := pitch * 0.5;
      halfRoll  : constant Real := roll  * 0.5;

      cosYaw    : constant Real := Cos (halfYaw);
      sinYaw    : constant Real := Sin (halfYaw);
      cosPitch  : constant Real := Cos (halfPitch);
      sinPitch  : constant Real := Sin (halfPitch);
      cosRoll   : constant Real := Cos (halfRoll);
      sinRoll   : constant Real := Sin (halfRoll);
   begin
      Self := (V => (sinRoll * cosPitch * cosYaw  -  cosRoll * sinPitch * sinYaw,   -- x
                     cosRoll * sinPitch * cosYaw  +  sinRoll * cosPitch * sinYaw,   -- y
                     cosRoll * cosPitch * sinYaw  -  sinRoll * sinPitch * cosYaw),   -- z
               R => cosRoll * cosPitch * cosYaw  +  sinRoll * sinPitch * sinYaw);  -- formerly yzx
   end setEulerZYX;





   function multiply (Left, Right : in Quaternion) return Quaternion
   is
      use linear_Algebra_3d;
   begin
      return Left * Right;
   end multiply;




   function dot (Left, Right : in Quaternion) return Real
   is
      use linear_Algebra_3d;
   begin
      return Left * Right;
   end dot;





   function length2 (Self : in Quaternion) return Real
   is
   begin
      return dot (Self, Self);
   end length2;



   function length (Self : in Quaternion) return Real
   is
      use math.Functions;
   begin
      return sqRt (length2 (Self));
   end length;





   function normalize (Self : access Quaternion) return Quaternion
   is
      use math.Vectors;
   begin
      Self.all := normalized (Self.all);
      return Self.all;
   end normalize;



   function normalized (Self : in     Quaternion) return Quaternion
   is
      use linear_Algebra_3d;
   begin
      return Self / length (Self);
   end normalized;




   procedure normalize  (Self : in out Quaternion)
   is
   begin
      Self := normalized (Self);
   end normalize;





--     function "*" (Left : in Quaternion;   Right : in Real) return Quaternion
--     is
--        use math.real_Arrays;
--     begin
--        return Quaternion (math.Vector_4 (Left) * Right);
--     end;



--     function "/" (Left : in Quaternion;   Right : in Real) return Quaternion
--     is
--        use math.real_Arrays;
--     begin
--        pragma assert (Right /= 0.0);
--        return Left * (1.0 / Right);
--     end;




   function Angle (Left, Right : in Quaternion) return Real
   is
      use math.Functions;
      s : constant Real := sqRt (length2 (Left) * length2 (Right));     pragma Assert (s /= 0.0);
   begin
      return arcCos (dot (Left, Right) / s);
   end Angle;




   function getAngle (Self        : in Quaternion) return Real
   is
      use math.Functions;
   begin
      return 2.0 * arcCos (Self.R);
   end getAngle;



   function getAxis  (Self : in Quaternion) return Vector_3
   is
      use impact.d3.Scalar,  math.Functions;

      s_squared : constant Real := 1.0  -  Self.R * Self.R;
      s         : Real;
   begin
      if s_squared < 10.0 * SIMD_EPSILON then    -- Check for divide by zero
         return (1.0, 0.0, 0.0);   -- Arbitrary
      end if;

      s := 1.0 / sqRt (s_squared);

      return (Self.V (1) * s,
              Self.V (2) * s,
              Self.V (3) * s);
   end getAxis;




   function inverse  (Self : in Quaternion) return Quaternion
   is
   begin
      return (Self.R,
              -Self.V);
   end inverse;





--     function "+" (Left, Right : in Quaternion) return Quaternion
--     is
--        use math.real_Arrays;
--     begin
--        return Quaternion (Vector_4 (Left) + Vector_4 (Right));
--     end;
--
--
--
--     function "-" (Left, Right : in Quaternion) return Quaternion
--     is
--        use math.real_Arrays;
--     begin
--        return Quaternion (Vector_4 (Left) - Vector_4 (Right));
--     end;




   function "-" (Self : in Quaternion) return Quaternion
   is
   begin
      return (R => -Self.R,
              V => -Self.V);
   end "-";





   function farthest (Self : in Quaternion;   qd : in Quaternion) return Quaternion
   is
      diff : constant Quaternion := Self - qd;
      sum  : constant Quaternion := Self + qd;
   begin
      if dot (diff, diff)  >  dot (sum, sum) then
         return qd;
      end if;

      return -qd;
   end farthest;



   function nearest (Self : in Quaternion;   qd : in Quaternion) return Quaternion
   is
      diff : constant Quaternion := Self - qd;
      sum  : constant Quaternion := Self + qd;
   begin
      if dot (diff, diff)  <  dot (sum, sum) then
         return qd;
      end if;

      return -qd;
   end nearest;




   function slerp  (Self : in Quaternion;   q : in Quaternion;
                                            t : in Real  ) return Quaternion
   is
      use math.Functions;

      theta  : constant Real := angle (Self, q);

      d,
      s0, s1 : Real;
   begin
      if theta /= 0.0 then
         d  := 1.0 / Sin (theta);
         s0 := Sin ((1.0 - t) * theta);
         s1 := Sin (t * theta);

         if dot (Self, q) < 0.0 then   -- Take care of long angle case see http://en.wikipedia.org/wiki/Slerp
            return (V => ((Self.V (1) * s0  +  (-x (q) * s1))  * d,
                          (Self.V (2) * s0  +  (-y (q) * s1))  * d,
                          (Self.V (3) * s0  +  (-z (q) * s1))  * d),
                    R => ( Self.R     * s0  +  (-q.R   * s1))  * d );
         else
            return (V => ((Self.V (1) * s0  +  x (q) * s1)  * d,
                          (Self.V (2) * s0  +  y (q) * s1)  * d,
                          (Self.V (3) * s0  +  z (q) * s1)  * d),
                    R => ( Self.R     * s0  +  q.R   * s1)  * d );
         end if;
      else
         return Self;
      end if;
   end slerp;




   function getIdentity return Quaternion
   is
   begin
      return (V => (0.0, 0.0, 0.0),
              R => 1.0);

   end getIdentity;





   function "*" (Left : in Quaternion;   Right : in Vector_3)   return Quaternion
   is
      use linear_Algebra_3d;
   begin
      return linear_Algebra_3d."*" (Left, Right);
--        return (w (Left) * x (Right)  +  y (Left) * z (Right)  -  z (Left) * y (Right),
--                   w (Left) * y (Right)  +  z (Left) * x (Right)  -  x (Left) * z (Right),
--                   w (Left) * z (Right)  +  x (Left) * y (Right)  -  y (Left) * x (Right),
--                  -x (Left) * x (Right)  -  y (Left) * y (Right)  -  z (Left) * z (Right));
   end "*";




   function "*" (Left : in Vector_3;     Right : in Quaternion) return Quaternion
   is
      use impact.d3.Vector;
   begin
      return linear_Algebra_3d."*" (Left, Right);
--        return (x (Left) * w (Right)  +  y (Left) * z (Right)  -  z (Left) * y (Right),
--                 y (Left) * w (Right)  +  z (Left) * x (Right)  -  x (Left) * z (Right),
--                 z (Left) * w (Right)  +  x (Left) * y (Right)  -  y (Left) * x (Right),
--                -x (Left) * x (Right)  -  y (Left) * y (Right)  -  z (Left) * z (Right));
   end;






   function quatRotate (rotation : in Quaternion;   v : in Vector_3) return Vector_3
   is
      q : Quaternion := rotation * v;
   begin
      q := multiply (q,  inverse (rotation));

      return (X (q), Y (q), Z (q));
   end quatRotate;




   function shortestArcQuat (v1, v2 : in Vector_3) return Quaternion
   is
      use impact.d3.Scalar, impact.d3.Vector, math.Functions;
      c : constant Vector_3 := cross (v1, v2);
      d : constant Real     := dot   (v1, v2);

      n,
      unused : Vector_3;

      s, rs  : Real;
   begin
      if d  <  -1.0 + SIMD_EPSILON then
         btPlaneSpace1 (v1, n, unused);
         return (V => (x (n),  y (n),  z (n)),
                 R => 0.0);    -- just pick any vector that is orthogonal to v1
      end if;

      s  := sqRt ((1.0 + d) * 2.0);
      rs := 1.0 / s;

      return (V => (X (c) * rs,  Y (c) * rs,  Z (c) * rs),
              R => s * 0.5);
   end shortestArcQuat;





   function shortestArcQuatNormalize2 (v1, v2 : access Vector_3) return Quaternion
   is
      use impact.d3.Scalar, impact.d3.Vector, math.Functions;
   begin
      normalize (v1.all);
      normalize (v2.all);

      return shortestArcQuat (v1.all, v2.all);
   end shortestArcQuatNormalize2;



end impact.d3.Quaternions;
