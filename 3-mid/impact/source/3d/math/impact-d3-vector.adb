with impact.d3.min_max;
--  with math; --.Algebra.linear.d3;


package body impact.d3.Vector
--
--
--
is


   function x (Self : in Vector_3) return Real
   is
   begin
      return Self (1);
   end x;



   function y (Self : in Vector_3) return Real
   is
   begin
      return Self (2);
   end y;


   function z (Self : in Vector_3) return Real
   is
   begin
      return Self (3);
   end z;




   function dot (Left, Right : in Vector_3) return Real
   is
   begin
      return   Left (1) * Right (1)
             + Left (2) * Right (2)
             + Left (3) * Right (3);
   end dot;



   function cross (Left, Right : in Vector_3) return Vector_3
   is
   begin
      return (Left (2) * Right (3) - Left (3) * Right (2),
              Left (3) * Right (1) - Left (1) * Right (3),
              Left (1) * Right (2) - Left (2) * Right (1));

   end cross;





   function length2 (Self : in Vector_3) return Real
   is
   begin
      return dot (Self, Self);
   end length2;




   function length  (Self : in Vector_3) return Real
   is
      use math.Functions;
   begin
      return sqRt (length2 (Self));
   end length;




   function distance2 (Left, Right : in Vector_3) return Real
   is
   begin
      return length2 (Right - Left);
   end distance2;



   function distance  (Left, Right : in Vector_3) return Real
   is
   begin
      return length (Right - Left);
   end distance;




   function safeNormalize (Self : access Vector_3) return Vector_3
   is
      absVec   : Vector_3   := absolute (Self.all);
      maxIndex : constant math.Index := maxAxis (absVec);
   begin
      if absVec (maxIndex) > 0.0 then
         Self.all := Self.all / absVec (maxIndex);
         Self.all := Self.all / length (Self.all);
         return Self.all;
      end if;

      Self.all := (1.0, 0.0, 0.0);
      return Self.all;
   end safeNormalize;



   function Normalize (Self : access Vector_3) return Vector_3
   is
   begin
      Self.all := Self.all / length (Self.all);
      return Self.all;
   end Normalize;




   function Normalized    (Self : in     Vector_3) return Vector_3
   is
   begin
      return Self / length (Self);
   end Normalized;



   procedure Normalize     (Self : in out Vector_3)
   is
   begin
      Self := Normalized (Self);
   end Normalize;






   function absolute (Self : in Vector_3) return Vector_3
   is
   begin
      return abs (Self);
   end absolute;





   function minAxis (Self : in Vector_3) return math.Index
   is
   begin
      if Self (1) < Self (2) then
         if Self (1) < Self (3) then
            return 1;
         else
            return 3;
         end if;

      elsif Self (2) < Self (3) then
         return 2;

      else
         return 3;
      end if;
   end minAxis;



   function maxAxis (Self : in Vector_3) return math.Index
   is
   begin
      if Self (1) < Self (2) then
         if Self (2) < Self (3) then
            return 3;
         else
            return 2;
         end if;

      elsif Self (1) < Self (3) then
         return 3;

      else
         return 1;
      end if;
   end maxAxis;






   function rotate (Self : in Vector_3;   wAxis : in Vector_3;
                                          Angle : in Real ) return Vector_3
   is
      use math.Functions;
      o : constant Vector_3 := wAxis * dot (wAxis, Self);   -- wAxis must be a unit lenght vector
      x : constant Vector_3 := Self - o;
      y : constant Vector_3 := cross (wAxis, Self);
   begin
      return o  +  x * Cos (angle)  +  y * Sin (angle);
   end rotate;




   function Angle (Left, Right : in Vector_3) return Real
   is
      use math.Functions;
      s : constant Real := sqRt (length2 (Left) * length2 (Right));
   begin
      pragma Assert (s /= 0.0);
      return arcCos (dot (Left, Right) / s);
   end Angle;




   function Triple (V1, V2, V3  : in Vector_3) return Real
   is
   begin
      return   V1 (1)  *  (V2 (2) * V3 (3) - V2 (3) * V3 (2))
             + V1 (2)  *  (V2 (3) * V3 (1) - V2 (1) * V3 (3))
             + V1 (3)  *  (V2 (1) * V3 (2) - V2 (2) * V3 (1));
   end Triple;




   function furthestAxis (Self : in Vector_3) return math.Index
   is
   begin
      return minAxis (absolute (Self));
   end furthestAxis;


   function closestAxis (Self : in Vector_3) return math.Index
   is
   begin
      return maxAxis (absolute (Self));
   end closestAxis;





   procedure setInterpolate3 (Self : in out Vector_3;   V1, V2 : in Vector_3;
                                                        rt     : in Real)
   is
      s : constant Real := 1.0 - rt;
   begin
      Self (1) := s * V1 (1)  +  rt * V2 (1);
      Self (2) := s * V1 (2)  +  rt * V2 (2);
      Self (3) := s * V1 (3)  +  rt * V2 (3);

      --  don't do the unused w component
      --  m_co[3] = s * v0[3] + rt * v1[3];
   end setInterpolate3;




   function lerp (V1, V2 : in Vector_3;   t : in Real) return Vector_3
   is
   begin
      return (V1 (1)  +  (V2 (1) - V1 (1)) * t,
              V1 (2)  +  (V2 (2) - V1 (2)) * t,
              V1 (3)  +  (V2 (3) - V1 (3)) * t);
   end lerp;



   function Scaled (Self : in    Vector_3;   By : in Vector_3) return Vector_3
   is
   begin
      return (Self (1) * By (1),
              Self (2) * By (2),
              Self (3) * By (3));
   end Scaled;



   function invScaled (Self : in    Vector_3;   By : in Vector_3) return Vector_3
   is
   begin
      return (Self (1) / By (1),
              Self (2) / By (2),
              Self (3) / By (3));
   end invScaled;





--     function "*" (L : in math.Vector_3;   R : in math.Real) return math.Vector_3
--     is
--     begin
--        return (L (1) * R,
--                L (2) * R,
--                L (3) * R);
--     end;






   function Scale (Self : access Vector_3;   By : in Vector_3) return Vector_3
   is
   begin
      Self.all := Scaled (Self.all, By);
      return Self.all;
   end Scale;




   procedure setMax (Self : in out Vector_3;   Other : in Vector_3)
   is
   begin
      Self := (Real'Max (Self (1), Other (1)),
               Real'Max (Self (2), Other (2)),
               Real'Max (Self (3), Other (3)));
   end setMax;




   procedure setMin (Self : in out Vector_3;   Other : in Vector_3)
   is
   begin
      Self := (Real'Min (Self (1), Other (1)),
               Real'Min (Self (2), Other (2)),
               Real'Min (Self (3), Other (3)));
   end setMin;





   procedure getSkewSymmetricMatrix (Self : in Vector_3;   V1, V2, V3 : out Vector_3)
   is
   begin
      v1 := (     0.0,   -Self (3),    Self (2));
      v2 := (Self (3),         0.0,   -Self (1));
      v3 := (-Self (2),    Self (1),         0.0);
   end getSkewSymmetricMatrix;




   procedure setZero (Self : out Vector_3)
   is
   begin
      Self := (0.0, 0.0, 0.0);
   end setZero;




   function  isZero (Self : in Vector_3) return Boolean
   is
   begin
      return Self (1) = 0.0  and then  Self (2) = 0.0  and then  Self (3) = 0.0;
   end isZero;




   function  fuzzyZero (Self : in Vector_3) return Boolean
   is
      use impact.d3.Scalar;
   begin
      return length2 (Self) < SIMD_EPSILON;
   end fuzzyZero;






   --- Vector_4
   --

   function absolute4 (Self : in Vector_4) return Vector_4
   is
      use math.Vectors;
   begin
      return abs (Self);
   end absolute4;




   function maxAxis4 (Self : in Vector_4) return math.Index
   is
      maxIndex : math.Index := -1;
      maxVal   : Real     := Real'First;   -- -BT_LARGE_FLOAT;

   begin
      if Self (1) > maxVal then
         maxIndex := 1;
         maxVal   := Self (1);
      end if;

      if Self (2) > maxVal then
         maxIndex := 2;
         maxVal   := Self (2);
      end if;

      if Self (3) > maxVal then
         maxIndex := 3;
         maxVal   := Self (3);
      end if;

      if Self (4) > maxVal then
         maxIndex := 4;
         maxVal   := Self (4);
      end if;


      return maxIndex;
   end maxAxis4;





   function minAxis4 (Self : in Vector_4) return math.Index
   is
      minIndex : math.Index := -1;
      minVal   : Real     := Real'Last;   -- BT_LARGE_FLOAT;

   begin
      if Self (1) < minVal then
         minIndex := 1;
         minVal   := Self (1);
      end if;

      if Self (2) < minVal then
         minIndex := 2;
         minVal   := Self (2);
      end if;

      if Self (3) < minVal then
         minIndex := 3;
         minVal   := Self (3);
      end if;

      if Self (4) < minVal then
         minIndex := 4;
         minVal   := Self (4);
      end if;


      return minIndex;
   end minAxis4;



   function closestAxis4  (Self : in Vector_4) return math.Index
   is
   begin
      return maxAxis4 (absolute4 (Self));
   end closestAxis4;



   procedure btPlaneSpace1 (n : in Vector_3;   p, q : out Vector_3)
   is
      use impact.d3.Scalar;
      a, k : Real;
   begin
      if abs n (3) > SIMDSQRT12 then   -- choose p in y-z plane
         a := n (2) * n (2)  +  n (3) * n (3);
         k := btRecipSqrt (a);

         p (1) := 0.0;
         p (2) := -n (3) * k;
         p (3) :=  n (2) * k;
         --  set q = n x p
         q (1) :=      a * k;
         q (2) := -n (1) * p (3);
         q (3) :=  n (1) * p (2);
      else                                -- choose p in x-y plane
         a := n (1) * n (1)  +  n (2) * n (2);
         k := btRecipSqrt (a);

         p (1) := -n (2) * k;
         p (2) :=  n (1) * k;
         p (3) := 0.0;
         --  set q = n x p
         q (1) := -n (3) * p (2);
         q (2) :=  n (3) * p (1);
         q (3) :=      a * k;
      end if;
   end btPlaneSpace1;



end impact.d3.Vector;
