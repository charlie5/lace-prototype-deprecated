with impact.d3.Scalar,
     impact.d3.min_max;



package impact.d3.Vector
--
-- Vector_3 in Bullet can be used to represent 3D points and vectors.
--
is
   use Math;



   --- Vector_3
   --

   function x (Self : in Vector_3) return Real;
   function y (Self : in Vector_3) return Real;
   function z (Self : in Vector_3) return Real;


   function dot   (Left, Right : in Vector_3) return Real;
   function cross (Left, Right : in Vector_3) return Vector_3;

   function length2 (Self : in Vector_3) return Real;            -- Return the length of the vector squared.
   function length  (Self : in Vector_3) return Real;            -- Return the length of the vector.


                                                                            -- These symantically treat the vector like a point.
   function distance2 (Left, Right : in Vector_3) return Real;            -- Return the distance squared between the ends of this and another vector.
   function distance  (Left, Right : in Vector_3) return Real;            -- Return the distance between the ends of this and another vector.


   function  safeNormalize (Self : access Vector_3) return Vector_3;
   function  Normalize     (Self : access Vector_3) return Vector_3;
   function  Normalized    (Self : in     Vector_3) return Vector_3;
   procedure Normalize     (Self : in out Vector_3);

   function absolute (Self : in Vector_3) return Vector_3;   -- Return a vector will the absolute values of each element.


   function minAxis (Self : in Vector_3) return math.Index;   -- Return the axis with the smallest value.
   function maxAxis (Self : in Vector_3) return math.Index;   -- Return the axis with the largest value.

   function rotate (Self : in Vector_3;   wAxis : in Vector_3;                    -- The axis to rotate about.
                                          Angle : in Real  )                    -- The angle to rotate by.
                                                               return Vector_3;
   --
   -- Return a rotated version of Self.


   function Angle  (Left, Right : in Vector_3) return Real;   -- Return the angle  between Left and Right.
   function Triple (V1, V2, V3  : in Vector_3) return Real;   -- Return the triple between V1, V2 and V3.


   function furthestAxis (Self : in Vector_3) return math.Index;
   function closestAxis  (Self : in Vector_3) return math.Index;


   procedure setInterpolate3 (Self : in out Vector_3;   V1, V2 : in Vector_3;
                                                        rt     : in Real);

   function lerp (V1, V2 : in Vector_3;   t : in Real) return Vector_3;
   --
   -- Return the linear interpolation between V1 and V2.
   -- 't' is the ration of V1 to V2 (t = 0 => return V1,
   --                                t = 1 => return V2)


--     function "*" (L : in math.Vector_3;   R : in math.Real) return math.Vector_3;



   function Scale (Self : access Vector_3;   By : in Vector_3) return Vector_3;   -- nb: this replaces   impact.d3.Vector& btVector::operator*=(const impact.d3.Vector& v)
                                                                                  --               and   operator*(const impact.d3.Vector& v1, const impact.d3.Vector& v2)
                                                                                  --     beware of using default Ada '*' for vectors, by mistake !
   --
   -- Elementwise multiply of Self by another vector.

   function    Scaled (Self : in    Vector_3;   By : in Vector_3) return Vector_3;
   function invScaled (Self : in    Vector_3;   By : in Vector_3) return Vector_3;



   procedure setMax (Self : in out Vector_3;   Other : in Vector_3);       -- Set each element to the max of the current values of 'Self' and the values of 'Other'.
   procedure setMin (Self : in out Vector_3;   Other : in Vector_3);       -- Set each element to the min of the current values of 'Self' and the values of 'Other'.


   procedure getSkewSymmetricMatrix (Self : in Vector_3;   V1, V2, V3 : out Vector_3);


   procedure setZero (Self : out Vector_3);


   function  isZero    (Self : in Vector_3) return Boolean;
   function  fuzzyZero (Self : in Vector_3) return Boolean;


   procedure btPlaneSpace1 (n : in Vector_3;   p, q : out Vector_3);





   --- Vector_4
   --

   function absolute4 (Self : in Vector_4) return Vector_4;   -- Return a vector will the absolute values of each element.

   function minAxis4 (Self : in Vector_4) return math.Index;   -- Return the axis with the smallest value.
   function maxAxis4 (Self : in Vector_4) return math.Index;   -- Return the axis with the largest value.

   function closestAxis4  (Self : in Vector_4) return math.Index;



end impact.d3.Vector;
