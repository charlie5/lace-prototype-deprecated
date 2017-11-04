generic
package any_math.any_Algebra.any_linear
is

   pragma Pure;


   ----------
   --  Vector
   --

   function  Norm_squared (Self : in     Vector)   return Real;     -- Length squared.

   function  Normalised   (Self : in     Vector)   return Vector;
   procedure Normalise    (Self : in out Vector);

   function  Normalised   (Self : in     Vector_2) return Vector_2;
   procedure Normalise    (Self : in out Vector_2);

   function  Normalised   (Self : in     Vector_3) return Vector_3;
   procedure Normalise    (Self : in out Vector_3);

   function  Min          (Left : in     Vector;   Right : in Vector) return Vector;
   function  Max          (Left : in     Vector;   Right : in Vector) return Vector;

   function  Scaled       (Self : in     Vector;   By    : in Vector) return Vector;


   ----------
   --  Matrix
   --

   function  to_Matrix    (Row_1,
                           Row_2,
                           Row_3 : in     Vector_3)     return Matrix_3x3;

   function  Identity     (Size  : in     Index   := 3) return Matrix;

   function  Min          (Self  : in     Matrix)       return Real;
   function  Max          (Self  : in     Matrix)       return Real;

   function  Image        (Self  : in     Matrix) return String;
   procedure invert       (Self  : in out Matrix);

   function  sub_Matrix   (Self  : in     Matrix;   start_Row, end_Row : in Index;
                                                    start_Col, end_Col : in Index) return Matrix;


   ---------------
   --  Quaternion
   --

   function to_Quaternion (aX, aY, aZ : in     Real;
                           Angle      : in     Real) return Quaternion;
   --
   --  Returns a quaternion defined by a rotation about an axis.


   function "*"           (Self : in     Quaternion;   By    : in    Quaternion) return Quaternion;
   --
   --  Grassmann product.

   function  Unit         (Self : in     Quaternion) return Quaternion;

   function  Conjugate    (Self : in     Quaternion) return Quaternion;
   --
   --  tbd: only for unit quaternions.

   function  euler_Angles (Self : in     Quaternion) return Vector_3;


   function infinitesimal_Rotation_from
                          (Self : in     Quaternion;   angular_Velocity : in Vector_3) return Quaternion;
   --
   --  An infinitesimal_Rotation may be multiplied by a duration and then added to the original Attitude
   --  to produce the attitude at the given time.


   function  Normalised   (Self : in     Quaternion) return Quaternion;
   procedure normalise    (Self : in out Quaternion);




private

   pragma Inline ("*");

   pragma Inline_Always (Norm_squared);
   pragma Inline_Always (Normalise);

end any_math.any_Algebra.any_linear;
