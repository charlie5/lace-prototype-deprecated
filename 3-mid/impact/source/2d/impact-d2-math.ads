package impact.d2.Math
--
--  Provides math subprograms.
--
is
   pragma Pure;


   function b2IsValid (x : in float32) return Boolean;   -- This function is used to ensure that a floating point number is not a NaN or infinity.
   function b2InvSqrt (x : in float32) return float32;   -- This is a approximate yet fast inverse square-root.



   --  A 2D column vector.
   --
   type b2Vec2 is
      record
         x, y : float32;
      end record;

   type b2Vec2_array is array (int32 range <>) of aliased b2Vec2;


   procedure setZero (Self : in out b2Vec2);

   function  "-" (Self : in b2Vec2) return b2Vec2;

   function      Element (Self : in     b2Vec2;   i : in int32)    return float32;    -- Read from an indexed element.
   procedure set_Element (Self : in out b2Vec2;   i : in int32;   To : in float32);   -- Write to  an indexed element.

   function "+" (Left, Right : in b2Vec2) return b2Vec2;                              -- Add two vectors component-wise.
   function "-" (Left, Right : in b2Vec2) return b2Vec2;                              -- Subtract two vectors component-wise.

   function  "*" (Left : in b2Vec2;    Right : in float32) return b2Vec2;
   function  "*" (Left : in float32;   Right : in b2Vec2) return b2Vec2;

   function  b2Distance        (a, b : in b2Vec2) return float32;
   function  b2DistanceSquared (a, b : in b2Vec2) return float32;



   function  Length        (Self : in b2Vec2) return float32;     -- Get the length of this vector (the norm).
   function  LengthSquared (Self : in b2Vec2) return float32;     -- Get the length squared. For performance, use this instead of Length (if possible).


   function   Normalize (Self : in     b2Vec2) return b2Vec2;    -- Convert this vector into a unit vector. Returns the length.
   function   Normalize (Self : access b2Vec2) return float32;    -- Convert this vector into a unit vector. Returns the length.
   procedure  Normalize (Self : in out b2Vec2);                   -- Convert this vector into a unit vector.

   function isValid (Self : in b2Vec2) return Boolean;           -- Does this vector contain finite coordinates ?



   --  A 2D column vector with 3 elements.
   --
   type b2Vec3 is
      record
         x, y, z : float32;
      end record;


   procedure setZero (Self : in out b2Vec3);

   function  "-" (Self : in b2Vec3) return b2Vec3;

   function  "+" (Left, Right : in b2Vec3) return b2Vec3;          -- Add      two vectors component-wise.
   function  "-" (Left, Right : in b2Vec3) return b2Vec3;          -- Subtract two vectors component-wise.

   function  "*" (Left : in b2Vec3;    Right : in float32) return b2Vec3;
   function  "*" (Left : in float32;   Right : in b2Vec3) return b2Vec3;


   function b2Dot   (a, b : in b2Vec3) return float32;    -- Perform the dot   product on two vectors.
   function b2Cross (a, b : in b2Vec3) return b2Vec3;     -- Perform the cross product on two vectors.





   --- A 2-by-2 matrix. Stored in column-major order.
   --
   type b2Mat22 is
      record
         col1, col2 : b2Vec2;
      end record;


   function  to_b2Mat22 (col1, col2         : in b2Vec2) return b2Mat22;    -- Construct this matrix using columns.
   function  to_b2Mat22 (a11, a12, a21, a22 : in float32) return b2Mat22;    -- Construct this matrix using scalars.
   function  to_b2Mat22 (angle              : in float32) return b2Mat22;    -- Construct this matrix using an angle.
                                                                             -- This matrix becomes an orthonormal rotation matrix.

   procedure set         (Self : in out b2Mat22;   angle : in float32);      -- Initialize this matrix using an angle. This matrix
                                                                             -- becomes an orthonormal rotation matrix.
   procedure setIdentity (Self : in out b2Mat22);                            -- Set this to the identity matrix.
   procedure setZero     (Self : in out b2Mat22);                            -- Set this matrix to all zeros.

   function  getAngle    (Self : in     b2Mat22) return float32;             -- Extract the angle from this matrix (assumed to be a rotation matrix).
   function  getInverse  (Self : in     b2Mat22) return b2Mat22;

   function  solve       (Self : in     b2Mat22;   b : in b2Vec2) return b2Vec2;   -- Solve A * x = b, where b is a column vector. This is more efficient
                                                                                   -- than computing the inverse in one-shot cases.




   --- A 3-by-3 matrix. Stored in column-major order.
   --
   type b2Mat33 is
      record
         col1, col2, col3 : b2Vec3;
      end record;


   function  to_b2Mat33 (col1, col2, col3   : in b2Vec3) return b2Mat33;      -- Construct this matrix using columns.

   procedure setZero (Self : in out b2Mat33);                                  -- Set this matrix to all zeros.

   --  Solve A * x = b, where b is a column vector. This is more efficient
   --  than computing the inverse in one-shot cases.
   --
   function  solve   (Self : in     b2Mat33;   b : in b2Vec3) return b2Vec3;
   function  solve   (Self : in     b2Mat33;   b : in b2Vec2) return b2Vec2;   -- Solve only the upper 2-by-2 matrix equation.






   --- A transform contains translation and rotation. It is used to represent
   --  the position and orientation of rigid frames.
   --
   type b2Transform is
      record
         position : b2Vec2;
         R        : b2Mat22;
      end record;


   function  to_btTransform (position : in b2Vec2;   R : in b2Mat22) return b2Transform;   -- Initialize using a position vector and a rotation matrix.

   procedure setIdentity (Self : in out b2Transform);                                      -- Set self to the identity transform.
   procedure set         (Self : in out b2Transform;   p     : in b2Vec2;                  -- Set self based on the position and angle.
                                                       angle : in float32);
   function  getAngle    (Self : in b2Transform) return float32;                           -- Calculate the angle that the rotation matrix represents.




   --- b2Sweep
   --

   type b2Sweep is tagged
      record
         localCenter : b2Vec2;                -- Local center of mass position.
         c0, c       : b2Vec2;                -- Center world positions.
         a0, a       : float32;               -- World angles.
         alpha0      : float32;               -- Fraction of the current time step in the range [0,1].
                                              -- c0 and a0 are the positions at alpha0.
      end record;
   --
   --  This describes the motion of a body/shape for TOI computation.
   --  Shapes are defined with respect to the body origin, which may
   --  no coincide with the center of mass. However, to support dynamics
   --  we must interpolate the center of mass position.


   procedure getTransform (Self : in b2Sweep;   xf    : access b2Transform;
                                                alpha : in     float32);
   --
   --  Get the interpolated transform at a specific time.
   --  'alpha' is a factor in [0,1], where 0 indicates t0.
   --  'xf'    specifies where the result is stored.

   procedure advance (Self : in out b2Sweep;   t : in float32);
   --
   --  Advance the sweep forward, yielding a new initial state.
   --  't' is the new initial time.

   procedure normalize (Self : in out b2Sweep);    -- Normalize the angles (in radians) to be between -pi and pi.






   b2Vec2_zero          : constant b2Vec2;
   b2Mat22_identity     : constant b2Mat22;
   b2Transform_identity : constant b2Transform;



   function b2Dot   (a, b : in b2Vec2) return float32;                   -- Perform the dot product on two vectors.

   function b2Cross (a : in b2Vec2;    b : in b2Vec2) return float32;   -- Perform the cross product on two vectors.           In 2D this produces a scalar.
   function b2Cross (a : in b2Vec2;    s : in float32) return b2Vec2;    -- Perform the cross product on a vector and a scalar. In 2D this produces a vector.
   function b2Cross (s : in float32;   a : in b2Vec2) return b2Vec2;    -- Perform the cross product on a scalar and a vector. In 2D this produces a vector.




   function b2Mul (A : in b2Mat22;   v : in b2Vec2) return b2Vec2;
   --
   --  Multiply a matrix times a vector. If a rotation matrix is provided,
   --  then this transforms the vector from one frame to another.




   function b2MulT (A : in b2Mat22;   v : in b2Vec2) return b2Vec2;
   --
   --  Multiply a matrix transpose times a vector. If a rotation matrix is provided,
   --  then this transforms the vector from one frame to another (inverse transform).






   function "+" (Left, Right : in b2Mat22) return b2Mat22;


   function b2Mul  (A, B : in b2Mat22) return b2Mat22;                    -- A   * B
   function b2MulT (A, B : in b2Mat22) return b2Mat22;                    -- A^T * B



   function b2Mul (A : in b2Mat33;       v : in b2Vec3) return b2Vec3;        -- Multiply a matrix times a vector.


   function b2Mul  (T : in b2Transform;   v : in b2Vec2) return b2Vec2;
   function b2MulT (T : in b2Transform;   v : in b2Vec2) return b2Vec2;

   function b2Abs (Self : in b2Vec2) return b2Vec2;
   function b2Abs (Self : in b2Mat22) return b2Mat22;





   function b2Min (a, b : in b2Vec2) return b2Vec2;
   function b2Max (a, b : in b2Vec2) return b2Vec2;

   function b2Clamp (a : in float32;   low, high : in float32) return float32;
   function b2Clamp (a : in b2Vec2;    low, high : in b2Vec2) return b2Vec2;


   procedure b2Swap is new swap_any (float32);



   function b2NextPowerOfTwo (x : in uint32) return uint32;
   --
   --   Next Largest Power of 2
   --   Given a binary integer value x, the next largest power of 2 can be computed by a SWAR algorithm
   --   that recursively "folds" the upper bits into the lower bits. This process yields a bit vector with
   --   the same most significant 1 as x, but all 1's below it. Adding 1 to that value yields the next
   --   largest power of 2. For a 32-bit value.


   function b2IsPowerOfTwo (x : in uint32) return Boolean;





private

   b2Vec2_zero          : constant b2Vec2      := (0.0, 0.0);
   b2Mat22_identity     : constant b2Mat22     := (col1 => (x => 1.0,  y => 0.0),
                                                   col2 => (x => 0.0,  y => 1.0));
   b2Transform_identity : constant b2Transform := (position => (0.0, 0.0),
                                                   R        => (col1 => (x => 1.0,  y => 0.0),
                                                                col2 => (x => 0.0,  y => 1.0)));
end impact.d2.Math;
