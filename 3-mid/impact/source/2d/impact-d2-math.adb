package body impact.d2.Math
is

   procedure dummy is begin null; end dummy;


   function b2IsValid (x : in float32) return Boolean
   is
   begin
      if x /= x then
         return False;                        -- NaN.
      end if;

      return    -float32'Last < x
        and then x            < float32'Last;
   end b2IsValid;



   function b2InvSqrt (x : in float32) return float32
   is
      use Interfaces;

      type Kind_type is (float, int);

      type convert_union (Kind : Kind_type := float) is
         record
            case Kind is
               when float => x : float32;
               when int   => i : interfaces.Unsigned_32;
            end case;
         end record;
      pragma Unchecked_Union (convert_union);

      convert : convert_union;
      Result  : float32      := x;

      xhalf : constant float32 := 0.5 * x;
   begin
      convert.x := x;
      convert.i := 16#5f3759df# - shift_Right (convert.i, 1);   -- tbd: check this for 64 bit builds.

      Result := convert.x;
      Result := Result * (1.5 - xhalf * Result * Result);

      return Result;
   end b2InvSqrt;



   --  A 2D column vector.
   --

   procedure setZero (Self : in out b2Vec2)
   is
   begin
      Self := (0.0, 0.0);
   end setZero;


   function  "-" (Self : in b2Vec2) return b2Vec2
   is
   begin
      return (-Self.x, -Self.y);
   end;



   function  Element (Self : in b2Vec2;   i : in int32) return float32
   is
   begin
      case i is
         when 0      => return Self.x;
         when 1      => return Self.y;
         when others => raise Constraint_Error with "Illegal index" & int32'Image (i) & " for b2Vec2";
      end case;
   end Element;



   procedure set_Element (Self : in out b2Vec2;   i : in int32;   To : in float32)
   is
   begin
      case i is
         when 0      => Self.x := To;
         when 1      => Self.y := To;
         when others => raise Constraint_Error with "Illegal index" & int32'Image (i) & " for b2Vec2";
      end case;
   end set_Element;



   function  "+" (Left, Right : b2Vec2) return b2Vec2
   is
   begin
      return (x => Left.x + Right.x,
              y => Left.y + Right.y);
   end;



   function  "-" (Left, Right : b2Vec2) return b2Vec2
   is
   begin
      return (x => Left.x - Right.x,
              y => Left.y - Right.y);
   end;



   function  "*" (Left : b2Vec2;   Right : in float32) return b2Vec2
   is
   begin
      return (x => Left.x * Right,
              y => Left.y * Right);
   end;




   function  Length        (Self : in b2Vec2) return float32
   is
      use float_math.Functions;
   begin
      return SqRt (Self.x * Self.x  +  Self.y * Self.y);
   end Length;



   function  LengthSquared (Self : in b2Vec2) return float32
   is
   begin
      return Self.x * Self.x  +  Self.y * Self.y;
   end LengthSquared;



   function  Normalize (Self : access b2Vec2) return float32
   is
      the_length : constant float32 := Length (Self.all);
      invlength  : float32;
   begin
      if the_length < b2_epsilon then
         return 0.0;
      end if;

      invLength := 1.0 / the_length;

      Self.x := Self.x * invLength;
      Self.y := Self.y * invLength;

      return the_length;
   end Normalize;



   procedure Normalize (Self : in out b2Vec2)
   is
      the_length : constant float32 := Length (Self);
      invlength  : float32;
   begin
      if the_length < b2_epsilon then
         return;
      end if;

      invLength := 1.0 / the_length;

      Self.x := Self.x * invLength;
      Self.y := Self.y * invLength;
   end Normalize;



   function   Normalize (Self : in     b2Vec2) return b2Vec2
   is
      Result : b2Vec2 := Self;
   begin
      normalize (Result);
      return Result;
   end Normalize;




   function isValid (Self : in b2Vec2) return Boolean
   is
   begin
      return b2IsValid (Self.x) and then b2IsValid (Self.y);
   end isValid;






   --  A 2D column vector with 3 elements.
   --

   procedure setZero (Self : in out b2Vec3)
   is
   begin
      Self := (0.0, 0.0, 0.0);
   end setZero;



   function  "-" (Self : in b2Vec3) return b2Vec3
   is
   begin
      return (-Self.x, -Self.y, -Self.z);
   end;



   function  "+" (Left, Right : b2Vec3) return b2Vec3
   is
   begin
      return (x => Left.x + Right.x,
              y => Left.y + Right.y,
              z => Left.z + Right.z);
   end;



   function  "-" (Left, Right : b2Vec3) return b2Vec3
   is
   begin
      return (x => Left.x - Right.x,
              y => Left.y - Right.y,
              z => Left.z - Right.z);
   end;



   function "*" (Left : in float32;   Right : in b2Vec3) return b2Vec3
   is
   begin
      return (x => Left * Right.x,
              y => Left * Right.y,
              z => Left * Right.z);
   end;



   function  "*" (Left : b2Vec3;   Right : in float32) return b2Vec3
   is
   begin
      return (x => Left.x * Right,
              y => Left.y * Right,
              z => Left.z * Right);
   end;




   --- A 2-by-2 matrix. Stored in column-major order.
   --

   function  to_b2Mat22 (col1, col2         : in b2Vec2) return b2Mat22
   is
   begin
      return (col1, col2);
   end to_b2Mat22;



   function  to_b2Mat22 (a11, a12, a21, a22 : in float32) return b2Mat22
   is
   begin
      return (col1 => (x => a11, y => a21),
              col2 => (x => a12, y => a22));

   end to_b2Mat22;



   function  to_b2Mat22 (angle              : in float32) return b2Mat22
   is
      use float_math.Functions;
      c : constant float32 := cos (angle);
      s : constant float32 := sin (angle);
   begin
      return (col1 => (x =>  c,  y => s),
              col2 => (x => -s,  y => c));
   end to_b2Mat22;



   procedure set         (Self : in out b2Mat22;   angle : in float32)
   is
   begin
      Self := to_b2Mat22 (angle);
   end set;


   procedure setIdentity (Self : in out b2Mat22)
   is
   begin
      Self := (col1 => (x => 1.0,  y => 0.0),
               col2 => (x => 0.0, y => 1.0));
   end setIdentity;



   procedure setZero     (Self : in out b2Mat22)
   is
   begin
      Self := ((0.0, 0.0), (0.0, 0.0));
   end setZero;



   function  getAngle    (Self : in     b2Mat22) return float32
   is
      use float_math.Functions;
   begin
      return arcTan (self.col1.y,  self.col1.x);
   end getAngle;



   function  getInverse  (Self : in     b2Mat22) return b2Mat22
   is
      a   : constant float32 := Self.col1.x;
      b   : constant float32 := Self.col2.x;
      c   : constant float32 := Self.col1.y;
      d   : constant float32 := Self.col2.y;

      det    : float32 := a * d - b * c;
      Result : b2Mat22;
   begin
      if det /= 0.0 then
         det := 1.0 / det;
      end if;

      Result.col1.x :=  det * d;        Result.col2.x := -det * b;
      Result.col1.y := -det * c;        Result.col2.y :=  det * a;

      return Result;
   end getInverse;




   function solve        (Self : in     b2Mat22;   b : in b2Vec2) return b2Vec2
   is
      a11 : constant float32 := Self.col1.x;
      a12 : constant float32 := Self.col2.x;
      a21 : constant float32 := Self.col1.y;
      a22 : constant float32 := Self.col2.y;

      det : float32 := a11 * a22 - a12 * a21;

      X   : b2Vec2;
   begin
      if det /= 0.0 then
         det := 1.0 / det;
      end if;

      X.x := det * (a22 * b.x - a12 * b.y);
      X.y := det * (a11 * b.y - a21 * b.x);

      return X;
   end solve;





   --  A 3-by-3 matrix. Stored in column-major order.
   --

   function  to_b2Mat33 (col1, col2, col3   : in b2Vec3) return b2Mat33
   is
   begin
      return (col1, col2, col3);
   end to_b2Mat33;


   procedure setZero (Self : in out b2Mat33)
   is
   begin
      Self := (others => (others => 0.0));
   end setZero;


   function  solve   (Self : in     b2Mat33;   b : in b2Vec3) return b2Vec3
   is
      det : float32 := b2Dot (Self.col1,  b2Cross (Self.col2, Self.col3));
      X   : b2Vec3;
   begin
      if det /= 0.0 then
         det := 1.0 / det;
      end if;

      X.x := det * b2Dot (b,          b2Cross (Self.col2, Self.col3));
      X.y := det * b2Dot (Self.col1,  b2Cross (b,         Self.col3));
      X.z := det * b2Dot (Self.col1,  b2Cross (Self.col2, b      ));

      return X;
   end solve;





   function  solve   (Self : in     b2Mat33;   b : in b2Vec2) return b2Vec2
   is
      a11 : constant float32 := Self.col1.x;
      a12 : constant float32 := Self.col2.x;
      a21 : constant float32 := Self.col1.y;
      a22 : constant float32 := Self.col2.y;

      det : float32 := a11 * a22 - a12 * a21;

      X   : b2Vec2;
   begin
      if det /= 0.0 then
         det := 1.0 / det;
      end if;

      X.x := det * (a22 * b.x - a12 * b.y);
      X.y := det * (a11 * b.y - a21 * b.x);

      return X;
   end solve;








   --- b2Transform
   --
   function  to_btTransform (position : in b2Vec2;   R : in b2Mat22) return b2Transform
   is
   begin
      return (position, R);
   end to_btTransform;


   procedure setIdentity (Self : in out b2Transform)
   is
   begin
      self.position := (0.0, 0.0);
      setIdentity (self.R);
   end setIdentity;



   procedure set         (Self : in out b2Transform;   p     : in b2Vec2;
                                                       angle : in float32)
   is
   begin
      Self.position := p;
      set (Self.R, angle);
   end set;



   function  getAngle    (Self : in b2Transform) return float32
   is
      use float_math.Functions;
   begin
      return arcTan (Self.R.col1.y,  Self.R.col1.x);
   end getAngle;




   --- b2Sweep
   --

   procedure getTransform (Self : in b2Sweep;   xf    : access b2Transform;
                                                alpha : in     float32)
   is
      angle : constant float32 := (1.0 - alpha) * Self.a0  +  alpha * Self.a;
   begin
      xf.position := (1.0 - alpha) * Self.c0  +  alpha * Self.c;
      set (xf.R, angle);

      xf.position := xf.position - b2Mul (xf.R, Self.localCenter);    -- Shift to origin
   end getTransform;



   procedure advance (Self : in out b2Sweep;   t : in float32)
   is
   begin
      Self.c0 := (1.0 - t) * Self.c0  +  t * Self.c;
      Self.a0 := (1.0 - t) * Self.a0  +  t * Self.a;
   end advance;



   procedure normalize (Self : in out b2Sweep)
   is
      twoPi : constant float32 := 2.0 * b2_pi;
      d     : constant float32 := twoPi * float32'Floor (Self.a0 / twoPi);
   begin
      Self.a0 := Self.a0 - d;
      Self.a  := Self.a  - d;
   end normalize;










   function b2Dot   (a, b : in b2Vec2) return float32
   is
   begin
      return a.x * b.x  +  a.y * b.y;
   end b2Dot;



   function b2Cross (a : in b2Vec2;    b : in b2Vec2) return float32
   is
   begin
      return a.x * b.y - a.y * b.x;
   end b2Cross;



   function b2Cross (a : in b2Vec2;    s : in float32) return b2Vec2
   is
   begin
      return (s * a.y,  -s * a.x);
   end b2Cross;



   function b2Cross (s : in float32;   a : in b2Vec2) return b2Vec2
   is
   begin
      return (-s * a.y,  s * a.x);
   end b2Cross;







   function b2Mul (A : in b2Mat22;   v : in b2Vec2) return b2Vec2
   is
   begin
      return (A.col1.x * v.x + A.col2.x * v.y,
              A.col1.y * v.x + A.col2.y * v.y);
   end b2Mul;





   function b2MulT (A : in b2Mat22;   v : in b2Vec2) return b2Vec2
   is
   begin
      return (b2Dot (v, A.col1),
              b2Dot (v, A.col2));
   end b2MulT;




   function "*" (Left : in float32;   Right : in b2Vec2) return b2Vec2
   is
   begin
      return (Left * Right.x,  Left * Right.y);
   end;




   function  b2Distance        (a, b : in b2Vec2) return float32
   is
      c : constant b2Vec2 := a - b;
   begin
      return Length (c);
   end b2Distance;



   function  b2DistanceSquared (a, b : in b2Vec2) return float32
   is
      c : constant b2Vec2 := a - b;
   begin
      return b2Dot (c, c);
   end b2DistanceSquared;









   function b2Dot (a, b : in b2Vec3) return float32
   is
   begin
      return a.x * b.x  +  a.y * b.y  +  a.z * b.z;
   end b2Dot;



   function b2Cross (a, b : in b2Vec3) return b2Vec3
   is
   begin
      return (a.y * b.z  -  a.z * b.y,
              a.z * b.x  -  a.x * b.z,
              a.x * b.y  -  a.y * b.x);
   end b2Cross;








   function "+" (Left, Right : in b2Mat22) return b2Mat22
   is
   begin
      return (Left.col1 + Right.col1,
              Left.col2 + Right.col2);
   end;



   function b2Mul  (A, B : in b2Mat22) return b2Mat22
   is
   begin
      return (b2Mul (A, B.col1),
              b2Mul (A, B.col2));
   end b2Mul;




   function b2MulT (A, B : in b2Mat22) return b2Mat22
   is
      c1 : constant b2Vec2 := (b2Dot (A.col1, B.col1),  b2Dot (A.col2, B.col1));
      c2 : constant b2Vec2 := (b2Dot (A.col1, B.col2),  b2Dot (A.col2, B.col2));
   begin
      return (c1, c2);
   end b2MulT;




   function b2Mul (A : in b2Mat33;       v : in b2Vec3) return b2Vec3
   is
   begin
      return    v.x * A.col1
             +  v.y * A.col2
             +  v.z * A.col3;
   end b2Mul;



   function b2Mul  (T : in b2Transform;   v : in b2Vec2) return b2Vec2
   is
      x : constant float32 := T.position.x + T.R.col1.x * v.x + T.R.col2.x * v.y;
      y : constant float32 := T.position.y + T.R.col1.y * v.x + T.R.col2.y * v.y;
   begin
      return (x, y);
   end b2Mul;




   function b2MulT (T : in b2Transform;   v : in b2Vec2) return b2Vec2
   is
   begin
      return b2MulT (T.R,  v - T.position);
   end b2MulT;



   function b2Abs (Self : in b2Vec2) return b2Vec2
   is
   begin
      return (abs (Self.x),
              abs (Self.y));
   end b2Abs;



   function b2Abs (Self : in b2Mat22) return b2Mat22
   is
   begin
      return (b2Abs (Self.col1),
              b2Abs (Self.col2));
   end b2Abs;




   function b2Min (a, b : in b2Vec2) return b2Vec2
   is
   begin
      return (float32'Min (a.x, b.x),
              float32'Min (a.y, b.y));
   end b2Min;


   function b2Max (a, b : in b2Vec2) return b2Vec2
   is
   begin
      return (float32'Max (a.x, b.x),
              float32'Max (a.y, b.y));
   end b2Max;





   function b2Clamp (a : in float32;   low, high : in float32) return float32
   is
   begin
      return float32'Max (low, float32'Min (a, high));
   end b2Clamp;



   function b2Clamp (a : in b2Vec2;    low, high : in b2Vec2) return b2Vec2
   is
   begin
      return b2Max (low, b2Min (a, high));
   end b2Clamp;





   --  "Next Largest Power of 2
   --   Given a binary integer value x, the next largest power of 2 can be computed by a SWAR algorithm
   --   that recursively "folds" the upper bits into the lower bits. This process yields a bit vector with
   --   the same most significant 1 as x, but all 1's below it. Adding 1 to that value yields the next
   --   largest power of 2. For a 32-bit value:"
   --
   function b2NextPowerOfTwo (x : in uint32) return uint32
   is
      use Interfaces;
      Pad : uint32 := x;
   begin
      Pad := Pad or shift_Right (Pad, 1);
      Pad := Pad or shift_Right (Pad, 2);
      Pad := Pad or shift_Right (Pad, 4);
      Pad := Pad or shift_Right (Pad, 8);
      Pad := Pad or shift_Right (Pad, 16);

      return Pad + 1;
   end b2NextPowerOfTwo;



   function b2IsPowerOfTwo (x : in uint32) return Boolean
   is
      use type uint32;
   begin
      return      x > 0
        and then (x and (x - 1)) = 0;
   end b2IsPowerOfTwo;





end impact.d2.Math;
