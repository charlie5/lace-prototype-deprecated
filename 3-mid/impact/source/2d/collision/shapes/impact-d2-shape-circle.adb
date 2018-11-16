
package body impact.d2.Shape.circle
is


   function to_Circle return b2CircleShape
   is
   begin
      return (m_kind   => e_circle,
              m_radius => 0.0,
              m_p      => (0.0, 0.0));
   end to_Circle;




   overriding function Clone        (Self : in b2CircleShape) return Shape.view
   is
   begin
      return new b2CircleShape'(Self);
   end Clone;




   overriding function TestPoint    (Self : in b2CircleShape;   transform : in b2Transform;
                                                     p  : in b2Vec2   ) return Boolean
   is
      center : constant b2Vec2 := transform.position + b2Mul (transform.R, Self.m_p);
      d      : constant b2Vec2 := p - center;
   begin
      return b2Dot (d, d) <= Self.m_radius * Self.m_radius;
   end TestPoint;




   --  Collision Detection in Interactive 3D Environments by Gino van den Bergen
   --  From Section 3.1.2
   --  x       = s + a * r
   --  norm(x) = radius
   --
   overriding function RayCast      (Self : in b2CircleShape;   output    : access collision.b2RayCastOutput;
                                                     input     : in     collision.b2RayCastInput;
                                                     transform : in     b2Transform            ) return Boolean
   is
      use float_math.Functions;
      position : constant b2Vec2 := transform.position + b2Mul (transform.R, Self.m_p);
      s        : constant b2Vec2 := input.p1 - position;
      b        : constant float32 := b2Dot (s, s) - Self.m_radius * Self.m_radius;

      --  Solve quadratic equation.
      r        : constant b2Vec2  := input.p2 - input.p1;
      c        : constant float32 := b2Dot (s, r);
      rr       : float32 := b2Dot (r, r);
      sigma    : constant float32 := c * c - rr * b;

      a        : float32;
   begin
      if sigma < 0.0 or else rr < b2_epsilon then   -- Check for negative discriminant and short segment.
         return False;
      end if;

      a := -(c + SqRt (sigma));    -- Find the point of intersection of the line with the circle.

      if 0.0 <= a and then a <= input.maxFraction * rr then    -- Is the intersection point on the segment?
         a               := a / rr;
         output.fraction := a;
         output.normal   := s + a * r;
         Normalize (output.normal);

         return True;
      end if;

      return False;
   end RayCast;








   overriding procedure ComputeAABB (Self : in b2CircleShape;   aabb      : access collision.b2AABB;
                                                     transform : in     b2Transform)
   is
      p : b2Vec2 := transform.position + b2Mul (transform.R, Self.m_p);
   begin
      aabb.lowerBound := (p.x - Self.m_radius, p.y - Self.m_radius);
      aabb.upperBound := (p.x + Self.m_radius, p.y + Self.m_radius);
   end ComputeAABB;





   overriding procedure ComputeMass (Self : in b2CircleShape;   massData : access b2MassData;
                                                     density  : in     float32)
   is
      Radius_squared : constant float32 := Self.m_radius * Self.m_radius;
   begin
      massData.mass   := density * b2_pi * Radius_squared;
      massData.center := Self.m_p;

      --  inertia about the local origin
      massData.I := massData.mass * (0.5 * Radius_squared + b2Dot (Self.m_p, Self.m_p));
   end ComputeMass;






   function GetSupport       (Self : in b2CircleShape;   d : in b2Vec2) return int32
   is
      pragma Unreferenced (d, Self);
   begin
      return 0;
   end GetSupport;



   function GetSupportVertex (Self : in b2CircleShape;   d : in b2Vec2) return b2Vec2
   is
      pragma Unreferenced (d);
   begin
      return Self.m_p;
   end GetSupportVertex;



   function GetVertexCount   (Self : in b2CircleShape               ) return int32
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end GetVertexCount;



   function GetVertex        (Self : in b2CircleShape;   index : in int32) return b2Vec2
   is
      use type int32;
   begin
      pragma Assert (index = 0);

      return Self.m_p;
   end GetVertex;



end impact.d2.Shape.circle;
