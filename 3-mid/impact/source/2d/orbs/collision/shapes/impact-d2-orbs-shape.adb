with impact.d2.Math;

package body impact.d2.orbs.Shape
is


   function to_Circle (Radius : in float_math.Real) return Shape.item
   is
   begin
      return (m_radius => Radius);
   end to_Circle;




   function Clone (Self : in Item) return Shape.item
   is
   begin
      return Self;
   end Clone;




   function test_Point (Self : in Item;   xf : in impact.d2.Math.b2Transform;
                        p  : in impact.d2.Math.b2Vec2   ) return Boolean
   is
      use impact.d2.Math;
      center : constant impact.d2.Math.b2Vec2 := xf.position;
      d      : constant impact.d2.Math.b2Vec2 := p - center;
   begin
      return impact.d2.Math.b2Dot (d, d) <= Self.m_radius * Self.m_radius;
   end test_Point;




   --  Collision Detection in Interactive 3D Environments by Gino van den Bergen
   --  From Section 3.1.2
   --  x       = s + a * r
   --  norm(x) = radius
   --
   function ray_Cast (Self : in Item;   output    : access collision.b2RayCastOutput;
                                          input     : in     collision.b2RayCastInput;
                      transform : in     impact.d2.Math.b2Transform            ) return Boolean
   is
      use float_math.Functions, impact.d2.Math;
      position : constant impact.d2.Math.b2Vec2 := transform.position;
      s        : constant impact.d2.Math.b2Vec2 := input.p1 - position;
      b        : constant float32 := impact.d2.Math.b2Dot (s, s) - Self.m_radius * Self.m_radius;

      --  Solve quadratic equation.
      r        : constant b2Vec2  := input.p2 - input.p1;
      c        : constant float32 := impact.d2.Math.b2Dot (s, r);
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
         impact.d2.Math.Normalize (output.normal);

         return True;
      end if;

      return False;
   end ray_Cast;








   procedure compute_AABB (Self : in Item;   aabb : access collision.b2AABB;
                           xf   : in     impact.d2.Math.b2Transform)
   is
      use impact.d2.Math;
      p : b2Vec2 := xf.position;
   begin
      aabb.lowerBound := (p.x - Self.m_radius,  p.y - Self.m_radius);
      aabb.upperBound := (p.x + Self.m_radius,  p.y + Self.m_radius);
   end compute_AABB;





   procedure compute_Mass (Self : in Item;   massData : access mass_Data;
                                             density  : in     float32)
   is
      Radius_squared : constant float32 := Self.m_radius * Self.m_radius;
   begin
      massData.mass   := density * b2_pi * Radius_squared;
      massData.center := (0.0, 0.0);

      --  inertia about the local origin
      massData.I := massData.mass * (0.5 * Radius_squared);
   end compute_Mass;






   function get_Support (Self : in Item;   d : in impact.d2.Math.b2Vec2) return int32
   is
      pragma Unreferenced (d, Self);
   begin
      return 0;
   end get_Support;



   function get_support_Vertex (Self : in Item;   d : in impact.d2.Math.b2Vec2) return impact.d2.Math.b2Vec2
   is
      pragma Unreferenced (d, Self);
   begin
      return (0.0, 0.0);
   end get_support_Vertex;



   function get_vertex_Count (Self : in Item                  ) return int32
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end get_vertex_Count;



   function get_Vertex (Self : in Item;   index : in int32) return impact.d2.Math.b2Vec2
   is
      pragma Unreferenced (Self);
      use type int32;
   begin
      pragma Assert (index = 0);

      return (0.0, 0.0); -- Self.m_p;
   end get_Vertex;




end impact.d2.orbs.Shape;
