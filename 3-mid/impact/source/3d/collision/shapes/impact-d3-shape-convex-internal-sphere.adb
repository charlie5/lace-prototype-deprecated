with impact.d3.collision.Proxy;
with impact.d3.Vector;
with impact.d3.Scalar;



package body impact.d3.Shape.convex.internal.sphere
is



   function to_sphere_Shape (radius : in math.Real) return Item
   is
      use impact.d3.collision.Proxy;

      Self : Item; --  := (to_impact.d3.Shape.convex.internal with others => <>);
   begin
      Self.setShapeType (SPHERE_SHAPE_PROXYTYPE);
      Self.setImplicitShapeDimensions ((radius, radius, radius));
      Self.setMargin (radius);

      return Self;
   end to_sphere_Shape;





   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;







   overriding function localGetSupportingVertex (Self : in Item;   vec : in Vector_3) return Vector_3
   is
      use impact.d3.Vector, impact.d3.Scalar, math.Vectors;
      supVertex : math.Vector_3 := Self.localGetSupportingVertexWithoutMargin (vec);
      vecnorm   : math.Vector_3 := vec;
   begin
      if length2 (vecnorm) < SIMD_EPSILON * SIMD_EPSILON then
         vecnorm := (-1.0, -1.0, -1.0);
      end if;

      normalize (vecnorm);
      supVertex := supVertex + Self.getMargin * vecnorm;


      return supVertex;
   end localGetSupportingVertex;








   overriding function localGetSupportingVertexWithoutMargin        (Self : in Item;   vec : in Vector_3) return Vector_3
   is
      pragma Unreferenced (Self, vec);
   begin
      return (0.0, 0.0, 0.0);
   end localGetSupportingVertexWithoutMargin;




   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer)
   is
      pragma Unreferenced (Self, vectors);
   begin
      for i in 1 .. numVectors
      loop
         supportVerticesOut (i) := (0.0, 0.0, 0.0);
      end loop;
   end batchedUnitVectorGetSupportingVertexWithoutMargin;









   --  broken due to scaling
   --
   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out Vector_3)
   is
      use math.Vectors;

      center : math.Vector_3 renames t.Translation;
      extent : constant math.Vector_3 :=      (Self.getMargin, Self.getMargin, Self.getMargin);
   begin
      aabbMin := center - extent;
      aabbMax := center + extent;
   end getAabb;








   overriding procedure setMargin (Self : in out Item;   margin : in Real)
   is
   begin
      impact.d3.Shape.convex.internal.setMargin (impact.d3.Shape.convex.internal.item (Self), margin);
   end setMargin;





   overriding function  getMargin (Self : in     Item)        return Real
   is
   begin
      --  To improve gjk behaviour, use radius+margin as the full margin, so never get into the penetration case.
      --  This means, non-uniform scaling is not supported anymore.
      --
      return Self.getRadius;
   end getMargin;








   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      elem : constant math.Real := 0.4 * mass * Self.getMargin * Self.getMargin;
   begin
      inertia := (elem, elem, elem);
   end calculateLocalInertia;









   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "SPHERE";
   end getName;





   function  getRadius   (Self : in     Item)        return math.Real
   is
   begin
      return Self.getImplicitShapeDimensions (1) * Self.getLocalScaling (1);
   end getRadius;






   procedure setUnscaledRadius (Self : in out Item;   To : in math.Real)
   is
   begin
      Self.setImplicitShapeDimensions ((To, To, To));
      impact.d3.Shape.convex.internal.setMargin (impact.d3.Shape.convex.internal.Item (Self),  To);
   end setUnscaledRadius;





end impact.d3.Shape.convex.internal.sphere;
