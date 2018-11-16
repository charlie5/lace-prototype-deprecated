with impact.d3.Vector,
     impact.d3.Scalar,
     impact.d3.Transform,
     impact.d3.aabb_Util;



package body impact.d3.Shape.convex.internal
is



   function getImplicitShapeDimensions (Self : in Item) return math.Vector_3
   is
   begin
      return Self.m_implicitShapeDimensions;
   end getImplicitShapeDimensions;



   procedure setImplicitShapeDimensions (Self : in out Item;   dimensions : math.Vector_3)
   is
   begin
      Self.m_implicitShapeDimensions := dimensions;
   end setImplicitShapeDimensions;





   procedure setSafeMargin (Self : in out Item'Class;   minDimension            : in math.Real;
                                                        defaultMarginMultiplier : in math.Real := 0.1)
   is
      safeMargin : math.Real := defaultMarginMultiplier * minDimension;
   begin
      if safeMargin < Self.getMargin then
         Self.setMargin (safeMargin);
      end if;
   end setSafeMargin;




   procedure setSafeMargin (Self : in out Item'Class;   halfExtents             : in math.Vector_3;
                                                        defaultMarginMultiplier : in math.Real := 0.1)
   is
      minDimension : constant math.Real := halfExtents (impact.d3.Vector.minAxis (halfExtents));
   begin
      --  see http://code.google.com/p/bullet/issues/detail?id=349
      --  this margin check could could be added to other collision shapes too, or add some assert/warning somewhere

      Self.setSafeMargin (minDimension, defaultMarginMultiplier);
   end setSafeMargin;





   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                                              aabbMin, aabbMax :    out math.Vector_3)
   is
   begin
      self.getAabbSlow (t, aabbMin, aabbMax);
   end getAabb;




   overriding function  getLocalScaling (Self : in     Item) return math.Vector_3
   is
   begin
      return Self.m_localScaling;
   end getLocalScaling;



   function  getLocalScalingNV (Self : in     Item) return math.Vector_3
   is
   begin
      return Self.m_localScaling;
   end getLocalScalingNV;




   overriding procedure setMargin (Self : in out Item;     margin : in math.Real)
   is
   begin
      Self.m_collisionMargin := margin;
   end setMargin;





   overriding function  getMargin (Self : in     Item) return math.Real
   is
   begin
      return Self.m_collisionMargin;
   end getMargin;



   function  getMarginNV (Self : in     Item)          return math.Real
   is
   begin
      return Self.m_collisionMargin;
   end getMarginNV;



   overriding function  getNumPreferredPenetrationDirections (Self : in     Item)          return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 0;
   end getNumPreferredPenetrationDirections;




   overriding procedure getPreferredPenetrationDirection (Self : in Item;     Index             : in      Integer;
                                                                   penetrationVector :     out math.Vector_3)
   is
   begin
      raise Program_Error;   -- btAssert(0);
   end getPreferredPenetrationDirection;




   overriding function  localGetSupportingVertex (Self : in     Item;   vec : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, impact.d3.Scalar, math.Vectors;

      supVertex : math.Vector_3 := Item'Class (Self).localGetSupportingVertexWithoutMargin (vec);
      vecnorm   : math.Vector_3;
   begin
      if Self.getMargin /= 0.0 then
         vecnorm := vec;

         if length2 (vecnorm) < SIMD_EPSILON * SIMD_EPSILON then
            vecnorm := (-1.0, -1.0, -1.0);
         end if;

         normalize (vecnorm);
         supVertex := supVertex  +  Self.getMargin * vecnorm;
      end if;


      return supVertex;
   end localGetSupportingVertex;






   overriding procedure getAabbSlow (Self : in Item;     t                : in     Transform_3d;
                          aabbMin, aabbMax :    out math.Vector_3)
   is
      use linear_Algebra_3d,  impact.d3.Vector, impact.d3.Scalar, impact.d3.Transform, math.Vectors;

      margin : math.Real := Self.getMargin;

      vec,
      sv,
      tmp    : math.Vector_3;

   begin
      for i in 1 .. 3 loop
         vec         := (0.0, 0.0, 0.0);
         vec (i)     := 1.0;

         sv          := Self.localGetSupportingVertex (vec * getBasis (t));

         tmp         := t * sv;
         aabbMax (i) := tmp (i) + margin;
         vec (i)     := -1.0;

         tmp         := t * (Self.localGetSupportingVertex (vec * getBasis (t)));
         aabbMin (i) := tmp (i) - margin;
      end loop;
   end getAabbSlow;




   overriding procedure setLocalScaling (Self : in out Item;     scaling : in math.Vector_3)
   is
      use impact.d3.Vector;
   begin
      Self.m_localScaling := absolute (scaling);
   end setLocalScaling;




   procedure set_m_localScaling (Self :     out Item;   To : in math.Vector_3)
   is
   begin
      Self.m_localScaling := To;
   end set_m_localScaling;









   --- btConvexInternalAabbCachingShape
   --


   procedure setCachedLocalAabb (Self : in out btConvexInternalAabbCachingShape;   aabbMin, aabbMax : in math.Vector_3)
   is
   begin
      Self.m_isLocalAabbValid := True;
      Self.m_localAabbMin     := aabbMin;
      Self.m_localAabbMax     := aabbMax;
   end setCachedLocalAabb;




   procedure getCachedLocalAabb (Self : in     btConvexInternalAabbCachingShape;   aabbMin, aabbMax : out math.Vector_3)
   is
   begin
      pragma Assert (Self.m_isLocalAabbValid);

      aabbMin := Self.m_localAabbMin;
      aabbMax := Self.m_localAabbMax;
   end getCachedLocalAabb;



   procedure getNonvirtualAabb (Self : in     btConvexInternalAabbCachingShape'Class;   trans            : in     Transform_3d;
                                                                                        aabbMin, aabbMax :    out math.Vector_3;
                                margin           : in     math.Real)
   is
      use impact.d3.aabb_Util;
   begin
      pragma Assert (Self.m_isLocalAabbValid);

      --  lazy evaluation of local aabb
      --
      Transform_Aabb (Self.m_localAabbMin, Self.m_localAabbMax,  margin, trans,  aabbMin, aabbMax);
   end getNonvirtualAabb;






   overriding procedure setLocalScaling (Self : in out btConvexInternalAabbCachingShape;   scaling : in math.Vector_3)
   is
   begin
      impact.d3.Shape.convex.internal.item (Self).setLocalScaling (scaling);
      Self.recalcLocalAabb;
   end setLocalScaling;



   overriding procedure getAabb (Self : in     btConvexInternalAabbCachingShape;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out math.Vector_3)
   is
   begin
      Self.getNonvirtualAabb (t,  aabbMin, aabbMax,  Self.getMargin);
   end getAabb;





   procedure recalcLocalAabb (Self : in out btConvexInternalAabbCachingShape'Class)
   is


      directions : constant Vector_3_array := ((1.0,  0.0,  0.0),
                                               (0.0,  1.0,  0.0),
                                               (0.0,  0.0,  1.0),

                                               (-1.0,  0.0,  0.0),
                                               (0.0, -1.0,  0.0),
                                               (0.0,  0.0, -1.0));

      supporting :          Vector_3_array := ((0.0,  0.0,  0.0),
                                               (0.0,  0.0,  0.0),
                                               (0.0,  0.0,  0.0),

                                               (0.0,  0.0,  0.0),
                                               (0.0,  0.0,  0.0),
                                               (0.0,  0.0,  0.0));
   begin
      Self.m_isLocalAabbValid := True;

      Self.batchedUnitVectorGetSupportingVertexWithoutMargin (directions, supporting, 6);


      for i in 1 .. 3 loop
         Self.m_localAabbMax (i) := supporting (i)(i)  + Self.m_collisionMargin;
         Self.m_localAabbMin (i) := supporting (i + 3)(i)  - Self.m_collisionMargin;
      end loop;
   end recalcLocalAabb;




end impact.d3.Shape.convex.internal;
