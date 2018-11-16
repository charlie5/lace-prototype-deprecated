--  with math.algebra.linear.d3;
with impact.d3.collision.Proxy;
with impact.d3.Vector;
with impact.d3.Matrix;


package body impact.d3.Shape.concave.static_plane
is

   --- Forge
   --

   function to_static_plane_Shape (planeNormal   : in math.Vector_3;
                                   planeConstant : in math.Real  ) return Item
   is
      use math.Vectors; -- ,  math.algebra.linear.d3;

      Self : Item;
   begin
      define (Self);

      Self.m_planeNormal   := abs (planeNormal);
      Self.m_planeConstant := planeConstant;
      Self.m_localScaling  := (0.0, 0.0, 0.0);

      Self.setShapeType (impact.d3.collision.Proxy.STATIC_PLANE_PROXYTYPE);

      --        btAssert( btFuzzyZero(m_planeNormal.length() - impact.d3.Scalar(1.)) );

      return Self;
   end to_static_plane_Shape;






   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;






   --- Attributes
   --


   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out Vector_3)
   is
      pragma Unreferenced (Self, t);
   begin
--          (void)t;
--
--          impact.d3.Vector infvec (impact.d3.Scalar(BT_LARGE_FLOAT),impact.d3.Scalar(BT_LARGE_FLOAT),impact.d3.Scalar(BT_LARGE_FLOAT));
--
--          impact.d3.Vector center = m_planeNormal*m_planeConstant;
--          aabbMin = center + infvec*m_planeNormal;
--          aabbMax = aabbMin;
--          aabbMin.setMin(center - infvec*m_planeNormal);
--          aabbMax.setMax(center - infvec*m_planeNormal);


      aabbMin := (-BT_LARGE_FLOAT, -BT_LARGE_FLOAT, -BT_LARGE_FLOAT);
      aabbMax := (BT_LARGE_FLOAT,  BT_LARGE_FLOAT,  BT_LARGE_FLOAT);
   end getAabb;







   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      pragma Unreferenced (Self, mass);
   begin
      inertia := (0.0, 0.0, 0.0);   -- Moving concave objects not supported.
   end calculateLocalInertia;





   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "STATICPLANE";
   end getName;





   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3)
   is
   begin
      Self.m_localScaling := scaling;
   end setLocalScaling;





   overriding function  getLocalScaling (Self : in     Item)         return math.Vector_3
   is
   begin
      return Self.m_localScaling;
   end getLocalScaling;





   function  getPlaneNormal   (Self : in     Item)         return math.Vector_3
   is
   begin
      return        Self.m_planeNormal;
   end getPlaneNormal;





   function  getPlaneConstant (Self : in     Item)         return math.Real
   is
   begin
      return Self.m_planeConstant;
   end getPlaneConstant;






   overriding procedure processAllTriangles (Self : in     Item;   callback         : access impact.d3.triangle_Callback.Item'Class;
                                                        aabbMin, aabbMax : in     math.Vector_3)
   is
      use impact.d3.Vector, impact.d3.Matrix, math.Vectors;

      halfExtents : constant math.Vector_3 := (aabbMax - aabbMin) * 0.5;
      center      : constant math.Vector_3 := (aabbMax + aabbMin) * 0.5;
      radius      : math.Real     := abs (halfExtents);               -- length (halfExtents);

      tangentDir0,
      tangentDir1  : math.Vector_3;  -- This is where the triangles are generated, given AABB and plane equation (normal/constant).


--        supVertex0,
--        supVertex1   : math.Vector_3;

      projectedCenter : math.Vector_3;

--        triangle : array (1..3) of aliased math.Vector_3;
      triangle : aliased math.Matrix_3x3;

   begin
      btPlaneSpace1 (Self.m_planeNormal,  tangentDir0, tangentDir1);    -- 'tangentDir0/tangentDir1' can be precalculated.

      projectedCenter :=   center
                         - (dot (Self.m_planeNormal, center)  -  Self.m_planeConstant)
                            * Self.m_planeNormal;


      Row (triangle'Access, 1).all := projectedCenter  +  tangentDir0*radius  +  tangentDir1*radius;
      Row (triangle'Access, 2).all := projectedCenter  +  tangentDir0*radius  -  tangentDir1*radius;
      Row (triangle'Access, 3).all := projectedCenter  -  tangentDir0*radius  -  tangentDir1*radius;

--        callback.processTriangle (triangle (1)'access,  0, 0);
      callback.processTriangle (triangle'Access,  0, 0);


      Row (triangle'Access, 1).all := projectedCenter  -  tangentDir0*radius  -  tangentDir1*radius;
      Row (triangle'Access, 2).all := projectedCenter  -  tangentDir0*radius  +  tangentDir1*radius;
      Row (triangle'Access, 3).all := projectedCenter  +  tangentDir0*radius  +  tangentDir1*radius;

--        callback.processTriangle (triangle (1)'access,  0, 1);
      callback.processTriangle (triangle'Access,  0, 1);
   end processAllTriangles;



end impact.d3.Shape.concave.static_plane;
