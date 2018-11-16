with impact.d3.Aabb_Util;
with impact.d3.Vector;
with impact.d3.collision.Proxy;
with impact.d3.Scalar;



package body impact.d3.Shape.convex.internal.cylinder
is


   package body Forge
   is

      function to_cylinder_Shape (halfExtents : in math.Vector_3) return Item
      is
         use impact.d3.Vector, math.Vectors;

         Self   : Item;
         margin : math.Vector_3;

      begin
         Self.m_upAxis := 2;
         Self.setSafeMargin (halfExtents);

         margin := (Self.getMargin,  Self.getMargin,  Self.getMargin);

         Self.setImplicitShapeDimensions (Scaled (halfExtents, by => Self.getLocalScaling) - margin);
         Self.setShapeType (impact.d3.collision.Proxy.CYLINDER_SHAPE_PROXYTYPE);


         return Self;
      end to_cylinder_Shape;



      function  to_cylinder_X_Shape (halfExtents : in math.Vector_3) return cylinderX
      is
         Self : cylinderX := (to_cylinder_Shape (halfExtents) with others => <>);
      begin
         Self.m_upAxis := 1;
         return Self;
      end to_cylinder_X_Shape;




      function  to_cylinder_Z_Shape (halfExtents : in math.Vector_3) return cylinderZ
      is
         Self : cylinderZ := (to_cylinder_Shape (halfExtents) with others => <>);
      begin
         Self.m_upAxis := 3;
         return Self;
      end to_cylinder_Z_Shape;


   end Forge;






   --- Utility
   --

   function CylinderLocalSupportX (halfExtents, v : in math.Vector_3) return math.Vector_3;
   function CylinderLocalSupportY (halfExtents, v : in math.Vector_3) return math.Vector_3;
   function CylinderLocalSupportZ (halfExtents, v : in math.Vector_3) return math.Vector_3;








   --- Operations
   --

   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      radius2     : math.Real;                                                -- square of cylinder radius
      height2     : math.Real;                                                -- square of cylinder height
      halfExtents : math.Vector_3 := Self.getHalfExtentsWithMargin;        -- get cylinder dimensions

      div12       : constant math.Real     := mass / 12.0;
      div4        : constant math.Real     := mass /  4.0;
      div2        : constant math.Real     := mass /  2.0;

      idxRadius,
      idxHeight   : Integer;

      t1, t2      : math.Real;

   begin
      --  cylinder is defined as following:
      --
      --  - principle axis aligned along y by default, radius in x, z-value not used
      --  - for impact.d3.Shape.convex.internal.cylinderX: principle axis aligned along x, radius in y direction, z-value not used
      --  - for impact.d3.Shape.convex.internal.cylinderZ: principle axis aligned along z, radius in x direction, y-value not used
      --


      --  get indices of radius and height of cylinder
      --
      case Self.m_upAxis
      is
      when 1 =>                        -- cylinder is aligned along x
         idxRadius := 2;
         idxHeight := 1;

      when 2 =>                -- cylinder is aligned along y
         idxRadius := 1;
         idxHeight := 2;

      when 3 =>                -- cylinder is aligned along z
         idxRadius := 1;
         idxHeight := 3;

      when others =>
         raise Program_Error;
      end case;


      --  calculate squares
      --
      radius2 :=       halfExtents (idxRadius) * halfExtents (idxRadius);
      height2 := 4.0 * halfExtents (idxHeight) * halfExtents (idxHeight);


      --  calculate tensor terms
      --
      t1 := div12 * height2  +  div4 * radius2;
      t2 := div2  * radius2;


      --  set diagonal elements of inertia tensor
      --
      case Self.m_upAxis
      is
      when 1 =>             inertia := (t2, t1, t1);      -- cylinder is aligned along x
      when 3 =>             inertia := (t1, t1, t2);      -- cylinder is aligned along z
      when 2 =>      inertia := (t1, t2, t1);      -- cylinder is aligned along y
      when others => raise Program_Error;
      end case;

   end calculateLocalInertia;






   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "CylinderY";
   end getName;





   overriding function  localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return Math.Vector_3
   is
   begin
      return CylinderLocalSupportY (Self.getHalfExtentsWithoutMargin, vec);
   end localGetSupportingVertexWithoutMargin;






   overriding function  localGetSupportingVertex              (Self : in Item;   vec : in math.Vector_3) return Math.Vector_3
   is
      use impact.d3.Vector, math.Vectors;

      supVertex,
      vecnorm  : math.Vector_3;

   begin
      supVertex := Self.localGetSupportingVertexWithoutMargin (vec);

      if Self.getMargin /= 0.0 then
         vecnorm := vec;

         if length2 (vecnorm)  <  impact.d3.Scalar.SIMD_EPSILON * impact.d3.Scalar.SIMD_EPSILON then
            vecnorm := (-1.0, -1.0, -1.0);
         end if;

         normalize (vecnorm);
         supVertex := supVertex  +  Self.getMargin * vecnorm;
      end if;


      return supVertex;
   end localGetSupportingVertex;







   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer)
   is
   begin
      for i in 1 .. numVectors
      loop
         supportVerticesOut (i) := CylinderLocalSupportY (Self.getHalfExtentsWithoutMargin, vectors (i));
      end loop;
   end batchedUnitVectorGetSupportingVertexWithoutMargin;





   function getHalfExtentsWithMargin (Self : in Item) return math.Vector_3
   is
      use math.Vectors;

      halfExtents : math.Vector_3 := Self.getHalfExtentsWithoutMargin;
      margin      : constant math.Vector_3 := (Self.getMargin, Self.getMargin, Self.getMargin);
   begin
      halfExtents := halfExtents + margin;
      return halfExtents;
   end getHalfExtentsWithMargin;






   function getHalfExtentsWithoutMargin (Self : in Item) return math.Vector_3
   is
   begin
      return Self.getImplicitShapeDimensions;    -- changed in Bullet 2.63: assume the scaling and margin are included
   end getHalfExtentsWithoutMargin;







   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :     out math.Vector_3)
   is
      use Aabb_Util;
   begin
      Transform_Aabb (Self.getHalfExtentsWithoutMargin, Self.getMargin,  t,  aabbMin, aabbMax);
   end getAabb;





   overriding procedure setMargin (Self : in out Item;   collisionMargin  : in     math.Real)
   is
      use math.Vectors;

      --  Correct the m_implicitShapeDimensions for the margin.
      --
      oldMargin                         : constant math.Vector_3 := (Self.getMargin, Self.getMargin, Self.getMargin);
      implicitShapeDimensionsWithMargin : constant math.Vector_3 := Self.getImplicitShapeDimensions + oldMargin;
      newMargin                         : math.Vector_3;

   begin
      impact.d3.Shape.convex.internal.item (Self).setMargin (collisionMargin);

      newMargin := (Self.getMargin,  Self.getMargin,  Self.getMargin);

      Self.setImplicitShapeDimensions (implicitShapeDimensionsWithMargin - newMargin);
   end setMargin;







   function getUpAxis (Self : in Item) return Integer
   is
   begin
      return Self.m_upAxis;
   end getUpAxis;







   function getRadius (Self : in Item) return math.Real
   is
   begin
      return Self.getHalfExtentsWithMargin (1);
   end getRadius;







   overriding procedure setLocalScaling (Self : in out Item;   scaling  : in     math.Vector_3)
   is
      use impact.d3.Vector, math.Vectors;

      oldMargin                                 : constant math.Vector_3 := (Self.getMargin,  Self.getMargin,  Self.getMargin);
      implicitShapeDimensionsWithMargin         : constant math.Vector_3 := Self.getImplicitShapeDimensions + oldMargin;
      initial_local_Scaling                     : math.Vector_3 := Self.getLocalScaling;
      inverse_local_Scaling                     : constant math.Vector_3 := (1.0 / initial_local_Scaling (1),
                                                                    1.0 / initial_local_Scaling (2),
                                                                    1.0 / initial_local_Scaling (3));
      unScaledImplicitShapeDimensionsWithMargin : constant math.Vector_3 := Scaled (implicitShapeDimensionsWithMargin, by => inverse_local_Scaling);
   begin
      impact.d3.Shape.convex.internal.item (Self).setLocalScaling (scaling);   -- call base class

      Self.setImplicitShapeDimensions (Scaled (unScaledImplicitShapeDimensionsWithMargin, by => Self.getLocalScaling)  -  oldMargin);
   end setLocalScaling;










   ---------------------
   --- impact.d3.Shape.convex.internal.cylinderX
   --





   overriding function  localGetSupportingVertexWithoutMargin (Self : in cylinderX;   vec : in math.Vector_3) return Math.Vector_3
   is
   begin
      return CylinderLocalSupportX (Self.getHalfExtentsWithoutMargin, vec);
   end localGetSupportingVertexWithoutMargin;




   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in cylinderX;   vectors            : in     Vector_3_array;
                                                                                              supportVerticesOut :    out Vector_3_array;
                                                                numVectors         : in     Integer)
   is
   begin
      for i in 1 .. numVectors
      loop
         supportVerticesOut (i) := CylinderLocalSupportX (Self.getHalfExtentsWithoutMargin, vectors (i));
      end loop;
   end batchedUnitVectorGetSupportingVertexWithoutMargin;




   overriding function  getName (Self : in     cylinderX)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "CylinderX";
   end getName;






   overriding function getRadius (Self : in cylinderX) return math.Real
   is
   begin
      return Self.getHalfExtentsWithMargin (2);
   end getRadius;







   ---------------------
   --- impact.d3.Shape.convex.internal.cylinderZ
   --




   overriding function  localGetSupportingVertexWithoutMargin (Self : in cylinderZ;   vec : in math.Vector_3) return Math.Vector_3
   is
   begin
      return CylinderLocalSupportZ (Self.getHalfExtentsWithoutMargin, vec);
   end localGetSupportingVertexWithoutMargin;






   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in cylinderZ;   vectors            : in     Vector_3_array;
                                                                                       supportVerticesOut :    out Vector_3_array;
                                                                                       numVectors         : in     Integer)
   is
   begin
      for i in 1 .. numVectors
      loop
         supportVerticesOut (i) := CylinderLocalSupportZ (Self.getHalfExtentsWithoutMargin, vectors (i));
      end loop;
   end batchedUnitVectorGetSupportingVertexWithoutMargin;





   overriding function  getName (Self : in     cylinderZ)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "CylinderZ";
   end getName;





   overriding function getRadius (Self : in cylinderZ) return math.Real
   is
   begin
      return Self.getHalfExtentsWithMargin (1);
   end getRadius;






   function CylinderLocalSupportX (halfExtents,
                                   v            : in math.Vector_3) return math.Vector_3
   is
      use math.Functions;

      cylinderUpAxis : constant Integer := 1;
      XX             : constant Integer := 2;
      YY             : constant Integer := 1;
      ZZ             : constant Integer := 3;

      radius     : constant math.Real := halfExtents (XX);
      halfHeight : constant math.Real := halfExtents (cylinderUpAxis);

      tmp        : math.Vector_3;
      d          : math.Real;

      s          : constant math.Real := sqRt (v (XX) * v (XX) + v (ZZ) * v (ZZ));

   begin
      --  mapping depends on how cylinder local orientation is
      --  extents of the cylinder is: X,Y is for radius, and Z for height


      if s /= 0.0 then
         d        := radius / s;
         tmp (XX) := v (XX) * d;
         tmp (YY) := (if v (YY) < 0.0 then -halfHeight else halfHeight);
         tmp (ZZ) := v (ZZ) * d;

         return tmp;
      else
         tmp (XX) := radius;
         tmp (YY) := (if v (YY) < 0.0 then -halfHeight else halfHeight);
         tmp (ZZ) := 0.0;

         return tmp;
      end if;

   end CylinderLocalSupportX;






   function CylinderLocalSupportY (halfExtents,
                                   v            : in math.Vector_3) return math.Vector_3
   is
      use math.Functions;

      cylinderUpAxis : constant Integer := 2;
      XX             : constant Integer := 1;
      YY             : constant Integer := 2;
      ZZ             : constant Integer := 3;

      radius     : constant math.Real := halfExtents (XX);
      halfHeight : constant math.Real := halfExtents (cylinderUpAxis);

      tmp        : math.Vector_3;
      d          : math.Real;

      s          : constant math.Real := sqRt (v (XX) * v (XX) + v (ZZ) * v (ZZ));

   begin
      --  mapping depends on how cylinder local orientation is
      --  extents of the cylinder is: X,Y is for radius, and Z for height


      if s /= 0.0 then
         d        := radius / s;
         tmp (XX) := v (XX) * d;
         tmp (YY) := (if v (YY) < 0.0 then -halfHeight else halfHeight);
         tmp (ZZ) := v (ZZ) * d;

         return tmp;
      else
         tmp (XX) := radius;
         tmp (YY) := (if v (YY) < 0.0 then -halfHeight else halfHeight);
         tmp (ZZ) := 0.0;

         return tmp;
      end if;

   end CylinderLocalSupportY;






   function CylinderLocalSupportZ (halfExtents,
                                   v            : in math.Vector_3) return math.Vector_3
   is
      use math.Functions;

      cylinderUpAxis : constant Integer := 3;
      XX             : constant Integer := 1;
      YY             : constant Integer := 3;
      ZZ             : constant Integer := 2;

      radius     : constant math.Real := halfExtents (XX);
      halfHeight : constant math.Real := halfExtents (cylinderUpAxis);

      tmp        : math.Vector_3;
      d          : math.Real;

      s          : constant math.Real := sqRt (v (XX) * v (XX) + v (ZZ) * v (ZZ));

   begin
      --  mapping depends on how cylinder local orientation is
      --  extents of the cylinder is: X,Y is for radius, and Z for height

      if s /= 0.0 then
         d        := radius / s;
         tmp (XX) := v (XX) * d;
         tmp (YY) := (if v (YY) < 0.0 then -halfHeight else halfHeight);
         tmp (ZZ) := v (ZZ) * d;

         return tmp;
      else
         tmp (XX) := radius;
         tmp (YY) := (if v (YY) < 0.0 then -halfHeight else halfHeight);
         tmp (ZZ) := 0.0;

         return tmp;
      end if;

   end CylinderLocalSupportZ;



end impact.d3.Shape.convex.internal.cylinder;
