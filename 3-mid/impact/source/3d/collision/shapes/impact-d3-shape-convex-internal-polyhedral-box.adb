with impact.d3.collision.Proxy,
     impact.d3.Shape.convex.internal,
     impact.d3.aabb_Util,
     impact.d3.Scalar,
     impact.d3.Vector;
--       math.Algebra.linear.d3;




package body impact.d3.Shape.convex.internal.polyhedral.box
is

   use impact.d3.Scalar, Math.Vectors;



   function to_box_Shape (boxHalfExtents : in math.Vector_3) return Item
   is
      use impact.d3.Vector, impact.d3.collision.Proxy;

      Self : Item;
      margin : math.Vector_3;
   begin
      Self.setShapeType (BOX_SHAPE_PROXYTYPE);

      Self.setSafeMargin (boxHalfExtents);

      margin := (Self.getMargin, Self.getMargin, Self.getMargin);

      Self.setImplicitShapeDimensions (Scaled (boxHalfExtents, by => Self.getLocalScaling) - margin);


      return Self;
   end to_box_Shape;






   overriding function localGetSupportingVertex (Self : in Item;   vec : in math.Vector_3) return math.Vector_3
   is
      halfExtents : math.Vector_3 := Self.getHalfExtentsWithoutMargin;
      margin      : constant math.Vector_3 := (Self.getMargin,  Self.getMargin,  Self.getMargin);
   begin
      halfExtents := halfExtents + margin;

      return (btFsels (vec (1),  halfExtents (1),  -halfExtents (1)),
              btFsels (vec (2),  halfExtents (2),  -halfExtents (2)),
              btFsels (vec (3),  halfExtents (3),  -halfExtents (3)));
   end localGetSupportingVertex;




   overriding function localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return math.Vector_3
   is
      halfExtents : math.Vector_3 renames Self.getHalfExtentsWithoutMargin;
   begin
      return (btFsels (vec (1),  halfExtents (1),  -halfExtents (1)),
              btFsels (vec (2),  halfExtents (2),  -halfExtents (2)),
              btFsels (vec (3),  halfExtents (3),  -halfExtents (3)));
   end localGetSupportingVertexWithoutMargin;






   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                numVectors         : in     Integer)
   is
      halfExtents : math.Vector_3 renames Self.getHalfExtentsWithoutMargin;
   begin
      for i in 1 .. numVectors loop
         declare
            vec : math.Vector_3 renames vectors (i);
         begin
            supportVerticesOut (i) := (btFsels (vec (1),  halfExtents (1),  -halfExtents (1)),
                                       btFsels (vec (2),  halfExtents (2),  -halfExtents (2)),
                                       btFsels (vec (3),  halfExtents (3),  -halfExtents (3)));
         end;
      end loop;
   end batchedUnitVectorGetSupportingVertexWithoutMargin;







   function getHalfExtentsWithMargin    (Self : in Item) return math.Vector_3
   is
      halfExtents : math.Vector_3  := Self.getHalfExtentsWithoutMargin;
      margin      : constant math.Vector_3  := (Self.getMargin,  Self.getMargin,  Self.getMargin);
   begin
      halfExtents := halfExtents + margin;

      return halfExtents;
   end getHalfExtentsWithMargin;






   function getHalfExtentsWithoutMargin (Self : in Item) return math.Vector_3
   is
   begin
      return Self.getImplicitShapeDimensions;    -- scaling is included, margin is not
   end getHalfExtentsWithoutMargin;









   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out math.Vector_3)
   is
      use impact.d3.aabb_Util;
   begin
      Transform_Aabb (Self.getHalfExtentsWithoutMargin,  Self.getMargin,  t,  aabbMin, aabbMax);
   end getAabb;







   overriding procedure getAabbSlow (Self : in Item;   t                : in     Transform_3d;
                          aabbMin, aabbMax :    out math.Vector_3)
   is
   begin
      raise Program_Error with "TBD";
      null;
   end getAabbSlow;





   overriding procedure setMargin     (Self : in out Item;   collisionMargin : in math.Real)
   is
      --  correct the m_implicitShapeDimensions for the margin
      --
      oldMargin                         : constant math.Vector_3 := (Self.getMargin, Self.getMargin, Self.getMargin);
      implicitShapeDimensionsWithMargin : constant math.Vector_3 := Self.getImplicitShapeDimensions + oldMargin;
      newMargin                         : math.Vector_3;
   begin
      impact.d3.Shape.convex.internal.Item (Self).setMargin (collisionMargin);
      newMargin := (Self.getMargin, Self.getMargin, Self.getMargin);
      Self.setImplicitShapeDimensions (implicitShapeDimensionsWithMargin - newMargin);
   end setMargin;






   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3)
   is
      use impact.d3.Vector;

      oldMargin                                 : constant math.Vector_3 := (Self.getMargin, Self.getMargin, Self.getMargin);
      implicitShapeDimensionsWithMargin         : constant math.Vector_3 := Self.getImplicitShapeDimensions + oldMargin;
      unScaledImplicitShapeDimensionsWithMargin : constant math.Vector_3 := invScaled (implicitShapeDimensionsWithMargin, by => (Self.getLocalScaling));
   begin
      impact.d3.Shape.convex.internal.item (Self).setLocalScaling (scaling);
      Self.setImplicitShapeDimensions (Scaled (unScaledImplicitShapeDimensionsWithMargin, by => Self.getLocalScaling) - oldMargin);
   end setLocalScaling;







   overriding procedure getPlane (Self : in Item;   planeNormal  :    out math.Vector_3;
                                         planeSupport :    out math.Vector_3;
                                         i            : in     Integer)
   is
      plane : math.Vector_4;      -- this plane might not be aligned...
   begin
      Self.getPlaneEquation (plane, i);

      planeNormal  := (plane (1),  plane (2),  plane (3));
      planeSupport := Self.localGetSupportingVertex (-planeNormal);
   end getPlane;







   overriding function getNumPlanes (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 6;
   end getNumPlanes;



   overriding function getNumVertices (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 8;
   end getNumVertices;



   overriding function getNumEdges (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 12;
   end getNumEdges;




   overriding procedure getVertex (Self : in Item;   i   : in     Integer;
                                          vtx :    out math.Vector_3)
   is
      halfExtents : math.Vector_3 := Self.getHalfExtentsWithMargin;
   begin
      vtx := (halfExtents (1) * Real (1 - (Flags (i) and 1))    - halfExtents (1) * Real (Flags (i) and 1),
              halfExtents (2) * Real (1 - ((Flags (i) and 2)/2)) - halfExtents (2) * Real ((Flags (i) and 2)/2),
              halfExtents (3) * Real (1 - ((Flags (i) and 4)/4)) - halfExtents (3) * Real ((Flags (i) and 4)/4));

   end getVertex;




   procedure getPlaneEquation (Self : in Item;   plane :    out math.Vector_4;
                                                 i     : in     Integer)
   is
      halfExtents : math.Vector_3 := Self.getHalfExtentsWithoutMargin;
   begin
      case i
      is
      when 1 =>   plane := (1.0,  0.0,  0.0, -halfExtents (1));
      when 2 =>   plane := (-1.0,  0.0,  0.0, -halfExtents (1));
      when 3 =>   plane := (0.0,  1.0,  0.0, -halfExtents (2));
      when 4 =>   plane := (0.0, -1.0,  0.0, -halfExtents (2));
      when 5 =>   plane := (0.0,  0.0,  1.0, -halfExtents (3));
      when 6 =>   plane := (0.0,  0.0, -1.0, -halfExtents (3));

      when others =>   null;   pragma Assert (False);
      end case;

   end getPlaneEquation;




   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      --  impact.d3.Scalar margin = impact.d3.Scalar(0.);
      halfExtents : math.Vector_3 := Self.getHalfExtentsWithMargin;

      lx : constant math.Real := 2.0 * halfExtents (1);
      ly : constant math.Real := 2.0 * halfExtents (2);
      lz : constant math.Real := 2.0 * halfExtents (3);
   begin

      inertia := (mass / 12.0  * (ly * ly + lz*lz),
                  mass / 12.0  * (lx * lx + lz*lz),
                  mass / 12.0  * (lx * lx + ly*ly));
   end calculateLocalInertia;







   overriding procedure getEdge (Self : in Item;   i      : in     Integer;
                                        pa, pb :    out math.Vector_3)
   is
      edgeVert0 : Integer := 1;
      edgeVert1 : Integer := 1;
   begin

      case i
      is
      when  1 => edgeVert0 := 0;   edgeVert1 := 1;
      when  2 => edgeVert0 := 0;   edgeVert1 := 2;
      when  3 => edgeVert0 := 1;   edgeVert1 := 3;
      when  4 => edgeVert0 := 2;   edgeVert1 := 3;
      when  5 => edgeVert0 := 0;   edgeVert1 := 4;
      when  6 => edgeVert0 := 1;   edgeVert1 := 5;
      when  7 => edgeVert0 := 2;   edgeVert1 := 6;
      when  8 => edgeVert0 := 3;   edgeVert1 := 7;
      when  9 => edgeVert0 := 4;   edgeVert1 := 5;
      when 10 => edgeVert0 := 4;   edgeVert1 := 6;
      when 11 => edgeVert0 := 5;   edgeVert1 := 7;
      when 12 => edgeVert0 := 6;   edgeVert1 := 7;

      when others =>   null;   pragma Assert (False);
      end case;


      Self.getVertex (edgeVert0, pa);
      Self.getVertex (edgeVert1, pb);
   end getEdge;






   overriding function isInside (Self : in Item;   pt        : in math.Vector_3;
                                        tolerance : in math.Real) return Boolean
   is
      halfExtents : math.Vector_3 := Self.getHalfExtentsWithoutMargin;
      --  impact.d3.Scalar minDist = 2*tolerance;
   begin

      return          pt (1) <=  halfExtents (1) + tolerance
        and then pt (1) >= -halfExtents (1) - tolerance
        and then pt (2) <=  halfExtents (2) + tolerance
        and then pt (2) >= -halfExtents (2) - tolerance
        and then pt (3) <=  halfExtents (3) + tolerance
        and then pt (3) >= -halfExtents (3) - tolerance;
   end isInside;





   overriding function getName   (Self : in Item) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Box";
   end getName;






   overriding function getNumPreferredPenetrationDirections (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 6;
   end getNumPreferredPenetrationDirections;






   overriding procedure getPreferredPenetrationDirection (Self : in Item;   index             : in     Integer;
                                                                 penetrationVector :    out math.Vector_3)
   is
      pragma Unreferenced (Self);
   begin
      case index
      is
      when 1 =>   penetrationVector := (1.0,  0.0,  0.0);
      when 2 =>   penetrationVector := (-1.0,  0.0,  0.0);
      when 3 =>   penetrationVector := (0.0,  1.0,  0.0);
      when 4 =>   penetrationVector := (0.0, -1.0,  0.0);
      when 5 =>   penetrationVector := (0.0,  0.0,  1.0);
      when 6 =>   penetrationVector := (0.0,  0.0, -1.0);

      when others => null;   pragma Assert (False);
      end case;
   end getPreferredPenetrationDirection;





end impact.d3.Shape.convex.internal.polyhedral.box;
