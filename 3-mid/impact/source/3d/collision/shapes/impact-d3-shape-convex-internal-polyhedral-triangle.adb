with impact.d3.collision.Proxy,
     impact.d3.Vector;





package body impact.d3.Shape.convex.internal.polyhedral.triangle
is

   use impact.d3.collision.Proxy;





   function to_triangle_Shape return impact.d3.Shape.convex.internal.polyhedral.triangle.item
   is
      Self : impact.d3.Shape.convex.internal.polyhedral.triangle.item;
   begin
      Self.setShapeType (TRIANGLE_SHAPE_PROXYTYPE);

      return Self;
   end to_triangle_Shape;





   function to_triangle_Shape (p0, p1, p2 : in math.Vector_3) return impact.d3.Shape.convex.internal.polyhedral.triangle.item
   is
      Self : impact.d3.Shape.convex.internal.polyhedral.triangle.item;
   begin
      Self.setShapeType (TRIANGLE_SHAPE_PROXYTYPE);

      Self.m_vertices1 (1) := p0;
      Self.m_vertices1 (2) := p1;
      Self.m_vertices1 (3) := p2;


      return Self;
   end to_triangle_Shape;





   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out Vector_3)
   is
   begin
      Self.getAabbSlow (t,  aabbMin, aabbMax);
   end getAabb;




   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "Triangle";
   end getName;




   overriding procedure setMargin (Self : in out Item;   margin : in math.Real)
   is
   begin
      null;
   end setMargin;





   overriding function  getMargin (Self : in     Item) return math.Real
   is
      pragma Unreferenced (Self);
   begin
      return 0.0;
   end getMargin;





   overriding function localGetSupportingVertex (Self : in Item;   vec : in Vector_3) return Vector_3
   is
      pragma Unreferenced (Self, vec);
   begin
      return (0.0, 0.0, 0.0);
   end localGetSupportingVertex;




   overriding function localGetSupportingVertexWithoutMargin (Self : in Item;   dir : in Vector_3) return Vector_3
   is
      use impact.d3.Vector;

      dots : constant math.Vector_3 := (dot (dir,  Self.m_vertices1 (1)),
                               dot (dir,  Self.m_vertices1 (2)),
                               dot (dir,  Self.m_vertices1 (3)));
   begin
      return Self.m_vertices1 (maxAxis (dots));
   end localGetSupportingVertexWithoutMargin;





   overriding procedure getAabbSlow (Self : in Item;   t                : in     Transform_3d;
                          aabbMin, aabbMax :    out Vector_3)
   is
   begin
      null;
   end getAabbSlow;





   overriding procedure setLocalScaling (Self : in out Item;   scaling : in Vector_3)
   is
   begin
      null;
   end setLocalScaling;





   overriding function  getLocalScaling (Self : in     Item) return Vector_3
   is
      pragma Unreferenced (Self);
   begin
      return (0.0, 0.0, 0.0);
   end getLocalScaling;





   overriding function  getNumPreferredPenetrationDirections (Self : in     Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 2;
   end getNumPreferredPenetrationDirections;





   overriding procedure getPreferredPenetrationDirection (Self : in     Item;   index             : in     Integer;
                                                                     penetrationVector :    out Vector_3)
   is
      use math.Vectors;
   begin
      Self.calcNormal (penetrationVector);

      if index /= 1 then
         penetrationVector := -penetrationVector;
      end if;
   end getPreferredPenetrationDirection;





   overriding function getNumVertices (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 3;
   end getNumVertices;




   overriding function getNumEdges    (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 3;
   end getNumEdges;





   overriding function getNumPlanes   (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end getNumPlanes;




   overriding procedure getVertex (Self : in Item;   i   : in     Integer;
                                          vtx :    out math.Vector_3)
   is
   begin
      vtx := Self.m_vertices1 (i);
   end getVertex;




   overriding procedure getEdge   (Self : in Item;   i      : in     Integer;
                                          pa, pb :    out math.Vector_3)
   is
   begin
      Self.getVertex (i, pa);

      if i = 3 then
         Self.getVertex (1,   pb);
      else
         Self.getVertex (i + 1, pb);
      end if;
   end getEdge;





   overriding procedure getPlane (Self : in Item;   planeNormal  :    out math.Vector_3;
                                         planeSupport :    out math.Vector_3;
                                         i            : in     Integer)
   is
   begin
      Self.getPlaneEquation (i, planeNormal, planeSupport);
   end getPlane;






   overriding function isInside   (Self : in Item;   pt        : in math.Vector_3;
                                          tolerance : in math.Real) return Boolean
   is
      use impact.d3.Vector, math.Vectors;

      normal : math.Vector_3;

      dist,                              -- distance to plane
      planeconst : math.Real;
   begin
      Self.calcNormal (normal);

      dist       := dot (pt, normal);
      planeconst := dot (Self.m_vertices1 (1),  normal);
      dist       := dist - planeconst;

      if         dist >= -tolerance
        and then dist <=  tolerance
      then
         --  inside check on edge-planes
         --
         for i in 1 .. 3
         loop
            declare
               pa, pb     : math.Vector_3;

               edge,
               edgeNormal : math.Vector_3;

               dist,
               edgeConst  : math.Real;

            begin
               Self.getEdge (i,  pa, pb);

               edge       := pb - pa;
               edgeNormal := cross (edge, normal);
               normalize (edgeNormal);

               dist       := dot (pt, edgeNormal);
               edgeConst  := dot (pa, edgeNormal);

               dist       := dist - edgeConst;

               if dist < -tolerance then
                  return False;
               end if;
            end;
         end loop;

         return True;
      end if;


      return False;
   end isInside;







   function getVertexPtr (Self : access Item;   index : in Integer) return access math.Vector_3
   is
   begin
      return Self.m_vertices1 (index)'Access;
   end getVertexPtr;





   procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     impact.d3.Shape.convex.internal.polyhedral.triangle.Vertices;
                                                                                  supportVerticesOut :    out impact.d3.Shape.convex.internal.polyhedral.triangle.Vertices;
                                                                                  numVectors         : in     Integer)
   is
      use impact.d3.Vector;
   begin
      for i in 1 .. numVectors
      loop
         declare
            dir  : constant math.Vector_3 := vectors (i);
            dots : constant math.Vector_3 := (dot (dir,  Self.m_vertices1 (1)),
                                     dot (dir,  Self.m_vertices1 (2)),
                                     dot (dir,  Self.m_vertices1 (3)));
         begin
            supportVerticesOut (i) := Self.m_vertices1 (maxAxis (dots));
         end;
      end loop;
   end batchedUnitVectorGetSupportingVertexWithoutMargin;





   procedure calcNormal       (Self : in Item;   normal : out math.Vector_3)
   is
      use math.Vectors,  impact.d3.Vector;
   begin
      normal := cross (Self.m_vertices1 (2) - Self.m_vertices1 (1),
                       Self.m_vertices1 (3) - Self.m_vertices1 (1));
      normalize (normal);
   end calcNormal;





   procedure getPlaneEquation (Self : in Item;   i            : in     Integer;
                                                 planeNormal  :    out math.Vector_3;
                                                 planeSupport :    out math.Vector_3)
   is
      pragma Unreferenced (i);
   begin
      Self.calcNormal (planeNormal);
      planeSupport := Self.m_vertices1 (1);
   end getPlaneEquation;





   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      pragma Unreferenced (Self, mass);
   begin
      pragma Assert (False);
      inertia := (0.0, 0.0, 0.0);
   end calculateLocalInertia;


end impact.d3.Shape.convex.internal.polyhedral.triangle;
