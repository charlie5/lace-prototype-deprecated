with impact.d3.Shape.convex.internal.polyhedral;




package impact.d3.Shape.convex.internal.polyhedral.triangle
--
--
--
is



   type Vertices is array (Positive range 1 .. 3) of aliased math.Vector_3;



   type Item is new impact.d3.Shape.convex.internal.polyhedral.item with
      record
         m_vertices1 : Vertices;
      end record;

   type View is access all Item'Class;



   function to_triangle_Shape                                 return impact.d3.Shape.convex.internal.polyhedral.triangle.item;
   function to_triangle_Shape (p0, p1, p2 : in math.Vector_3) return impact.d3.Shape.convex.internal.polyhedral.triangle.item;








   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                                        aabbMin, aabbMax :    out Vector_3);

   overriding function  getName   (Self : in     Item)        return String;

   overriding procedure setMargin (Self : in out Item;   margin : in math.Real);
   overriding function  getMargin (Self : in     Item)        return math.Real;


   overriding function  localGetSupportingVertex              (Self : in Item;   vec : in Vector_3) return Vector_3;
   overriding function  localGetSupportingVertexWithoutMargin (Self : in Item;   dir : in Vector_3) return Vector_3;



   overriding procedure getAabbSlow (Self : in Item;   t                : in     Transform_3d;
                                            aabbMin, aabbMax :    out Vector_3);


   overriding procedure setLocalScaling (Self : in out Item;   scaling : in Vector_3);
   overriding function  getLocalScaling (Self : in     Item)         return Vector_3;




   overriding function  getNumPreferredPenetrationDirections (Self : in     Item) return Integer;
   overriding procedure getPreferredPenetrationDirection     (Self : in     Item;   index             : in     Integer;
                                                                         penetrationVector :    out Vector_3);


   overriding function getNumVertices (Self : in Item) return Integer;


   overriding function getNumEdges    (Self : in Item) return Integer;
   overriding function getNumPlanes   (Self : in Item) return Integer;


   overriding procedure getVertex (Self : in Item;   i   : in     Integer;
                                          vtx :    out math.Vector_3);

   overriding procedure getEdge   (Self : in Item;   i      : in     Integer;
                                          pa, pb :    out math.Vector_3);


   overriding procedure getPlane (Self : in Item;   planeNormal  :    out math.Vector_3;
                                         planeSupport :    out math.Vector_3;
                                         i            : in     Integer);


   overriding function isInside   (Self : in Item;   pt        : in math.Vector_3;
                                          tolerance : in math.Real) return Boolean;







   function getVertexPtr (Self : access Item;   index : in Integer) return access math.Vector_3;

   procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     impact.d3.Shape.convex.internal.polyhedral.triangle.Vertices;
                                                                                  supportVerticesOut :    out impact.d3.Shape.convex.internal.polyhedral.triangle.Vertices;
                                                                                  numVectors         : in     Integer);


   procedure calcNormal       (Self : in Item;   normal : out math.Vector_3);


   procedure getPlaneEquation (Self : in Item;   i            : in     Integer;
                                                 planeNormal  :    out math.Vector_3;
                                                 planeSupport :    out math.Vector_3);


   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);




end impact.d3.Shape.convex.internal.polyhedral.triangle;
