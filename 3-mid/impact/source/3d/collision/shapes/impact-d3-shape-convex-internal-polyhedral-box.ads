with impact.d3.Shape.convex.internal.polyhedral;



package impact.d3.Shape.convex.internal.polyhedral.box
--
--  The impact.d3.Shape.convex.internal.polyhedral.box is a box primitive around the origin, its sides axis aligned with length specified by half extents, in local shape coordinates.
--
--  When used as part of a impact.d3.Object or impact.d3.Object.rigid it will be an oriented box in world space.
--
is

   type Item is new impact.d3.Shape.convex.internal.polyhedral.item with private;
   type View is access all Item'Class;


   function to_box_Shape (boxHalfExtents : in math.Vector_3) return Item;





   overriding function localGetSupportingVertex                     (Self : in Item;   vec : in math.Vector_3) return math.Vector_3;
   overriding function localGetSupportingVertexWithoutMargin        (Self : in Item;   vec : in math.Vector_3) return math.Vector_3;

   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer);

   overriding procedure getAabb     (Self : in Item;   t                : in     Transform_3d;
                                            aabbMin, aabbMax :    out math.Vector_3);

   overriding procedure getAabbSlow (Self : in Item;   t                : in     Transform_3d;
                                            aabbMin, aabbMax :    out math.Vector_3);



   function getHalfExtentsWithMargin    (Self : in Item) return math.Vector_3;
   function getHalfExtentsWithoutMargin (Self : in Item) return math.Vector_3;





   overriding procedure setMargin     (Self : in out Item;   collisionMargin : in math.Real);



   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3);


   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);


   overriding procedure getPlane (Self : in Item;   planeNormal  :    out math.Vector_3;
                                         planeSupport :    out math.Vector_3;
                                         i            : in     Integer);





   overriding function getNumPlanes   (Self : in Item) return Integer;
   overriding function getNumVertices (Self : in Item) return Integer;
   overriding function getNumEdges    (Self : in Item) return Integer;


   overriding procedure getVertex (Self : in Item;   i   : in     Integer;
                                          vtx :    out math.Vector_3);



   procedure getPlaneEquation (Self : in Item;   plane :    out math.Vector_4;
                                                 i     : in     Integer);




   overriding procedure getEdge (Self : in Item;   i      : in     Integer;
                                        pa, pb :    out math.Vector_3);





   overriding function isInside   (Self : in Item;   pt        : in math.Vector_3;
                                          tolerance : in math.Real) return Boolean;


   overriding function getName   (Self : in Item) return String;     -- for debugging


   overriding function getNumPreferredPenetrationDirections (Self : in Item) return Integer;






   overriding procedure getPreferredPenetrationDirection (Self : in Item;   index             : in     Integer;
                                                                 penetrationVector :    out math.Vector_3);







private


   type Item is new impact.d3.Shape.convex.internal.polyhedral.item with
      record
         null;
         --  impact.d3.Vector     m_boxHalfExtents1;     -- use m_implicitShapeDimensions instead
      end record;



end impact.d3.Shape.convex.internal.polyhedral.box;
