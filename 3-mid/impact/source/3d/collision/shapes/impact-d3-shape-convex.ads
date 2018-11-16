with impact.d3.Shape;



package impact.d3.Shape.convex
--
--  The impact.d3.Shape.convex is an abstract shape interface, implemented by all convex shapes such as impact.d3.Shape.convex.internal.polyhedral.box, impact.d3.convex_HullShape etc.
--
--  It describes general convex shapes using the localGetSupportingVertex interface, used by collision detectors such as impact.d3.collision.Detector.discrete.gjk_pair.
--
is



   type Item is abstract new impact.d3.Shape.item with null record;
   type View is access all Item'Class;


   MAX_PREFERRED_PENETRATION_DIRECTIONS : constant := 10;



   overriding procedure destruct (Self : in out Item) is null;



   function localGetSupportingVertex                     (Self : in Item;   vec : in Vector_3) return Vector_3   is abstract;
   function localGetSupportingVertexWithoutMargin        (Self : in Item;   vec : in Vector_3) return Vector_3   is abstract;

   function localGetSupportVertexWithoutMarginNonVirtual (Self : in Item'Class;   localDir : in Vector_3) return Vector_3;
   function localGetSupportVertexNonVirtual              (Self : in Item'Class;        vec : in Vector_3) return Vector_3;

   function getMarginNonVirtual (Self : in Item'Class) return Real;


   procedure getAabbNonVirtual (Self : in Item'Class;   t                : in     Transform_3d;
                                aabbMin, aabbMax :    out Vector_3);

   procedure project (Self : in Item;         t        : in     Transform_3d;
                                              dir      : in     Vector_3;
                                              min, max :    out Real);




   procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer)   is abstract;
   --
   --  Note that the vectors should be unit length.


   procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                                        aabbMin, aabbMax :    out Vector_3)       is  abstract;
   --
   --  'getAabb's default implementation is brute force, expected derived classes to implement a fast dedicated version


   procedure getAabbSlow (Self : in Item;   t                : in     Transform_3d;
                                            aabbMin, aabbMax :    out Vector_3)   is abstract;


   procedure setLocalScaling (Self : in out Item;   scaling : in Vector_3)        is  abstract;
   function  getLocalScaling (Self : in     Item)         return Vector_3         is  abstract;


   procedure setMargin (Self : in out Item;   margin : in Real)                 is  abstract;
   function  getMargin (Self : in     Item)        return Real                  is  abstract;


   function  getNumPreferredPenetrationDirections (Self : in     Item) return Integer    is abstract;
   procedure getPreferredPenetrationDirection     (Self : in     Item;   index             : in     Integer;
                                                                         penetrationVector :    out Vector_3)   is abstract;


end impact.d3.Shape.convex;
