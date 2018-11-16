with impact.d3.Shape.convex.internal;



package impact.d3.Shape.convex.internal.sphere
--
--  The impact.d3.Shape.convex.internal.sphere implements an implicit sphere, centered around a local origin with radius.
--
is




   type Item is new impact.d3.Shape.convex.internal.Item with private;
   type View is access all Item'Class;




   --- Forge
   --
   function to_sphere_Shape (radius : in math.Real) return Item;

   overriding procedure destruct (Self : in out Item);




   --- Attributes
   --

   overriding function localGetSupportingVertex                     (Self : in Item;   vec : in Vector_3) return Vector_3;
   overriding function localGetSupportingVertexWithoutMargin        (Self : in Item;   vec : in Vector_3) return Vector_3;




   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer);


   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                                        aabbMin, aabbMax :    out Vector_3)     ;
   --
   --  'getAabb's default implementation is brute force, expected derived classes to implement a fast dedicated version




   overriding procedure setMargin (Self : in out Item;   margin : in Real)               ;
   overriding function  getMargin (Self : in     Item)        return Real                ;





   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);

   overriding function  getName   (Self : in     Item)        return String     ;



   function  getRadius   (Self : in     Item)        return math.Real;






private

   type Item is new impact.d3.Shape.convex.internal.Item with
      record
         null;
      end record;

end impact.d3.Shape.convex.internal.sphere;
