with impact.d3.Shape.convex.internal;


--  #include "impact.d3.Shape.convex.internal.polyhedral.box.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Proxy.h" // for the types
--  #include "LinearMath/impact.d3.Vector.h"


package impact.d3.Shape.convex.internal.cylinder
--
--  The impact.d3.Shape.convex.internal.cylinder class implements a cylinder shape primitive, centered around the origin. Its central axis aligned
--  with the Y axis. impact.d3.Shape.convex.internal.cylinderX is aligned with the X axis and impact.d3.Shape.convex.internal.cylinderZ around the Z axis.
--
is

   type Item is new impact.d3.Shape.convex.internal.item with private;

   type View is access all Item'Class;



   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);

   overriding function  getName   (Self : in     Item)        return String;




   overriding function  localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return Math.Vector_3;



   overriding function  localGetSupportingVertex              (Self : in Item;   vec : in math.Vector_3) return Math.Vector_3;





   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer);





   function getHalfExtentsWithMargin (Self : in Item) return math.Vector_3;




   function getHalfExtentsWithoutMargin (Self : in Item) return math.Vector_3;




   overriding procedure getAabb   (Self : in Item;   t                : in     Transform_3d;
                                          aabbMin, aabbMax :     out math.Vector_3);




   overriding procedure setMargin (Self : in out Item;   collisionMargin  : in     math.Real);




   --     procedure calculateLocalInertia (Self : in     Item;   mass    : in     math.Real;
   --                                                            inertia :    out math.Vector_3);
   --
   --  Use box inertia.
   --  //    virtual void        calculateLocalInertia(impact.d3.Scalar mass,impact.d3.Vector& inertia) const;




   function getUpAxis (Self : in Item) return Integer;


   function getRadius (Self : in Item) return math.Real;





   overriding procedure setLocalScaling (Self : in out Item;   scaling  : in     math.Vector_3);







   --------------
   --- cylinderX
   --

   type cylinderX is new impact.d3.Shape.convex.internal.cylinder.Item with private;



   overriding function  localGetSupportingVertexWithoutMargin (Self : in cylinderX;   vec : in math.Vector_3) return Math.Vector_3;

   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in cylinderX;   vectors            : in     Vector_3_array;
                                                                                              supportVerticesOut :    out Vector_3_array;
                                                                                              numVectors         : in     Integer);


   overriding function  getName   (Self : in     cylinderX)        return String;

--          virtual const char*        getName()const
--          {
--                  return "CylinderX";
--          }


   overriding function getRadius (Self : in cylinderX) return math.Real;

--          virtual impact.d3.Scalar getRadius() const
--          {
--                  return getHalfExtentsWithMargin().getY();
--          }






   --------------
   --- cylinderZ
   --

   type cylinderZ is new impact.d3.Shape.convex.internal.cylinder.Item with private;





   overriding function  localGetSupportingVertexWithoutMargin (Self : in cylinderZ;   vec : in math.Vector_3) return Math.Vector_3;

   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in cylinderZ;   vectors            : in     Vector_3_array;
                                                                                              supportVerticesOut :    out Vector_3_array;
                                                                                              numVectors         : in     Integer);


   overriding function  getName   (Self : in     cylinderZ)        return String;

--          virtual const char*        getName()const
--          {
--                  return "CylinderZ";
--          }


   overriding function getRadius (Self : in cylinderZ) return math.Real;

--          virtual impact.d3.Scalar getRadius() const
--          {
--                  return getHalfExtentsWithMargin().getX();
--          }




   package Forge
   is
      function  to_cylinder_Shape   (halfExtents : in math.Vector_3) return Item;
      function  to_cylinder_Z_Shape (halfExtents : in math.Vector_3) return cylinderZ;
      function  to_cylinder_X_Shape (halfExtents : in math.Vector_3) return cylinderX;
   end Forge;





private

   type Item is new impact.d3.Shape.convex.internal.item with
      record
         m_upAxis : Integer;
      end record;


   type cylinderX is new impact.d3.Shape.convex.internal.cylinder.item with null record;
   type cylinderZ is new impact.d3.Shape.convex.internal.cylinder.item with null record;


end impact.d3.Shape.convex.internal.cylinder;
