with impact.d3.Shape.convex.internal;

--  #include "impact.d3.Shape.convex.internal.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Proxy.h" // for the types



package impact.d3.Shape.convex.internal.cone
--
--  The impact.d3.Shape.convex.internal.cone implements a cone shape primitive, centered around the origin and aligned with the Y axis. The impact.d3.Shape.convex.internal.coneX
--  is aligned around the X axis and impact.d3.Shape.convex.internal.coneZ around the Z axis.
--
is

   type Item is new impact.d3.Shape.convex.internal.item with private;




   ----------
   --- Forge
   --

   function to_cone_Shape  (radius, height : in math.Real) return Item;      -- Returns a Cone shape, around the Y axis.
   function to_coneX_Shape (radius, height : in math.Real) return Item;      -- Returns a Cone shape, around the X axis.
   function to_coneZ_Shape (radius, height : in math.Real) return Item;      -- Returns a Cone shape, around the Z axis.





   ---------------
   --- Attributes
   --


   function getRadius (Self : in Item) return math.Real;
   function getHeight (Self : in Item) return math.Real;

--          impact.d3.Scalar getRadius() const { return m_radius;}
--          impact.d3.Scalar getHeight() const { return m_height;}


   overriding function localGetSupportingVertex              (Self : in Item;   vec : in math.Vector_3) return math.Vector_3;
   overriding function localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return math.Vector_3;

   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer);


   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);

--          virtual void        calculateLocalInertia(impact.d3.Scalar mass,impact.d3.Vector& inertia) const
--          {
--                  impact.d3.Transform identity;
--                  identity.setIdentity();
--                  impact.d3.Vector aabbMin,aabbMax;
--                  getAabb(identity,aabbMin,aabbMax);
--
--                  impact.d3.Vector halfExtents = (aabbMax-aabbMin)*impact.d3.Scalar(0.5);
--
--                  impact.d3.Scalar margin = getMargin();
--
--                  impact.d3.Scalar lx=impact.d3.Scalar(2.)*(halfExtents.x()+margin);
--                  impact.d3.Scalar ly=impact.d3.Scalar(2.)*(halfExtents.y()+margin);
--                  impact.d3.Scalar lz=impact.d3.Scalar(2.)*(halfExtents.z()+margin);
--                  const impact.d3.Scalar x2 = lx*lx;
--                  const impact.d3.Scalar y2 = ly*ly;
--                  const impact.d3.Scalar z2 = lz*lz;
--                  const impact.d3.Scalar scaledmass = mass * impact.d3.Scalar(0.08333333);
--
--                  inertia = scaledmass * (impact.d3.Vector(y2+z2,x2+z2,x2+y2));
--
--  //                inertia.x() = scaledmass * (y2+z2);
--  //                inertia.y() = scaledmass * (x2+z2);
--  //                inertia.z() = scaledmass * (x2+y2);
--          }




   overriding function  getName   (Self : in     Item)        return String;

--                  virtual const char*        getName()const
--                  {
--                          return "Cone";
--                  }




   procedure setConeUpIndex (Self : in out Item;   upIndex : in Integer);
   --
   --  Choose upAxis index.



   function  getConeUpIndex (Self : in     Item)        return Integer;

--                  int        getConeUpIndex() const
--                  {
--                          return m_coneIndices[1];
--                  }



   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3);








private

   type Indices is array (Positive range 1 .. 3) of Integer;



   type Item is new impact.d3.Shape.convex.internal.item with
      record
         m_sinAngle,
         m_radius,
         m_height      : math.Real;

         m_coneIndices : Indices;
      end record;



   function coneLocalSupport (Self : in Item;   v : in math.Vector_3) return math.Vector_3;

end impact.d3.Shape.convex.internal.cone;
