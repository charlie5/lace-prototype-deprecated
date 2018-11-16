--  #include "BulletCollision/CollisionShapes/impact.d3.collision.Margin.h"
--
--  #include "LinearMath/impact.d3.Vector.h"
--  #include "impact.d3.collision.convex_Raycast.h"
--  class impact.d3.Shape.convex;
--  class btMinkowskiSumShape;
--  #include "impact.d3.collision.simplex_Solver.h"

with impact.d3.collision.convex_Raycast;
with impact.d3.collision.simplex_Solver;
with impact.d3.Shape.convex;



package impact.d3.collision.convex_Raycast.gjk
--
--  Performs a raycast on a convex object using support mapping.
--
is

   type Item is new impact.d3.collision.convex_Raycast.item with private;


   function to_gjk_convex_Raycast (convexA, convexB : access impact.d3.Shape.convex.Item'Class;
                                   simplexSolver    : access impact.d3.collision.simplex_Solver.Item'Class) return Item;



   overriding function calcTimeOfImpact (Self : access Item;   fromA, toA : in     Transform_3d;
                                                    fromB, toB : in     Transform_3d;
                                                    result     : access impact.d3.collision.convex_Raycast.CastResult'Class) return Boolean;
   --
   --  Cast a convex against another convex object.




private

   type Item is new impact.d3.collision.convex_Raycast.item with
      record
         m_simplexSolver : access impact.d3.collision.simplex_Solver.Item'Class;

         m_convexA,
         m_convexB       :  access impact.d3.Shape.convex.Item'Class;
      end record;



end impact.d3.collision.convex_Raycast.gjk;


