with impact.d3.collision.convex_Raycast,
     impact.d3.Shape.convex,
     impact.d3.collision.simplex_Solver;



package impact.d3.collision.convex_Raycast.subsimplex
--
--  impact.d3.collision.convex_Raycast.subsimplex implements Gino van den Bergens' paper ...
--         "Ray Casting against bteral Convex Objects with Application to Continuous Collision Detection".
--
--  GJK based Ray Cast, optimized version.
--
--  Objects should not start in overlap, otherwise results are not defined.
--
is

   type Item is new impact.d3.collision.convex_Raycast.Item with private;


   function to_convex_Raycast (shapeA, shapeB : in     impact.d3.Shape.convex.view;
                               simplexSolver  : access impact.d3.collision.simplex_Solver.Item'Class) return Item;


   overriding function calcTimeOfImpact (Self : access Item;   fromA, toA : in     Transform_3d;
                                                    fromB, toB : in     Transform_3d;
                                                    result     : access impact.d3.collision.convex_Raycast.CastResult'Class) return Boolean;
   --
   --  SimsimplexConvexCast calculateTimeOfImpact calculates the time of impact+normal for the linear cast (sweep) between two moving objects.
   --
   --  Precondition is that objects should not penetration/overlap at the start from the interval. Overlap can be tested using impact.d3.collision.Detector.discrete.gjk_pair.








private

   type Item is new impact.d3.collision.convex_Raycast.Item with
      record
         m_simplexSolver : access impact.d3.collision.simplex_Solver.Item'Class;

         m_convexA,
         m_convexB       : access impact.d3.Shape.convex.Item'Class;
      end record;

end impact.d3.collision.convex_Raycast.subsimplex;
