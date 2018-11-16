with impact.d3.Shape.convex,
     impact.d3.collision.simplex_Solver;



package impact.d3.collision.convex_penetration_depth_Solver
--
--  ConvexPenetrationDepthSolver provides an interface for penetration depth calculation.
--
is
   use Math;


   type Item is abstract tagged null record;


   procedure destruct (Self : in out Item)   is null;


   function calcPenDepth (Self : access Item;   simplexSolver    : access impact.d3.collision.simplex_Solver.Item'Class;
                                                convexA, convexB : in     impact.d3.Shape.convex.view;
                                                transA,  transB  : in     Transform_3d;
                                                v                : access math.Vector_3;
                                                pa,      pb      : access math.Vector_3) return Boolean
                          is abstract;


end impact.d3.collision.convex_penetration_depth_Solver;
