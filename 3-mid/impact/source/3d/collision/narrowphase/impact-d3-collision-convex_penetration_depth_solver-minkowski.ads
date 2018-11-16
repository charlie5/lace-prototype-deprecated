with impact.d3.collision.convex_penetration_depth_Solver;
with impact.d3.collision.simplex_Solver;
with impact.d3.Shape.convex;



package impact.d3.collision.convex_penetration_depth_Solver.minkowski
--
--  MinkowskiPenetrationDepthSolver implements bruteforce penetration depth estimation.
--
--  Implementation is based on sampling the depth using support mapping, and using GJK step to get the witness points.
--
is


   type Item is new impact.d3.collision.convex_penetration_depth_Solver.Item with private;



   overriding function calcPenDepth (Self : access Item;   simplexSolver    : access impact.d3.collision.simplex_Solver.Item'Class;
                                                convexA, convexB : in     impact.d3.Shape.convex.view;
                                                transA,  transB  : in     Transform_3d;
                                                v                : access math.Vector_3;
                                                pa,      pb      : access math.Vector_3) return Boolean;




private

   type Item is new impact.d3.collision.convex_penetration_depth_Solver.Item with
      record
         null;
      end record;


   function getPenetrationDirections (i : in Integer) return access math.Vector_3;



end impact.d3.collision.convex_penetration_depth_Solver.minkowski;
