with impact.d3.collision.convex_penetration_depth_Solver,
     impact.d3.collision.simplex_Solver,
     impact.d3.Shape.convex;



package impact.d3.collision.convex_penetration_depth_Solver.gjk_epa
--
--  EpaPenetrationDepthSolver uses the Expanding Polytope Algorithm to
--  calculate the penetration depth between two convex shapes.
--
is



   type Item is new impact.d3.collision.convex_penetration_depth_Solver.item with
      record
         null;
      end record;



   overriding function calcPenDepth (Self : access Item;   simplexSolver            : access impact.d3.collision.simplex_Solver.Item'Class;
                                                pConvexA,    pConvexB    : in     impact.d3.Shape.convex.view;
                                                transformA,  transformB  : in     Transform_3d;
                                                v                        : access math.Vector_3;
                                                wWitnessOnA, wWitnessOnB : access math.Vector_3) return Boolean;




end impact.d3.collision.convex_penetration_depth_Solver.gjk_epa;


