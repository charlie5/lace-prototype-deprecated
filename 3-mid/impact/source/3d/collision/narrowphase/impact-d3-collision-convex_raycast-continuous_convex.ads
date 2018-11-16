with impact.d3.collision.convex_Raycast,
     impact.d3.collision.simplex_Solver,
     impact.d3.collision.convex_penetration_depth_Solver,
     impact.d3.Shape.convex,
     impact.d3.Shape.concave.static_plane,
     impact.d3.collision.point_Collector;



package impact.d3.collision.convex_Raycast.continuous_convex
--
--  impact.d3.collision.convex_Raycast.continuous_convex implements angular and linear time of impact for convex objects.
--
--  Based on Brian Mirtich's Conservative Advancement idea (PhD thesis).
--
--  Algorithm operates in worldspace, in order to keep inbetween motion globally consistent.
--  It uses GJK at the moment. Future improvement would use minkowski sum / supporting vertex, merging innerloops
--
is

   type Item is new impact.d3.collision.convex_Raycast.Item with private;




   --- Forge
   --

   function to_convex_Raycast (shapeA, shapeB         : access impact.d3.Shape.convex.Item'Class;
                               simplexSolver          : access impact.d3.collision.simplex_Solver.item'Class;
                               penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.item'Class) return Item;


   function to_convex_Raycast (shapeA                 : access impact.d3.Shape.convex.Item'Class;
                               plane                  : access impact.d3.Shape.concave.static_plane.item'Class) return Item;




   --- Attributes
   --

   overriding function calcTimeOfImpact (Self : access Item;   fromA, toA : in     Transform_3d;
                                                    fromB, toB : in     Transform_3d;
                              result     : access impact.d3.collision.convex_Raycast.CastResult'Class) return Boolean;





private

   type Item is new  impact.d3.collision.convex_Raycast.Item with
      record
         m_simplexSolver          : access impact.d3.collision.simplex_Solver      .Item'Class;
         m_penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.Item'Class;

         m_convexA                : access impact.d3.Shape.convex                 .Item'Class;
         m_convexB1               : access impact.d3.Shape.convex                 .Item'Class;    -- second object is either a convex or a plane (code sharing)

         m_planeShape             : access impact.d3.Shape.concave.static_plane            .Item'Class;
      end record;



   procedure computeClosestPoints (Self : in out Item;   transA, transB : in     Transform_3d;
                                                         pointCollector : in out impact.d3.collision.point_Collector.item);



end impact.d3.collision.convex_Raycast.continuous_convex;
