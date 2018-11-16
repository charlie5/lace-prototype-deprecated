with impact.d3.collision.Detector.discrete;
with impact.d3.collision.convex_penetration_depth_Solver;
with impact.d3.collision.simplex_Solver;
with impact.d3.Shape.convex;
with impact.d3.collision.Proxy;

--  #include "impact.d3.collision.Detector.discrete.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.collision.Margin.h"
--
--  class impact.d3.Shape.convex;
--  #include "impact.d3.collision.simplex_Solver.h"
--  class impact.d3.collision.convex_penetration_depth_Solver;




package impact.d3.collision.Detector.discrete.gjk_pair
--
--  impact.d3.collision.Detector.discrete.gjk_pair uses GJK to implement the impact.d3.collision.Detector.discrete.
--
is

   type Item is new impact.d3.collision.Detector.discrete.item with private;




   --- Forge
   --

   function to_gjk_pair_Detector (objectA, objectB       : access impact.d3.Shape.convex.Item'Class;
                                  simplexSolver          : access impact.d3.collision.simplex_Solver.Item'Class;
                                  penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.Item'Class) return Item;



   function to_gjk_pair_Detector (objectA,    objectB    : access impact.d3.Shape.convex.Item'Class;
                                  shapeTypeA, shapeTypeB : in     impact.d3.collision.Proxy.BroadphaseNativeTypes;
                                  marginA,    marginB    : in     math.Real;
                                  simplexSolver          : access impact.d3.collision.simplex_Solver.Item'Class;
                                  penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.Item'Class) return Item;


   overriding procedure destruct (Self : in out Item)   is null;





   --- Attributes
   --


   procedure setMinkowskiA (Self :    out Item;   minkA : access impact.d3.Shape.convex.Item'Class);

--          void setMinkowskiA(impact.d3.Shape.convex* minkA)
--          {
--                  m_minkowskiA = minkA;
--          }


   procedure setMinkowskiB (Self :    out Item;   minkB : access impact.d3.Shape.convex.Item'Class);

--          void setMinkowskiB(impact.d3.Shape.convex* minkB)
--          {
--                  m_minkowskiB = minkB;
--          }




   procedure setCachedSeperatingAxis (Self :    out Item;   seperatingAxis : in math.Vector_3);

--          void setCachedSeperatingAxis(const impact.d3.Vector& seperatingAxis)
--          {
--                  m_cachedSeparatingAxis = seperatingAxis;
--          }



   function getCachedSeparatingAxis (Self : in Item) return math.Vector_3;

--          const impact.d3.Vector& getCachedSeparatingAxis() const
--          {
--                  return m_cachedSeparatingAxis;
--          }


   function getCachedSeparatingDistance (Self : in Item) return math.Real;

--          impact.d3.Scalar        getCachedSeparatingDistance() const
--          {
--                  return m_cachedSeparatingDistance;
--          }
--


   procedure setPenetrationDepthSolver (Self :    out Item;   penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.Item'Class);

--          void        setPenetrationDepthSolver(impact.d3.collision.convex_penetration_depth_Solver*        penetrationDepthSolver)
--          {
--                  m_penetrationDepthSolver = penetrationDepthSolver;
--          }



   procedure setIgnoreMargin (Self :    out Item;   ignoreMargin : in Boolean);
   --
   --  Don't use setIgnoreMargin, it's for Bullet's internal use.


--          void        setIgnoreMargin(bool ignoreMargin)
--          {
--                  m_ignoreMargin = ignoreMargin;
--          }
--





   --- Operations
   --

   overriding procedure getClosestPoints (Self : in out Item;   input       : in     impact.d3.collision.Detector.discrete.ClosestPointInput;
                                                     output      : in out impact.d3.collision.Detector.discrete.Result'Class;
                                                     swapResults : in     Boolean                                               := False);


   procedure getClosestPointsNonVirtual (Self : in out Item'Class;   input       : in     impact.d3.collision.Detector.discrete.ClosestPointInput;
                                                                     output      : in out impact.d3.collision.Detector.discrete.Result'Class);



private

   type Item is new impact.d3.collision.Detector.discrete.item with
      record
         m_cachedSeparatingAxis     : aliased math.Vector_3;

         m_penetrationDepthSolver   : access impact.d3.collision.convex_penetration_depth_Solver.Item'Class;
         m_simplexSolver            : access impact.d3.collision.simplex_Solver.Item'Class;

         m_minkowskiA,
         m_minkowskiB               : access impact.d3.Shape.convex.Item'Class;

         m_shapeTypeA,
         m_shapeTypeB               : impact.d3.collision.Proxy.BroadphaseNativeTypes;

         m_marginA                  : math.Real;
         m_marginB                  : math.Real;

         m_ignoreMargin             : Boolean;
         m_cachedSeparatingDistance : math.Real;

         --  some debugging to fix degeneracy problems
         --
         m_lastUsedMethod,
         m_curIter,
         m_degenerateSimplex        : Integer;
         m_catchDegeneracies        : Boolean;
      end record;




end impact.d3.collision.Detector.discrete.gjk_pair;
