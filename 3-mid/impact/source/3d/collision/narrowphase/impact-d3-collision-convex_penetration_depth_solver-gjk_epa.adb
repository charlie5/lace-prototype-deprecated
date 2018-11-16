with impact.d3.collision.gjk_epa;

with impact.d3.Transform;
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.h"
--  #include "impact.d3.collision.convex_penetration_depth_Solver.gjk_epa.h"
--
--
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.gjk_epa.h"




package body impact.d3.collision.convex_penetration_depth_Solver.gjk_epa
is

   use impact.d3.collision.gjk_epa;




   overriding function calcPenDepth (Self : access Item;   simplexSolver            : access impact.d3.collision.simplex_Solver.Item'Class;
                                                pConvexA,    pConvexB    : in     impact.d3.Shape.convex.view;
                                                transformA,  transformB  : in     Transform_3d;
                                                v                        : access math.Vector_3;
                          wWitnessOnA, wWitnessOnB : access math.Vector_3) return Boolean
   is
      pragma Unreferenced (Self, simplexSolver);
      use impact.d3.Transform, math.Vectors;

      guessVector : constant math.Vector_3           := (getOrigin (transformA) - getOrigin (transformB));
      results     : aliased btGjkEpaSolver2.sResults;

   begin
      if btGjkEpaSolver2.Penetration (pConvexA,    transformA,
                                      pConvexB,    transformB,
                                      guessVector, results'Access)

      then
         --  resultOut->addContactPoint(results.normal,results.witnesses[1],-results.depth);
         wWitnessOnA.all := results.witnesses (1);
         wWitnessOnB.all := results.witnesses (2);
         v.all           := results.normal;

         return True;
      else

         if btGjkEpaSolver2.Distance (pConvexA, transformA,
                                      pConvexB, transformB,
                                      guessVector, results'Access)
         then
            wWitnessOnA.all := results.witnesses (1);
            wWitnessOnB.all := results.witnesses (2);
            v.all           := results.normal;

            return False;
         end if;
      end if;


      return False;
   end calcPenDepth;



end impact.d3.collision.convex_penetration_depth_Solver.gjk_epa;



--
--  bool impact.d3.collision.convex_penetration_depth_Solver.gjk_epa::calcPenDepth( impact.d3.collision.simplex_Solver& simplexSolver,
--                                                                                            const impact.d3.Shape.convex* pConvexA, const impact.d3.Shape.convex* pConvexB,
--                                                                                            const impact.d3.Transform& transformA, const impact.d3.Transform& transformB,
--                                                                                            impact.d3.Vector& v, impact.d3.Vector& wWitnessOnA, impact.d3.Vector& wWitnessOnB,
--                                                                                            class btIDebugDraw* debugDraw, btStackAlloc* stackAlloc )
--  {
--
--          (void)debugDraw;
--          (void)v;
--          (void)simplexSolver;
--
--  //        const impact.d3.Scalar                                radialmargin(impact.d3.Scalar(0.));
--
--          impact.d3.Vector        guessVector(transformA.getOrigin()-transformB.getOrigin());
--          btGjkEpaSolver2::sResults        results;
--
--
--          if(btGjkEpaSolver2::Penetration(pConvexA,transformA,
--                                                                  pConvexB,transformB,
--                                                                  guessVector,results))
--
--                  {
--          //        debugDraw->drawLine(results.witnesses[1],results.witnesses[1]+results.normal,impact.d3.Vector(255,0,0));
--                  //resultOut->addContactPoint(results.normal,results.witnesses[1],-results.depth);
--                  wWitnessOnA = results.witnesses[0];
--                  wWitnessOnB = results.witnesses[1];
--                  v = results.normal;
--                  return true;
--                  } else
--          {
--                  if(btGjkEpaSolver2::Distance(pConvexA,transformA,pConvexB,transformB,guessVector,results))
--                  {
--                          wWitnessOnA = results.witnesses[0];
--                          wWitnessOnB = results.witnesses[1];
--                          v = results.normal;
--                          return false;
--                  }
--          }
--
--          return false;
--  }


