with impact.d3.collision.Detector.discrete;
with impact.d3.Transform;
with impact.d3.Vector;
with impact.d3.collision.Detector.discrete.gjk_pair;


--  #include "impact.d3.collision.convex_penetration_depth_Solver.minkowski.h"
--  #include "BulletCollision/NarrowPhaseCollision/btSubSimplexConvexCast.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.simplex_Solver.voronoi.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.Detector.discrete.gjk_pair.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.h"



package body impact.d3.collision.convex_penetration_depth_Solver.minkowski
is

   NUM_UNITSPHERE_POINTS : constant := 42;






   --- btIntermediateResult
   --

   type btIntermediateResult is new impact.d3.collision.Detector.discrete.Result with
      record
         m_normalOnBInWorld,
         m_pointInWorld    : math.Vector_3;

         m_depth           : math.Real;
         m_hasResult       : Boolean  := False;
      end record;


   overriding procedure addContactPoint      (Self : in out btIntermediateResult;   normalOnBInWorld : in math.Vector_3;
                                                                         pointInWorld     : in math.Vector_3;
                                                                         depth            : in math.Real  )
   is
   begin
      Self.m_normalOnBInWorld := normalOnBInWorld;
      Self.m_pointInWorld     := pointInWorld;
      Self.m_depth            := depth;
      Self.m_hasResult        := True;
   end addContactPoint;







   --- Operations
   --

   overriding function calcPenDepth (Self : access Item;   simplexSolver    : access impact.d3.collision.simplex_Solver.Item'Class;
                                                convexA, convexB : in     impact.d3.Shape.convex.view;
                                                transA,  transB  : in     Transform_3d;
                                                v                : access math.Vector_3;
                          pa,      pb      : access math.Vector_3) return Boolean
   is
      pragma Unreferenced (Self);
      use impact.d3.Transform;

      check2d                : constant Boolean :=          convexA.isConvex2d
                                          and then convexB.isConvex2d;

      --  just take fixed number of orientation, and sample the penetration depth in that direction
      --
      minProj                : math.Real := BT_LARGE_FLOAT;

      minNorm                : math.Vector_3 := (0.0, 0.0, 0.0);
      minA, minB             : math.Vector_3;

      seperatingAxisInA,
      seperatingAxisInB      : math.Vector_3;
      pragma Unreferenced (seperatingAxisInA, seperatingAxisInB);

      pInA, qInB,
      pWorld, qWorld,
      w                      : math.Vector_3;

      supportVerticesABatch  : Vector_3_array (1 .. NUM_UNITSPHERE_POINTS + impact.d3.Shape.convex.MAX_PREFERRED_PENETRATION_DIRECTIONS * 2);
      supportVerticesBBatch  : Vector_3_array (1 .. NUM_UNITSPHERE_POINTS + impact.d3.Shape.convex.MAX_PREFERRED_PENETRATION_DIRECTIONS * 2);
      seperatingAxisInABatch : Vector_3_array (1 .. NUM_UNITSPHERE_POINTS + impact.d3.Shape.convex.MAX_PREFERRED_PENETRATION_DIRECTIONS * 2);
      seperatingAxisInBBatch : Vector_3_array (1 .. NUM_UNITSPHERE_POINTS + impact.d3.Shape.convex.MAX_PREFERRED_PENETRATION_DIRECTIONS * 2);

      numSampleDirections    : Integer := NUM_UNITSPHERE_POINTS;

   begin

      for i in 1 .. numSampleDirections
      loop
         declare
            norm : constant math.Vector_3 := getPenetrationDirections (i).all;
         begin
            seperatingAxisInABatch (i) := (-norm) * getBasis (transA);
            seperatingAxisInBBatch (i) :=   norm  * getBasis (transB);
         end;
      end loop;


      declare
         numPDA : constant Integer      := convexA.getNumPreferredPenetrationDirections;
         norm   : math.Vector_3;
      begin
         if numPDA /= 0 then

            for i in 1 .. numPDA
            loop
               convexA.getPreferredPenetrationDirection (i,  norm);
               norm := getBasis (transA) * norm;

               getPenetrationDirections (numSampleDirections).all :=   norm;
               seperatingAxisInABatch   (numSampleDirections) := (-norm) * getBasis (transA);
               seperatingAxisInBBatch   (numSampleDirections) :=   norm  * getBasis (transB);

               numSampleDirections := numSampleDirections + 1;
            end loop;

         end if;
      end;


      declare
         numPDB : constant Integer      := convexB.getNumPreferredPenetrationDirections;
         norm   : math.Vector_3;
      begin
         if numPDB /= 0 then

            for i in 1 .. numPDB
            loop
               convexB.getPreferredPenetrationDirection (i,  norm);
               norm := getBasis (transB) * norm;

               getPenetrationDirections (numSampleDirections).all :=   norm;
               seperatingAxisInABatch   (numSampleDirections) := (-norm) * getBasis (transA);
               seperatingAxisInBBatch   (numSampleDirections) :=   norm  * getBasis (transB);

               numSampleDirections := numSampleDirections + 1;
            end loop;

         end if;
      end;


      convexA.batchedUnitVectorGetSupportingVertexWithoutMargin (seperatingAxisInABatch, supportVerticesABatch, numSampleDirections);
      convexB.batchedUnitVectorGetSupportingVertexWithoutMargin (seperatingAxisInBBatch, supportVerticesBBatch, numSampleDirections);

      for i in 1 .. numSampleDirections
      loop
         declare
            use linear_Algebra_3d,  impact.d3.Vector;
            norm      : math.Vector_3 := getPenetrationDirections (i).all;
            the_delta : math.Real;

         begin
            if check2d then
               norm (3) := 0.0;
            end if;


            if length2 (norm)  >  0.01 then

               seperatingAxisInA := seperatingAxisInABatch (i);
               seperatingAxisInB := seperatingAxisInBBatch (i);

               pInA := supportVerticesABatch (i);
               qInB := supportVerticesBBatch (i);

               pWorld := transA * pInA;
               qWorld := transB * qInB;

               if check2d then
                  pWorld (3) := 0.0;
                  qWorld (3) := 0.0;
               end if;

               w         := qWorld - pWorld;
               the_delta := dot (norm, w);

               --  find smallest delta
               if the_delta < minProj then
                  minProj := the_delta;
                  minNorm := norm;
                  minA    := pWorld;
                  minB    := qWorld;
               end if;

            end if;

         end;
      end loop;




      --  add the margins
      --
      minA := minA  +  minNorm * convexA.getMarginNonVirtual;
      minB := minB  -  minNorm * convexB.getMarginNonVirtual;

      --  no penetration
      if minProj < 0.0 then
         return False;
      end if;


      minProj := minProj  +  0.5     -- 'extraSeparation' ~ scale dependent
                          + (convexA.getMarginNonVirtual + convexB.getMarginNonVirtual);

      declare
         use impact.d3.collision.Detector.discrete.gjk_pair;
         gjkdet           : impact.d3.collision.Detector.discrete.gjk_pair.item := to_gjk_pair_Detector (convexA, convexB, simplexSolver, null);

         offsetDist       : constant math.Real         := minProj;
         offset           : constant math.Vector_3     := minNorm * offsetDist;

         input            : impact.d3.collision.Detector.discrete.ClosestPointInput;

         newOrg           : constant math.Vector_3     := getOrigin (transA) + offset;
         displacedTrans   : Transform_3d  := transA;

         penetration_relaxation : constant math.Real := 1.0;

         res                    : btIntermediateResult;
         correctedMinNorm       : math.Real;

      begin
         setOrigin (displacedTrans,  newOrg);

         input.m_transformA := displacedTrans;
         input.m_transformB := transB;

         input.m_maximumDistanceSquared := BT_LARGE_FLOAT;  -- minProj;

         gjkdet.setCachedSeperatingAxis (-minNorm);
         gjkdet.getClosestPoints        (input, res);

         correctedMinNorm := minProj - res.m_depth;
         minNorm          := minNorm * penetration_relaxation;   -- the penetration depth is over-estimated, relax it


         if res.m_hasResult then
            pa.all := res.m_pointInWorld - minNorm * correctedMinNorm;
            pb.all := res.m_pointInWorld;
            v .all := minNorm;
         end if;


         return res.m_hasResult;
      end;
   end calcPenDepth;







   sPenetrationDirections : Vector_3_array (1 .. NUM_UNITSPHERE_POINTS + impact.d3.Shape.convex.MAX_PREFERRED_PENETRATION_DIRECTIONS*2)
     :=        ((0.000000, -0.000000, -1.000000),   -- these defaults are for UNITSPHERE_POINTS !
         (0.723608, -0.525725, -0.447219),
         (-0.276388, -0.850649, -0.447219),
         (-0.894426, -0.000000, -0.447216),
         (-0.276388,  0.850649, -0.447220),
         (0.723608,  0.525725, -0.447219),
         (0.276388, -0.850649,  0.447220),
         (-0.723608, -0.525725,  0.447219),
         (-0.723608,  0.525725,  0.447219),
         (0.276388,  0.850649,  0.447219),
         (0.894426,  0.000000,  0.447216),
         (-0.000000,  0.000000,  1.000000),
         (0.425323, -0.309011, -0.850654),
         (-0.162456, -0.499995, -0.850654),
         (0.262869, -0.809012, -0.525738),
         (0.425323,  0.309011, -0.850654),
         (0.850648, -0.000000, -0.525736),
         (-0.525730, -0.000000, -0.850652),
         (-0.688190, -0.499997, -0.525736),
         (-0.162456,  0.499995, -0.850654),
         (-0.688190,  0.499997, -0.525736),
         (0.262869,  0.809012, -0.525738),
         (0.951058,  0.309013,  0.000000),
         (0.951058, -0.309013,  0.000000),
         (0.587786, -0.809017,  0.000000),
         (0.000000, -1.000000,  0.000000),
         (-0.587786, -0.809017,  0.000000),
         (-0.951058, -0.309013, -0.000000),
         (-0.951058,  0.309013, -0.000000),
         (-0.587786,  0.809017, -0.000000),
         (-0.000000,  1.000000, -0.000000),
         (0.587786,  0.809017, -0.000000),
         (0.688190, -0.499997,  0.525736),
         (-0.262869, -0.809012,  0.525738),
         (-0.850648,  0.000000,  0.525736),
         (-0.262869,  0.809012,  0.525738),
         (0.688190,  0.499997,  0.525736),
         (0.525730,  0.000000,  0.850652),
         (0.162456, -0.499995,  0.850654),
         (-0.425323, -0.309011,  0.850654),
         (-0.425323,  0.309011,  0.850654),
         (0.162456,  0.499995,  0.850654),
         others => (0.0,       0.0,       0.0   ));   -- remaining ones following 1 .. UNITSPHERE_POINTS.




   function getPenetrationDirections (i : in Integer) return access math.Vector_3
   is
   begin
      return sPenetrationDirections (i)'Access;
   end getPenetrationDirections;




end impact.d3.collision.convex_penetration_depth_Solver.minkowski;


--  bool impact.d3.collision.convex_penetration_depth_Solver.minkowski::calcPenDepth(impact.d3.collision.simplex_Solver& simplexSolver,
--                                                                                                     const impact.d3.Shape.convex* convexA,const impact.d3.Shape.convex* convexB,
--                                                                                                     const impact.d3.Transform& transA,const impact.d3.Transform& transB,
--                                                                                                     impact.d3.Vector& v, impact.d3.Vector& pa, impact.d3.Vector& pb,
--                                                                                                     class btIDebugDraw* debugDraw,btStackAlloc* stackAlloc
--                                                                                                     )
--  {
--
--          (void)stackAlloc;
--          (void)v;
--
--          bool check2d= convexA->isConvex2d() && convexB->isConvex2d();
--
--          //just take fixed number of orientation, and sample the penetration depth in that direction
--          impact.d3.Scalar minProj = impact.d3.Scalar(BT_LARGE_FLOAT);
--          impact.d3.Vector minNorm(impact.d3.Scalar(0.), impact.d3.Scalar(0.), impact.d3.Scalar(0.));
--          impact.d3.Vector minA,minB;
--          impact.d3.Vector seperatingAxisInA,seperatingAxisInB;
--          impact.d3.Vector pInA,qInB,pWorld,qWorld,w;
--



--
--          impact.d3.Vector        supportVerticesABatch[NUM_UNITSPHERE_POINTS+MAX_PREFERRED_PENETRATION_DIRECTIONS*2];
--          impact.d3.Vector        supportVerticesBBatch[NUM_UNITSPHERE_POINTS+MAX_PREFERRED_PENETRATION_DIRECTIONS*2];
--          impact.d3.Vector        seperatingAxisInABatch[NUM_UNITSPHERE_POINTS+MAX_PREFERRED_PENETRATION_DIRECTIONS*2];
--          impact.d3.Vector        seperatingAxisInBBatch[NUM_UNITSPHERE_POINTS+MAX_PREFERRED_PENETRATION_DIRECTIONS*2];
--          int i;
--
--          int numSampleDirections = NUM_UNITSPHERE_POINTS;
--
--          for (i=0;i<numSampleDirections;i++)
--          {
--                  impact.d3.Vector norm = getPenetrationDirections()[i];
--                  seperatingAxisInABatch[i] =  (-norm) * transA.getBasis() ;
--                  seperatingAxisInBBatch[i] =  norm   * transB.getBasis() ;
--          }
--
--          {
--                  int numPDA = convexA->getNumPreferredPenetrationDirections();
--                  if (numPDA)
--                  {
--                          for (int i=0;i<numPDA;i++)
--                          {
--                                  impact.d3.Vector norm;
--                                  convexA->getPreferredPenetrationDirection(i,norm);
--                                  norm  = transA.getBasis() * norm;
--                                  getPenetrationDirections()[numSampleDirections] = norm;
--                                  seperatingAxisInABatch[numSampleDirections] = (-norm) * transA.getBasis();
--                                  seperatingAxisInBBatch[numSampleDirections] = norm * transB.getBasis();
--                                  numSampleDirections++;
--                          }
--                  }
--          }
--
--          {
--                  int numPDB = convexB->getNumPreferredPenetrationDirections();
--                  if (numPDB)
--                  {
--                          for (int i=0;i<numPDB;i++)
--                          {
--                                  impact.d3.Vector norm;
--                                  convexB->getPreferredPenetrationDirection(i,norm);
--                                  norm  = transB.getBasis() * norm;
--                                  getPenetrationDirections()[numSampleDirections] = norm;
--                                  seperatingAxisInABatch[numSampleDirections] = (-norm) * transA.getBasis();
--                                  seperatingAxisInBBatch[numSampleDirections] = norm * transB.getBasis();
--                                  numSampleDirections++;
--                          }
--                  }
--          }
--
--
--
--
--          convexA->batchedUnitVectorGetSupportingVertexWithoutMargin(seperatingAxisInABatch,supportVerticesABatch,numSampleDirections);
--          convexB->batchedUnitVectorGetSupportingVertexWithoutMargin(seperatingAxisInBBatch,supportVerticesBBatch,numSampleDirections);
--
--          for (i=0;i<numSampleDirections;i++)
--          {
--                  impact.d3.Vector norm = getPenetrationDirections()[i];
--                  if (check2d)
--                  {
--                          norm[2] = 0.f;
--                  }
--                  if (norm.length2()>0.01)
--                  {
--
--                          seperatingAxisInA = seperatingAxisInABatch[i];
--                          seperatingAxisInB = seperatingAxisInBBatch[i];
--
--                          pInA = supportVerticesABatch[i];
--                          qInB = supportVerticesBBatch[i];
--
--                          pWorld = transA(pInA);
--                          qWorld = transB(qInB);
--                          if (check2d)
--                          {
--                                  pWorld[2] = 0.f;
--                                  qWorld[2] = 0.f;
--                          }
--
--                          w        = qWorld - pWorld;
--                          impact.d3.Scalar delta = norm.dot(w);
--                          //find smallest delta
--                          if (delta < minProj)
--                          {
--                                  minProj = delta;
--                                  minNorm = norm;
--                                  minA = pWorld;
--                                  minB = qWorld;
--                          }
--                  }
--          }



--
--          //add the margins
--
--          minA += minNorm*convexA->getMarginNonVirtual();
--          minB -= minNorm*convexB->getMarginNonVirtual();
--          //no penetration
--          if (minProj < impact.d3.Scalar(0.))
--                  return false;
--
--          impact.d3.Scalar extraSeparation = 0.5f;///scale dependent
--          minProj += extraSeparation+(convexA->getMarginNonVirtual() + convexB->getMarginNonVirtual());
--
--
--
--          impact.d3.collision.Detector.discrete.gjk_pair gjkdet(convexA,convexB,&simplexSolver,0);
--
--          impact.d3.Scalar offsetDist = minProj;
--          impact.d3.Vector offset = minNorm * offsetDist;
--
--
--
--          impact.d3.collision.Detector.discrete.gjk_pair::ClosestPointInput input;
--
--          impact.d3.Vector newOrg = transA.getOrigin() + offset;
--
--          impact.d3.Transform displacedTrans = transA;
--          displacedTrans.setOrigin(newOrg);
--
--          input.m_transformA = displacedTrans;
--          input.m_transformB = transB;
--          input.m_maximumDistanceSquared = impact.d3.Scalar(BT_LARGE_FLOAT);//minProj;
--
--          btIntermediateResult res;
--          gjkdet.setCachedSeperatingAxis(-minNorm);
--          gjkdet.getClosestPoints(input,res,debugDraw);
--
--          impact.d3.Scalar correctedMinNorm = minProj - res.m_depth;
--
--
--          //the penetration depth is over-estimated, relax it
--          impact.d3.Scalar penetration_relaxation= impact.d3.Scalar(1.);
--          minNorm*=penetration_relaxation;
--
--
--          if (res.m_hasResult)
--          {
--
--                  pa = res.m_pointInWorld - minNorm * correctedMinNorm;
--                  pb = res.m_pointInWorld;
--                  v = minNorm;
--
--          }
--          return res.m_hasResult;
--  }











--  impact.d3.Vector*        impact.d3.collision.convex_penetration_depth_Solver.minkowski::getPenetrationDirections()
--  {
--          static impact.d3.Vector        sPenetrationDirections[NUM_UNITSPHERE_POINTS+MAX_PREFERRED_PENETRATION_DIRECTIONS*2] =
--          {
--          impact.d3.Vector(impact.d3.Scalar(0.000000) , impact.d3.Scalar(-0.000000),impact.d3.Scalar(-1.000000)),
--          impact.d3.Vector(impact.d3.Scalar(0.723608) , impact.d3.Scalar(-0.525725),impact.d3.Scalar(-0.447219)),
--          impact.d3.Vector(impact.d3.Scalar(-0.276388) , impact.d3.Scalar(-0.850649),impact.d3.Scalar(-0.447219)),
--          impact.d3.Vector(impact.d3.Scalar(-0.894426) , impact.d3.Scalar(-0.000000),impact.d3.Scalar(-0.447216)),
--          impact.d3.Vector(impact.d3.Scalar(-0.276388) , impact.d3.Scalar(0.850649),impact.d3.Scalar(-0.447220)),
--          impact.d3.Vector(impact.d3.Scalar(0.723608) , impact.d3.Scalar(0.525725),impact.d3.Scalar(-0.447219)),
--          impact.d3.Vector(impact.d3.Scalar(0.276388) , impact.d3.Scalar(-0.850649),impact.d3.Scalar(0.447220)),
--          impact.d3.Vector(impact.d3.Scalar(-0.723608) , impact.d3.Scalar(-0.525725),impact.d3.Scalar(0.447219)),
--          impact.d3.Vector(impact.d3.Scalar(-0.723608) , impact.d3.Scalar(0.525725),impact.d3.Scalar(0.447219)),
--          impact.d3.Vector(impact.d3.Scalar(0.276388) , impact.d3.Scalar(0.850649),impact.d3.Scalar(0.447219)),
--          impact.d3.Vector(impact.d3.Scalar(0.894426) , impact.d3.Scalar(0.000000),impact.d3.Scalar(0.447216)),
--          impact.d3.Vector(impact.d3.Scalar(-0.000000) , impact.d3.Scalar(0.000000),impact.d3.Scalar(1.000000)),
--          impact.d3.Vector(impact.d3.Scalar(0.425323) , impact.d3.Scalar(-0.309011),impact.d3.Scalar(-0.850654)),
--          impact.d3.Vector(impact.d3.Scalar(-0.162456) , impact.d3.Scalar(-0.499995),impact.d3.Scalar(-0.850654)),
--          impact.d3.Vector(impact.d3.Scalar(0.262869) , impact.d3.Scalar(-0.809012),impact.d3.Scalar(-0.525738)),
--          impact.d3.Vector(impact.d3.Scalar(0.425323) , impact.d3.Scalar(0.309011),impact.d3.Scalar(-0.850654)),
--          impact.d3.Vector(impact.d3.Scalar(0.850648) , impact.d3.Scalar(-0.000000),impact.d3.Scalar(-0.525736)),
--          impact.d3.Vector(impact.d3.Scalar(-0.525730) , impact.d3.Scalar(-0.000000),impact.d3.Scalar(-0.850652)),
--          impact.d3.Vector(impact.d3.Scalar(-0.688190) , impact.d3.Scalar(-0.499997),impact.d3.Scalar(-0.525736)),
--          impact.d3.Vector(impact.d3.Scalar(-0.162456) , impact.d3.Scalar(0.499995),impact.d3.Scalar(-0.850654)),
--          impact.d3.Vector(impact.d3.Scalar(-0.688190) , impact.d3.Scalar(0.499997),impact.d3.Scalar(-0.525736)),
--          impact.d3.Vector(impact.d3.Scalar(0.262869) , impact.d3.Scalar(0.809012),impact.d3.Scalar(-0.525738)),
--          impact.d3.Vector(impact.d3.Scalar(0.951058) , impact.d3.Scalar(0.309013),impact.d3.Scalar(0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(0.951058) , impact.d3.Scalar(-0.309013),impact.d3.Scalar(0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(0.587786) , impact.d3.Scalar(-0.809017),impact.d3.Scalar(0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(0.000000) , impact.d3.Scalar(-1.000000),impact.d3.Scalar(0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(-0.587786) , impact.d3.Scalar(-0.809017),impact.d3.Scalar(0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(-0.951058) , impact.d3.Scalar(-0.309013),impact.d3.Scalar(-0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(-0.951058) , impact.d3.Scalar(0.309013),impact.d3.Scalar(-0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(-0.587786) , impact.d3.Scalar(0.809017),impact.d3.Scalar(-0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(-0.000000) , impact.d3.Scalar(1.000000),impact.d3.Scalar(-0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(0.587786) , impact.d3.Scalar(0.809017),impact.d3.Scalar(-0.000000)),
--          impact.d3.Vector(impact.d3.Scalar(0.688190) , impact.d3.Scalar(-0.499997),impact.d3.Scalar(0.525736)),
--          impact.d3.Vector(impact.d3.Scalar(-0.262869) , impact.d3.Scalar(-0.809012),impact.d3.Scalar(0.525738)),
--          impact.d3.Vector(impact.d3.Scalar(-0.850648) , impact.d3.Scalar(0.000000),impact.d3.Scalar(0.525736)),
--          impact.d3.Vector(impact.d3.Scalar(-0.262869) , impact.d3.Scalar(0.809012),impact.d3.Scalar(0.525738)),
--          impact.d3.Vector(impact.d3.Scalar(0.688190) , impact.d3.Scalar(0.499997),impact.d3.Scalar(0.525736)),
--          impact.d3.Vector(impact.d3.Scalar(0.525730) , impact.d3.Scalar(0.000000),impact.d3.Scalar(0.850652)),
--          impact.d3.Vector(impact.d3.Scalar(0.162456) , impact.d3.Scalar(-0.499995),impact.d3.Scalar(0.850654)),
--          impact.d3.Vector(impact.d3.Scalar(-0.425323) , impact.d3.Scalar(-0.309011),impact.d3.Scalar(0.850654)),
--          impact.d3.Vector(impact.d3.Scalar(-0.425323) , impact.d3.Scalar(0.309011),impact.d3.Scalar(0.850654)),
--          impact.d3.Vector(impact.d3.Scalar(0.162456) , impact.d3.Scalar(0.499995),impact.d3.Scalar(0.850654))
--          };
--
--          return sPenetrationDirections;
--  }


