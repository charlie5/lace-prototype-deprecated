with impact.d3.Transform;
with impact.d3.collision.point_Collector;
with impact.d3.collision.Detector.discrete.gjk_pair;
with impact.d3.collision.Detector.discrete;
with impact.d3.Vector;
--  #include "impact.d3.collision.convex_Raycast.gjk.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.internal.sphere.h"
--  #include "impact.d3.collision.Detector.discrete.gjk_pair.h"
--  #include "impact.d3.collision.point_Collector.h"
--  #include "LinearMath/impact.d3.TransformUtil.h"



package body impact.d3.collision.convex_Raycast.gjk
is


   ------------
   --- Globals
   --


   --  #ifdef BT_USE_DOUBLE_PRECISION
   MAX_ITERATIONS : constant := 64;
   --  #else
   --  #define MAX_ITERATIONS 32
   --  #endif






   ----------
   --- Forge
   --

   function to_gjk_convex_Raycast (convexA, convexB : access impact.d3.Shape.convex.Item'Class;
                                   simplexSolver    : access impact.d3.collision.simplex_Solver.Item'Class) return Item
   is
      Self : constant Item := (impact.d3.collision.convex_Raycast.item with
                      m_simplexSolver => simplexSolver,
                      m_convexA       => convexA,
                      m_convexB       => convexB);
   begin
      return Self;
   end to_gjk_convex_Raycast;





   ---------------
   --- Attributes
   --

   overriding function calcTimeOfImpact (Self : access Item;   fromA, toA : in     Transform_3d;
                                                    fromB, toB : in     Transform_3d;
                              result     : access impact.d3.collision.convex_Raycast.CastResult'Class) return Boolean
   is
      use impact.d3.Transform, math.Vectors;

      linVelA : constant math.Vector_3 := getOrigin (toA) - getOrigin (fromA);
      linVelB : constant math.Vector_3 := getOrigin (toB) - getOrigin (fromB);

      radius  : math.Real     := 0.001;
      lambda  : math.Real     := 0.0;

      v       : math.vector_3 := (1.0, 0.0, 0.0);

      maxIter : constant Integer := MAX_ITERATIONS;

      n         : math.Vector_3 := (0.0, 0.0, 0.0);
      hasResult : Boolean       := False;

      c         : math.Vector_3;
      r         : constant math.Vector_3 := linVelA - linVelB;

      lastLambda : math.Real := lambda;
      --  impact.d3.Scalar epsilon = impact.d3.Scalar(0.001);

      numIter : Integer := 0;
      --  first solution, using GJK

      identityTrans  : Transform_3d     := getIdentity;
      pointCollector : impact.d3.collision.point_Collector.item;

      gjk            : Detector.discrete.gjk_pair.item    := Detector.discrete.gjk_pair.to_gjk_pair_Detector (Self.m_convexA, Self.m_convexB, Self.m_simplexSolver, null);   -- m_penetrationDepthSolver);
      input          : Detector.discrete.ClosestPointInput;

      dist           : math.Real;

   begin
      Self.m_simplexSolver.reset;

      --  compute linear velocity for this interval, to interpolate
      --  assume no rotation/angular velocity, assert here?
      --

      --  we don't use margins during CCD
      --  gjk.setIgnoreMargin(true);

      input.m_transformA := fromA;
      input.m_transformB := fromB;

      gjk.getClosestPoints (input, pointCollector, False);

      hasResult := pointCollector.m_hasResult;
      c         := pointCollector.m_pointInWorld;

      if hasResult then
         dist := pointCollector.m_distance;
         n    := pointCollector.m_normalOnBInWorld;

         --  not close enough
         --
         while dist > radius
         loop
            numIter := numIter + 1;
            if numIter > maxIter then
               return False;      -- todo: report a failure
            end if;

            declare
               use impact.d3.Vector;
               dLambda                 : math.Real := 0.0;
               projectedLinearVelocity : constant math.Real := dot (r, n);
            begin
               dLambda := dist / projectedLinearVelocity;
               lambda  := lambda - dLambda;

               if lambda > 1.0 then
                  return False;
               end if;

               if lambda < 0.0 then
                  return False;
               end if;

               --  todo: next check with relative epsilon

               if lambda <= lastLambda then
                  return False;
                  --  n.setValue(0,0,0);
                  --  exit;
               end if;

               lastLambda := lambda;

               --  interpolate to next lambda
               --
               setInterpolate3 (getOrigin (input.m_transformA'Access).all,  getOrigin (fromA),  getOrigin (toA),  lambda);
               setInterpolate3 (getOrigin (input.m_transformB'Access).all,  getOrigin (fromB),  getOrigin (toB),  lambda);

               gjk.getClosestPoints (input, pointCollector, False);

               if pointCollector.m_hasResult then

                  if pointCollector.m_distance < 0.0 then

                     result.m_fraction := lastLambda;
                     n                 := pointCollector.m_normalOnBInWorld;
                     result.m_normal   := n;
                     result.m_hitPoint := pointCollector.m_pointInWorld;

                     return True;
                  end if;

                  c    := pointCollector.m_pointInWorld;
                  n    := pointCollector.m_normalOnBInWorld;
                  dist := pointCollector.m_distance;

               else
                  return False;     -- ??
               end if;

            end;
         end loop;

         --  is n normalized?
         --  don't report time of impact for motion away from the contact normal (or causes minor penetration)
         --
         if impact.d3.Vector.dot (n, r) >= -result.m_allowedPenetration then
            return False;
         end if;

         result.m_fraction := lambda;
         result.m_normal   := n;
         result.m_hitPoint := c;

         return True;
      end if;


      return False;
   end calcTimeOfImpact;

end impact.d3.collision.convex_Raycast.gjk;





--
--
--
--  bool        impact.d3.collision.convex_Raycast.gjk::calcTimeOfImpact(
--                                          const impact.d3.Transform& fromA,
--                                          const impact.d3.Transform& toA,
--                                          const impact.d3.Transform& fromB,
--                                          const impact.d3.Transform& toB,
--                                          CastResult& result)
--  {
--
--
--          m_simplexSolver->reset();
--
--          /// compute linear velocity for this interval, to interpolate
--          //assume no rotation/angular velocity, assert here?
--          impact.d3.Vector linVelA,linVelB;
--          linVelA = toA.getOrigin()-fromA.getOrigin();
--          linVelB = toB.getOrigin()-fromB.getOrigin();
--
--          impact.d3.Scalar radius = impact.d3.Scalar(0.001);
--          impact.d3.Scalar lambda = impact.d3.Scalar(0.);
--          impact.d3.Vector v(1,0,0);
--
--          int maxIter = MAX_ITERATIONS;
--
--          impact.d3.Vector n;
--          n.setValue(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.));
--          bool hasResult = false;
--          impact.d3.Vector c;
--          impact.d3.Vector r = (linVelA-linVelB);
--
--          impact.d3.Scalar lastLambda = lambda;
--          //impact.d3.Scalar epsilon = impact.d3.Scalar(0.001);
--
--          int numIter = 0;
--          //first solution, using GJK
--
--
--          impact.d3.Transform identityTrans;
--          identityTrans.setIdentity();
--
--
--  //        result.drawCoordSystem(sphereTr);
--
--          impact.d3.collision.point_Collector        pointCollector;
--
--
--          impact.d3.collision.Detector.discrete.gjk_pair gjk(m_convexA,m_convexB,m_simplexSolver,0);//m_penetrationDepthSolver);
--          impact.d3.collision.Detector.discrete.gjk_pair::ClosestPointInput input;
--
--          //we don't use margins during CCD
--          //        gjk.setIgnoreMargin(true);
--
--          input.m_transformA = fromA;
--          input.m_transformB = fromB;
--          gjk.getClosestPoints(input,pointCollector,0);
--
--          hasResult = pointCollector.m_hasResult;
--          c = pointCollector.m_pointInWorld;
--
--          if (hasResult)
--          {
--                  impact.d3.Scalar dist;
--                  dist = pointCollector.m_distance;
--                  n = pointCollector.m_normalOnBInWorld;
--
--
--
--                  //not close enough
--                  while (dist > radius)
--                  {
--                          numIter++;
--                          if (numIter > maxIter)
--                          {
--                                  return false; //todo: report a failure
--                          }
--                          impact.d3.Scalar dLambda = impact.d3.Scalar(0.);
--
--                          impact.d3.Scalar projectedLinearVelocity = r.dot(n);
--
--                          dLambda = dist / (projectedLinearVelocity);
--
--                          lambda = lambda - dLambda;
--
--                          if (lambda > impact.d3.Scalar(1.))
--                                  return false;
--
--                          if (lambda < impact.d3.Scalar(0.))
--                                  return false;
--
--                          //todo: next check with relative epsilon
--                          if (lambda <= lastLambda)
--                          {
--                                  return false;
--                                  //n.setValue(0,0,0);
--                                  break;
--                          }
--                          lastLambda = lambda;
--
--                          //interpolate to next lambda
--                          result.DebugDraw( lambda );
--                          input.m_transformA.getOrigin().setInterpolate3(fromA.getOrigin(),toA.getOrigin(),lambda);
--                          input.m_transformB.getOrigin().setInterpolate3(fromB.getOrigin(),toB.getOrigin(),lambda);
--
--                          gjk.getClosestPoints(input,pointCollector,0);
--                          if (pointCollector.m_hasResult)
--                          {
--                                  if (pointCollector.m_distance < impact.d3.Scalar(0.))
--                                  {
--                                          result.m_fraction = lastLambda;
--                                          n = pointCollector.m_normalOnBInWorld;
--                                          result.m_normal=n;
--                                          result.m_hitPoint = pointCollector.m_pointInWorld;
--                                          return true;
--                                  }
--                                  c = pointCollector.m_pointInWorld;
--                                  n = pointCollector.m_normalOnBInWorld;
--                                  dist = pointCollector.m_distance;
--                          } else
--                          {
--                                  //??
--                                  return false;
--                          }
--
--                  }
--
--                  //is n normalized?
--                  //don't report time of impact for motion away from the contact normal (or causes minor penetration)
--                  if (n.dot(r)>=-result.m_allowedPenetration)
--                          return false;
--
--                  result.m_fraction = lambda;
--                  result.m_normal = n;
--                  result.m_hitPoint = c;
--                  return true;
--          }
--
--          return false;
--
--
--  }

