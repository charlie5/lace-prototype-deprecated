with impact.d3.Transform;
with impact.d3.Vector;
with impact.d3.Transform_Util;
with impact.d3.Scalar;

with impact.d3.collision.Detector.discrete.gjk_pair;
with impact.d3.collision.Detector.discrete;


--  #include "impact.d3.collision.convex_Raycast.continuous_convex.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.simplex_Solver.h"
--  #include "LinearMath/impact.d3.TransformUtil.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.internal.sphere.h"
--
--  #include "impact.d3.collision.Detector.discrete.gjk_pair.h"
--  #include "impact.d3.collision.point_Collector.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.concave.static_plane.h"

package body impact.d3.collision.convex_Raycast.continuous_convex
is

   --- Forge
   --

   function to_convex_Raycast (shapeA, shapeB         : access impact.d3.Shape.convex.Item'Class;
                               simplexSolver          : access impact.d3.collision.simplex_Solver.item'Class;
                               penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.item'Class) return Item
   is
      Self : Item;
   begin
      Self.m_simplexSolver          := simplexSolver;
      Self.m_penetrationDepthSolver := penetrationDepthSolver;
      Self.m_convexA                := shapeA;
      Self.m_convexB1               := shapeB;

      return Self;
   end to_convex_Raycast;


--  impact.d3.collision.convex_Raycast.continuous_convex::impact.d3.collision.convex_Raycast.continuous_convex ( const impact.d3.Shape.convex*        convexA,const impact.d3.Shape.convex*        convexB,impact.d3.collision.simplex_Solver* simplexSolver, impact.d3.collision.convex_penetration_depth_Solver* penetrationDepthSolver)
--  :m_simplexSolver(simplexSolver),
--  m_penetrationDepthSolver(penetrationDepthSolver),
--  m_convexA(convexA),m_convexB1(convexB),m_planeShape(0)
--  {
--  }




--
--  impact.d3.collision.convex_Raycast.continuous_convex::impact.d3.collision.convex_Raycast.continuous_convex( const impact.d3.Shape.convex*        convexA,const impact.d3.Shape.concave.static_plane*        plane)
--  :m_simplexSolver(0),
--  m_penetrationDepthSolver(0),
--  m_convexA(convexA),m_convexB1(0),m_planeShape(plane)
--  {
--  }




   function to_convex_Raycast (shapeA                 : access impact.d3.Shape.convex.Item'Class;
                               plane                  : access impact.d3.Shape.concave.static_plane.item'Class) return Item
   is
      Self : Item;
   begin
      Self.m_convexA                := shapeA;
      Self.m_planeShape             := plane;

      return Self;
   end to_convex_Raycast;






   --- Attributes
   --


   --  This maximum should not be necessary. It allows for untested/degenerate cases in production code.
   --  You don't want your game ever to lock-up.
   --
   MAX_ITERATIONS : constant := 64;




   overriding function calcTimeOfImpact (Self : access Item;   fromA, toA : in     Transform_3d;
                                                    fromB, toB : in     Transform_3d;
                                                    result     : access impact.d3.collision.convex_Raycast.CastResult'Class) return Boolean
   is
      linVelA,
      angVelA,
      linVelB,
      angVelB : math.Vector_3;

   begin
      --  compute linear and angular velocity for this interval, to interpolate
      --
      impact.d3.transform_Util.calculateVelocity (fromA, toA, 1.0, linVelA, angVelA);
      impact.d3.transform_Util.calculateVelocity (fromB, toB, 1.0, linVelB, angVelB);

      declare
         use impact.d3.Vector, math.Vectors;

         boundingRadiusA : math.Real := Self.m_convexA.getAngularMotionDisc;
         boundingRadiusB : constant math.Real := (if Self.m_convexB1 /= null then Self.m_convexB1.getAngularMotionDisc else 0.0);

         maxAngularProjectedVelocity : math.Real :=   length (angVelA) * boundingRadiusA
                                                    + length (angVelB) * boundingRadiusB;
         relLinVel : constant math.Vector_3 := linVelB - linVelA;

         relLinVelocLength : constant math.Real := length (linVelB - linVelA);

      begin
         if relLinVelocLength + maxAngularProjectedVelocity = 0.0 then
            return False;
         end if;

         declare

            lambda          : math.Real     := 0.0;
            v               : math.Vector_3 := (1.0, 0.0, 0.0);

            maxIter         : constant Integer       := MAX_ITERATIONS;

            n               : math.Vector_3 := (0.0, 0.0, 0.0);
            hasResult       : Boolean       := False;
            c               : math.Vector_3;

            lastLambda      : math.Real     := lambda;
            --  impact.d3.Scalar epsilon = impact.d3.Scalar(0.001);

            numIter         : Integer       := 0;

            --  first solution, using GJK

            radius          : math.Real     := 0.001;

            pointCollector1 : impact.d3.collision.point_Collector.Item;

         begin
            Self.computeClosestPoints (fromA, fromB,  pointCollector1);

            hasResult := pointCollector1.m_hasResult;
            c         := pointCollector1.m_pointInWorld;

            if hasResult then
               n := pointCollector1.m_normalOnBInWorld;
               declare
                  dist                    : math.Real := pointCollector1.m_distance + result.m_allowedPenetration;
                  projectedLinearVelocity : math.Real := dot (relLinVel, n);
                  dLambda                 : math.Real;
               begin

                  if projectedLinearVelocity + maxAngularProjectedVelocity  <=  impact.d3.Scalar.SIMD_EPSILON then
                     return False;
                  end if;

                  --  not close enough
                  --
                  while dist > radius
                  loop
                     dLambda                 := 0.0;
                     projectedLinearVelocity := dot (relLinVel, n);

                     --  don't report time of impact for motion away from the contact normal (or causes minor penetration)
                     --
                     if projectedLinearVelocity + maxAngularProjectedVelocity  <=  impact.d3.Scalar.SIMD_EPSILON then
                        return False;
                     end if;

                     dLambda := dist / (projectedLinearVelocity + maxAngularProjectedVelocity);
                     lambda  := lambda + dLambda;

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
                        exit;
                     end if;

                     lastLambda := lambda;


                     --  Interpolate to next lambda.
                     --
                     declare
                        use impact.d3.Transform;

                        interpolatedTransA,
                        interpolatedTransB,
                        relativeTrans     : Transform_3d;
                        pragma Unreferenced (relativeTrans);
                        pointCollector    : impact.d3.collision.point_Collector.item;
                     begin
                        impact.d3.transform_Util.integrateTransform (fromA, linVelA, angVelA, lambda, interpolatedTransA);
                        impact.d3.transform_Util.integrateTransform (fromB, linVelB, angVelB, lambda, interpolatedTransB);

                        relativeTrans := inverseTimes (interpolatedTransB, interpolatedTransA);

                        Self.computeClosestPoints (interpolatedTransA, interpolatedTransB, pointCollector);

                        if pointCollector.m_hasResult then
                           dist := pointCollector.m_distance + result.m_allowedPenetration;
                           c    := pointCollector.m_pointInWorld;
                           n    := pointCollector.m_normalOnBInWorld;
                        else
                           result.reportFailure (-1, numIter);
                           return False;
                        end if;

                        numIter := numIter + 1;

                        if numIter > maxIter then
                           result.reportFailure (-2, numIter);
                           return False;
                        end if;
                     end;
                  end loop;


                  result.m_fraction := lambda;
                  result.m_normal   := n;
                  result.m_hitPoint := c;

                  return True;
               end;
            end if;

            return False;
         end;
      end;
   end calcTimeOfImpact;









   procedure computeClosestPoints (Self : in out Item;   transA, transB : in     Transform_3d;
                                                         pointCollector : in out impact.d3.collision.point_Collector.item)
   is
   begin
      if Self.m_convexB1 /= null then
         Self.m_simplexSolver.reset;
         declare
            use impact.d3.collision.Detector.discrete.gjk_pair;

            gjk   : impact.d3.collision.Detector.discrete.gjk_pair.item
              := to_gjk_pair_Detector (Self.m_convexA,              Self.m_convexB1,
                                       Self.m_convexA.getShapeType, Self.m_convexB1.getShapeType,
                                       Self.m_convexA.getMargin,    Self.m_convexB1.getMargin,
                                       Self.m_simplexSolver,
                                       Self.m_penetrationDepthSolver);

            input : impact.d3.collision.Detector.discrete.ClosestPointInput;
         begin
            input.m_transformA := transA;
            input.m_transformB := transB;
            gjk.getClosestPoints (input, pointCollector, False);
         end;

      else   -- convex versus plane
         declare
            use linear_Algebra_3d, impact.d3.Transform,  impact.d3.Vector,  math.Vectors;

            convexShape : constant access impact.d3.Shape.convex     .Item'Class := Self.m_convexA;
            planeShape  : constant access impact.d3.Shape.concave.static_plane.Item'Class := Self.m_planeShape;

            hasCollision         : Boolean          :=      False;
            planeNormal          : math.Vector_3    renames planeShape.getPlaneNormal;
            planeConstant        : math.Real        :=      planeShape.getPlaneConstant;

            convexWorldTransform : constant Transform_3d :=      transA;
            convexInPlaneTrans   : constant Transform_3d :=      inverse (transB) * convexWorldTransform;
            planeInConvex        : constant Transform_3d :=      inverse (convexWorldTransform) * transB;

            vtx                  : constant math.Vector_3    :=      convexShape.localGetSupportingVertex (getBasis (planeInConvex) * (-planeNormal));

            vtxInPlane           : constant math.Vector_3    :=      convexInPlaneTrans * vtx;
            distance             : constant math.Real        :=      dot (planeNormal, vtxInPlane)  -  planeConstant;

            vtxInPlaneProjected  : constant math.Vector_3    :=      vtxInPlane - distance*planeNormal;
            vtxInPlaneWorld      : constant math.Vector_3    :=      transB * vtxInPlaneProjected;
            normalOnSurfaceB     : constant math.Vector_3    :=      getBasis (transB) * planeNormal;
         begin
            pointCollector.addContactPoint (normalOnSurfaceB,
                                            vtxInPlaneWorld,
                                            distance);
         end;
      end if;
   end computeClosestPoints;




end impact.d3.collision.convex_Raycast.continuous_convex;









--  bool        impact.d3.collision.convex_Raycast.continuous_convex::calcTimeOfImpact(
--                                  const impact.d3.Transform& fromA,
--                                  const impact.d3.Transform& toA,
--                                  const impact.d3.Transform& fromB,
--                                  const impact.d3.Transform& toB,
--                                  CastResult& result)
--  {
--
--
--          /// compute linear and angular velocity for this interval, to interpolate
--          impact.d3.Vector linVelA,angVelA,linVelB,angVelB;
--          impact.d3.TransformUtil::calculateVelocity(fromA,toA,impact.d3.Scalar(1.),linVelA,angVelA);
--          impact.d3.TransformUtil::calculateVelocity(fromB,toB,impact.d3.Scalar(1.),linVelB,angVelB);
--
--
--          impact.d3.Scalar boundingRadiusA = m_convexA->getAngularMotionDisc();
--          impact.d3.Scalar boundingRadiusB = m_convexB1?m_convexB1->getAngularMotionDisc():0.f;
--
--          impact.d3.Scalar maxAngularProjectedVelocity = angVelA.length() * boundingRadiusA + angVelB.length() * boundingRadiusB;
--          impact.d3.Vector relLinVel = (linVelB-linVelA);
--
--          impact.d3.Scalar relLinVelocLength = (linVelB-linVelA).length();
--
--          if ((relLinVelocLength+maxAngularProjectedVelocity) == 0.f)
--                  return false;
--
--
--
--          impact.d3.Scalar lambda = impact.d3.Scalar(0.);
--          impact.d3.Vector v(1,0,0);
--
--          int maxIter = MAX_ITERATIONS;
--
--          impact.d3.Vector n;
--          n.setValue(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.));
--          bool hasResult = false;
--          impact.d3.Vector c;
--
--          impact.d3.Scalar lastLambda = lambda;
--          //impact.d3.Scalar epsilon = impact.d3.Scalar(0.001);
--
--          int numIter = 0;
--          //first solution, using GJK
--
--
--          impact.d3.Scalar radius = 0.001f;
--  //        result.drawCoordSystem(sphereTr);
--
--          impact.d3.collision.point_Collector        pointCollector1;
--
--          {
--
--                  computeClosestPoints(fromA,fromB,pointCollector1);
--
--                  hasResult = pointCollector1.m_hasResult;
--                  c = pointCollector1.m_pointInWorld;
--          }
--
--          if (hasResult)
--          {
--                  impact.d3.Scalar dist;
--                  dist = pointCollector1.m_distance + result.m_allowedPenetration;
--                  n = pointCollector1.m_normalOnBInWorld;
--                  impact.d3.Scalar projectedLinearVelocity = relLinVel.dot(n);
--                  if ((projectedLinearVelocity+ maxAngularProjectedVelocity)<=SIMD_EPSILON)
--                          return false;
--
--                  //not close enough
--                  while (dist > radius)
--                  {
--                          if (result.m_debugDrawer)
--                          {
--                                  result.m_debugDrawer->drawSphere(c,0.2f,impact.d3.Vector(1,1,1));
--                          }
--                          impact.d3.Scalar dLambda = impact.d3.Scalar(0.);
--
--                          projectedLinearVelocity = relLinVel.dot(n);
--
--
--                          //don't report time of impact for motion away from the contact normal (or causes minor penetration)
--                          if ((projectedLinearVelocity+ maxAngularProjectedVelocity)<=SIMD_EPSILON)
--                                  return false;
--
--                          dLambda = dist / (projectedLinearVelocity+ maxAngularProjectedVelocity);
--
--
--
--                          lambda = lambda + dLambda;
--
--                          if (lambda > impact.d3.Scalar(1.))
--                                  return false;
--
--                          if (lambda < impact.d3.Scalar(0.))
--                                  return false;
--
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
--
--
--                          //interpolate to next lambda
--                          impact.d3.Transform interpolatedTransA,interpolatedTransB,relativeTrans;
--
--                          impact.d3.TransformUtil::integrateTransform(fromA,linVelA,angVelA,lambda,interpolatedTransA);
--                          impact.d3.TransformUtil::integrateTransform(fromB,linVelB,angVelB,lambda,interpolatedTransB);
--                          relativeTrans = interpolatedTransB.inverseTimes(interpolatedTransA);
--
--                          if (result.m_debugDrawer)
--                          {
--                                  result.m_debugDrawer->drawSphere(interpolatedTransA.getOrigin(),0.2f,impact.d3.Vector(1,0,0));
--                          }
--
--                          result.DebugDraw( lambda );
--
--                          impact.d3.collision.point_Collector        pointCollector;
--                          computeClosestPoints(interpolatedTransA,interpolatedTransB,pointCollector);
--
--                          if (pointCollector.m_hasResult)
--                          {
--                                  dist = pointCollector.m_distance+result.m_allowedPenetration;
--                                  c = pointCollector.m_pointInWorld;
--                                  n = pointCollector.m_normalOnBInWorld;
--                          } else
--                          {
--                                  result.reportFailure(-1, numIter);
--                                  return false;
--                          }
--
--                          numIter++;
--                          if (numIter > maxIter)
--                          {
--                                  result.reportFailure(-2, numIter);
--                                  return false;
--                          }
--                  }
--
--                  result.m_fraction = lambda;
--                  result.m_normal = n;
--                  result.m_hitPoint = c;
--                  return true;
--          }
--
--          return false;
--
--  }

