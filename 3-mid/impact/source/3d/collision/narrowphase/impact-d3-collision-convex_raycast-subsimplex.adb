with impact.d3.Transform;
with impact.d3.Vector;
with impact.d3.Scalar;

--  #include "btSubSimplexConvexCast.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.h"
--
--  #include "BulletCollision/CollisionShapes/btMinkowskiSumShape.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.simplex_Solver.h"
--  #include "impact.d3.collision.point_Collector.h"
--  #include "LinearMath/impact.d3.TransformUtil.h"



package body impact.d3.collision.convex_Raycast.subsimplex
is


   function to_convex_Raycast (shapeA, shapeB : in     impact.d3.Shape.convex.view;
                               simplexSolver  : access impact.d3.collision.simplex_Solver.Item'Class) return Item
   is
      Self : Item;
   begin
      Self.m_simplexSolver := simplexSolver;
      Self.m_convexA       := shapeA;
      Self.m_convexB       := shapeB;

      return Self;
   end to_convex_Raycast;



   --  Typically the conservative advancement reaches solution in a few iterations, clip it to 32 for degenerate cases.
   --  See discussion about this here http://continuousphysics.com/Bullet/phpBB2/viewtopic.php?t=565
   --
   MAX_ITERATIONS : constant := 64;




   overriding function calcTimeOfImpact (Self : access Item;   fromA, toA : in     Transform_3d;
                                                    fromB, toB : in     Transform_3d;
                              result     : access impact.d3.collision.convex_Raycast.CastResult'Class) return Boolean
   is
      use linear_Algebra_3d, impact.d3.Scalar,  impact.d3.Vector,  impact.d3.Transform,  math.Vectors;

      linVelA    : constant math.Vector_3 := getOrigin (toA) - getOrigin (fromA);
      linVelB    : constant math.Vector_3 := getOrigin (toB) - getOrigin (fromB);

      lambda     : math.Real     := 0.0;

      interpolatedTransA : aliased Transform_3d := fromA;
      interpolatedTransB : aliased Transform_3d := fromB;

      r          : constant math.Vector_3 := linVelA - linVelB;    -- take relative motion

      supVertexA : math.Vector_3 := fromA * Self.m_convexA.localGetSupportingVertex (-r * getBasis (fromA));
      supVertexB : math.Vector_3 := fromB * Self.m_convexB.localGetSupportingVertex (r * getBasis (fromB));

      v          : aliased math.Vector_3 := supVertexA - supVertexB;
      maxIter    : Integer               := MAX_ITERATIONS;

      n          : math.Vector_3 := (0.0, 0.0, 0.0);
      hasResult  : Boolean       := False;
      pragma Unreferenced (hasResult);
--        c          : math.Vector_3;

      lastLambda : math.Real     := lambda;
      pragma Unreferenced (lastLambda);


      dist2      : math.Real     := length2 (v);
      epsilon    : math.Real     := 0.0001;

      w          : math.Vector_3;
      --  p       : math.Vector_3;

      VdotR,
      VdotW      : math.Real;

   begin
      Self.m_simplexSolver.reset;


      while      dist2 > epsilon
        and then maxIter /= 0
      loop
         maxIter := maxIter - 1;

         supVertexA := interpolatedTransA * Self.m_convexA.localGetSupportingVertex (-v * getBasis (interpolatedTransA));
         supVertexB := interpolatedTransB * Self.m_convexB.localGetSupportingVertex (v * getBasis (interpolatedTransB));
         w          := supVertexA - supVertexB;
         VdotW      := dot (v, w);


         if lambda > 1.0 then
            return False;
         end if;


         if VdotW > 0.0 then
            VdotR := dot (v, r);

            if VdotR >= -(SIMD_EPSILON * SIMD_EPSILON) then
               return False;
            else
               lambda     := lambda - VdotW / VdotR;
               --  interpolate to next lambda

               --        x = s + lambda * r;

               setInterpolate3 (getOrigin (interpolatedTransA'Access).all,  getOrigin (fromA),  getOrigin (toA),  lambda);
               setInterpolate3 (getOrigin (interpolatedTransB'Access).all,  getOrigin (fromB),  getOrigin (toB),  lambda);

               --  m_simplexSolver->reset();

               --  check next line
               w          := supVertexA - supVertexB;
               lastLambda := lambda;
               n          := v;
               hasResult  := True;
            end if;
         end if;


         --  Just like regular GJK only add the vertex if it isn't already (close) to current vertex, it would lead to divisions by zero and NaN etc.
         --
         if not Self.m_simplexSolver.inSimplex (w) then
            Self.m_simplexSolver.addVertex (w,  supVertexA, supVertexB);
         end if;


         if Self.m_simplexSolver.closest (v'Access) then
            dist2     := length2 (v);
            hasResult := True;
            --  todo: check this normal for validity
            --  n=v;
         else
            dist2 := 0.0;
         end if;
      end loop;

      --  int numiter = MAX_ITERATIONS - maxIter;


      --  don't report a time of impact when moving 'away' from the hitnormal
      --
      result.m_fraction := lambda;

      if length2 (n) >= SIMD_EPSILON * SIMD_EPSILON then
         result.m_normal := normalized (n);
      else
         result.m_normal := (0.0, 0.0, 0.0);
      end if;


      --  don't report time of impact for motion away from the contact normal (or causes minor penetration)
      --
      if dot (result.m_normal, r)  >=  -result.m_allowedPenetration then
         return False;
      end if;


      declare
         hitA, hitB : math.Vector_3;
      begin
         Self.m_simplexSolver.compute_points (hitA, hitB);
         result.m_hitPoint := hitB;
      end;

      return True;
   end calcTimeOfImpact;


end impact.d3.collision.convex_Raycast.subsimplex;














--  bool        impact.d3.collision.convex_Raycast.subsimplex::calcTimeOfImpact(
--                  const impact.d3.Transform& fromA,
--                  const impact.d3.Transform& toA,
--                  const impact.d3.Transform& fromB,
--                  const impact.d3.Transform& toB,
--                  CastResult& result)
--  {
--
--          m_simplexSolver->reset();
--
--          impact.d3.Vector linVelA,linVelB;
--          linVelA = toA.getOrigin()-fromA.getOrigin();
--          linVelB = toB.getOrigin()-fromB.getOrigin();
--
--          impact.d3.Scalar lambda = impact.d3.Scalar(0.);
--
--          impact.d3.Transform interpolatedTransA = fromA;
--          impact.d3.Transform interpolatedTransB = fromB;
--
--          ///take relative motion
--          impact.d3.Vector r = (linVelA-linVelB);
--          impact.d3.Vector v;
--
--          impact.d3.Vector supVertexA = fromA(m_convexA->localGetSupportingVertex(-r*fromA.getBasis()));
--          impact.d3.Vector supVertexB = fromB(m_convexB->localGetSupportingVertex(r*fromB.getBasis()));
--          v = supVertexA-supVertexB;
--          int maxIter = MAX_ITERATIONS;
--
--          impact.d3.Vector n;
--          n.setValue(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.));
--          bool hasResult = false;
--          impact.d3.Vector c;
--
--          impact.d3.Scalar lastLambda = lambda;
--
--
--          impact.d3.Scalar dist2 = v.length2();
--  #ifdef BT_USE_DOUBLE_PRECISION
--          impact.d3.Scalar epsilon = impact.d3.Scalar(0.0001);
--  #else
--          impact.d3.Scalar epsilon = impact.d3.Scalar(0.0001);
--  #endif //BT_USE_DOUBLE_PRECISION
--          impact.d3.Vector        w,p;
--          impact.d3.Scalar VdotR;
--
--          while ( (dist2 > epsilon) && maxIter--)
--          {
--                  supVertexA = interpolatedTransA(m_convexA->localGetSupportingVertex(-v*interpolatedTransA.getBasis()));
--                  supVertexB = interpolatedTransB(m_convexB->localGetSupportingVertex(v*interpolatedTransB.getBasis()));
--                  w = supVertexA-supVertexB;
--
--                  impact.d3.Scalar VdotW = v.dot(w);
--
--                  if (lambda > impact.d3.Scalar(1.0))
--                  {
--                          return false;
--                  }
--
--                  if ( VdotW > impact.d3.Scalar(0.))
--                  {
--                          VdotR = v.dot(r);
--
--                          if (VdotR >= -(SIMD_EPSILON*SIMD_EPSILON))
--                                  return false;
--                          else
--                          {
--                                  lambda = lambda - VdotW / VdotR;
--                                  //interpolate to next lambda
--                                  //        x = s + lambda * r;
--                                  interpolatedTransA.getOrigin().setInterpolate3(fromA.getOrigin(),toA.getOrigin(),lambda);
--                                  interpolatedTransB.getOrigin().setInterpolate3(fromB.getOrigin(),toB.getOrigin(),lambda);
--                                  //m_simplexSolver->reset();
--                                  //check next line
--                                   w = supVertexA-supVertexB;
--                                  lastLambda = lambda;
--                                  n = v;
--                                  hasResult = true;
--                          }
--                  }
--                  ///Just like regular GJK only add the vertex if it isn't already (close) to current vertex, it would lead to divisions by zero and NaN etc.
--                  if (!m_simplexSolver->inSimplex(w))
--                          m_simplexSolver->addVertex( w, supVertexA , supVertexB);
--
--                  if (m_simplexSolver->closest(v))
--                  {
--                          dist2 = v.length2();
--                          hasResult = true;
--                          //todo: check this normal for validity
--                          //n=v;
--                          //printf("V=%f , %f, %f\n",v[0],v[1],v[2]);
--                          //printf("DIST2=%f\n",dist2);
--                          //printf("numverts = %i\n",m_simplexSolver->numVertices());
--                  } else
--                  {
--                          dist2 = impact.d3.Scalar(0.);
--                  }
--          }
--
--          //int numiter = MAX_ITERATIONS - maxIter;
--  //        printf("number of iterations: %d", numiter);
--
--          //don't report a time of impact when moving 'away' from the hitnormal
--
--
--          result.m_fraction = lambda;
--          if (n.length2() >= (SIMD_EPSILON*SIMD_EPSILON))
--                  result.m_normal = n.normalized();
--          else
--                  result.m_normal = impact.d3.Vector(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
--
--          //don't report time of impact for motion away from the contact normal (or causes minor penetration)
--          if (result.m_normal.dot(r)>=-result.m_allowedPenetration)
--                  return false;
--
--          impact.d3.Vector hitA,hitB;
--          m_simplexSolver->compute_points(hitA,hitB);
--          result.m_hitPoint=hitB;
--          return true;
--  }
--
--


