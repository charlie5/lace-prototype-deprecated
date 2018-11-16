with impact.d3.Transform;
with impact.d3.Vector;
with impact.d3.Scalar;

--  #include "impact.d3.collision.Detector.discrete.gjk_pair.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.simplex_Solver.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.convex_penetration_depth_Solver.h"
--


package body impact.d3.collision.Detector.discrete.gjk_pair
--
--
--
is


   REL_ERROR2  : constant := 1.0e-6;      -- Must be above the machine epsilon.


   --  temp globals, to improve GJK/EPA/penetration calculations
   --

   gNumDeepPenetrationChecks : Integer := 0;
   gNumGjkChecks             : Integer := 0;





   --- Forge
   --

   function to_gjk_pair_Detector (objectA, objectB       : access impact.d3.Shape.convex.Item'Class;
                                  simplexSolver          : access impact.d3.collision.simplex_Solver.Item'Class;
                                  penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.Item'Class) return Item
   is
      Self : Item;
   begin
      Self.m_cachedSeparatingAxis   := (0.0, 1.0, 0.0);
      Self.m_penetrationDepthSolver := penetrationDepthSolver;
      Self.m_simplexSolver          := simplexSolver;
      Self.m_minkowskiA             := objectA;
      Self.m_minkowskiB             := objectB;
      Self.m_shapeTypeA             := objectA.getShapeType;
      Self.m_shapeTypeB             := objectB.getShapeType;
      Self.m_marginA                := objectA.getMargin;
      Self.m_marginB                := objectB.getMargin;
      Self.m_ignoreMargin           := False;
      Self.m_lastUsedMethod         := -1;
      Self.m_catchDegeneracies      :=  True;

      return Self;
   end to_gjk_pair_Detector;




   function to_gjk_pair_Detector (objectA,    objectB    : access impact.d3.Shape.convex.Item'Class;
                                  shapeTypeA, shapeTypeB : in     impact.d3.collision.Proxy.BroadphaseNativeTypes;
                                  marginA,    marginB    : in     math.Real;
                                  simplexSolver          : access impact.d3.collision.simplex_Solver.Item'Class;
                                  penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.Item'Class) return Item
   is
      Self : Item;
   begin
      Self.m_cachedSeparatingAxis   := (0.0, 1.0, 0.0);
      Self.m_penetrationDepthSolver := penetrationDepthSolver;
      Self.m_simplexSolver          := simplexSolver;
      Self.m_minkowskiA             := objectA;
      Self.m_minkowskiB             := objectB;
      Self.m_shapeTypeA             := shapeTypeA;
      Self.m_shapeTypeB             := shapeTypeB;
      Self.m_marginA                := marginA;
      Self.m_marginB                := marginB;
      Self.m_ignoreMargin           := False;
      Self.m_lastUsedMethod         := -1;
      Self.m_catchDegeneracies      :=  True;

      return Self;
   end to_gjk_pair_Detector;







   --- Attributes
   --


   procedure setMinkowskiA (Self :    out Item;   minkA : access impact.d3.Shape.convex.Item'Class)
   is
   begin
      Self.m_minkowskiA := minkA;
   end setMinkowskiA;




   procedure setMinkowskiB (Self :    out Item;   minkB : access impact.d3.Shape.convex.Item'Class)
   is
   begin
      Self.m_minkowskiB := minkB;
   end setMinkowskiB;





   procedure setCachedSeperatingAxis (Self :    out Item;   seperatingAxis : in math.Vector_3)
   is
   begin
      Self.m_cachedSeparatingAxis := seperatingAxis;
   end setCachedSeperatingAxis;




   function getCachedSeparatingAxis (Self : in Item) return math.Vector_3
   is
   begin
      return Self.m_cachedSeparatingAxis;
   end getCachedSeparatingAxis;




   function getCachedSeparatingDistance (Self : in Item) return math.Real
   is
   begin
      return Self.m_cachedSeparatingDistance;
   end getCachedSeparatingDistance;




   procedure setPenetrationDepthSolver (Self :    out Item;   penetrationDepthSolver : access impact.d3.collision.convex_penetration_depth_Solver.Item'Class)
   is
   begin
      Self.m_penetrationDepthSolver := penetrationDepthSolver;
   end setPenetrationDepthSolver;





   procedure setIgnoreMargin (Self :    out Item;   ignoreMargin : in Boolean)
   is
   begin
      Self.m_ignoreMargin := ignoreMargin;
   end setIgnoreMargin;







   --- Operations
   --

   overriding procedure getClosestPoints (Self : in out Item;   input       : in     impact.d3.collision.Detector.discrete.ClosestPointInput;
                                                     output      : in out impact.d3.collision.Detector.discrete.Result'Class;
                                                     swapResults : in     Boolean                                               := False)
   is
      pragma Unreferenced (swapResults);
   begin
      Self.getClosestPointsNonVirtual (input, output);
   end getClosestPoints;







   procedure getClosestPointsNonVirtual (Self : in out Item'Class;   input       : in     impact.d3.collision.Detector.discrete.ClosestPointInput;
                                                                     output      : in out impact.d3.collision.Detector.discrete.Result'Class)
   is
      use impact.d3.Transform;

      distance       : math.Real        := 0.0;

      normalInB      : math.Vector_3    := (0.0, 0.0, 0.0);

      pointOnA,
      pointOnB       : math.Vector_3;

      localTransA    : aliased Transform_3d := input.m_transformA;
      localTransB    : aliased Transform_3d := input.m_transformB;

      positionOffset : constant math.Vector_3    := (getOrigin (localTransA) + getOrigin (localTransB)) * 0.5;

      check2d        : constant Boolean          := Self.m_minkowskiA.isConvex2d and then Self.m_minkowskiB.isConvex2d;

      marginA        : math.Real        := Self.m_marginA;
      marginB        : math.Real        := Self.m_marginB;

      gGjkMaxIter    : constant Integer          := 1000;   -- this is to catch invalid input, perhaps check for #NaN?

   begin
      Self.m_cachedSeparatingDistance := 0.0;

      getOrigin (localTransA'Unchecked_Access).all := getOrigin (localTransA) - positionOffset;
      getOrigin (localTransB'Unchecked_Access).all := getOrigin (localTransB) - positionOffset;

--        localTransA.getOrigin() -= positionOffset;
--        localTransB.getOrigin() -= positionOffset;

      gNumGjkChecks := gNumGjkChecks + 1;


      if Self.m_ignoreMargin then   -- for CCD we don't use margins
         marginA := 0.0;
         marginB := 0.0;
      end if;


      Self.m_curIter              :=  0;

      Self.m_cachedSeparatingAxis := (0.0, 1.0, 0.0);
      Self.m_degenerateSimplex    :=  0;
      Self.m_lastUsedMethod       := -1;


      declare
         isValid          : Boolean := False;
         checkSimplex     : Boolean := False;
         checkPenetration : constant Boolean := True;

         squaredDistance  : math.Real := BT_LARGE_FLOAT;
         the_delta        : math.Real := 0.0;

         margin           : math.Real := marginA + marginB;

      begin
         Self.m_simplexSolver.reset;

         loop
            declare
               use linear_Algebra_3d, impact.d3.Vector, impact.d3.Scalar;

               seperatingAxisInA : constant math.Vector_3 := -Self.m_cachedSeparatingAxis  *  getBasis (input.m_transformA);
               seperatingAxisInB : constant math.Vector_3 :=  Self.m_cachedSeparatingAxis  *  getBasis (input.m_transformB);

               pInA              : constant math.Vector_3 := Self.m_minkowskiA.localGetSupportVertexWithoutMarginNonVirtual (seperatingAxisInA);
               qInB              : constant math.Vector_3 := Self.m_minkowskiB.localGetSupportVertexWithoutMarginNonVirtual (seperatingAxisInB);

               pWorld            : math.Vector_3 := localTransA * pInA;
               qWorld            : math.Vector_3 := localTransB * qInB;

               w                 : math.Vector_3;
               f0, f1            : math.Real;

               newCachedSeparatingAxis : aliased math.Vector_3;
               previousSquaredDistance : math.Real;
               check                   : Boolean;
            begin

               if check2d then
                  pWorld (3) := 0.0;
                  qWorld (3) := 0.0;
               end if;

               w         := pWorld - qWorld;
               the_delta := dot (Self.m_cachedSeparatingAxis, w);


               --  potential exit, they don't overlap
               --
               if         the_delta              >  0.0
                 and then the_delta * the_delta  >  squaredDistance * input.m_maximumDistanceSquared
               then
                  Self.m_degenerateSimplex := 10;
                  checkSimplex := True;
                  --  // checkPenetration = false;
                  exit;
               end if;


               --  exit 0: the new point is already in the simplex, or we didn't come any closer
               --
               if Self.m_simplexSolver.inSimplex (w) then
                  Self.m_degenerateSimplex := 1;
                  checkSimplex             := True;
                  exit;
               end if;


               --  are we getting any closer ?
               f0 := squaredDistance - the_delta;
               f1 := squaredDistance * REL_ERROR2;

               if f0 <= f1 then
                  if f0 <= 0.0 then
                     Self.m_degenerateSimplex := 2;
                  else
                     Self.m_degenerateSimplex := 11;
                  end if;

                  checkSimplex := True;

                  exit;
               end if;

               --  add current vertex to simplex
               Self.m_simplexSolver.addVertex (w, pWorld, qWorld);


               --  calculate the closest point to the origin (update vector v)
               --
               if not Self.m_simplexSolver.closest (newCachedSeparatingAxis'Access) then
                  Self.m_degenerateSimplex := 3;
                  checkSimplex             := True;

                  exit;
               end if;


               if length2 (newCachedSeparatingAxis) < REL_ERROR2 then
                  Self.m_cachedSeparatingAxis := newCachedSeparatingAxis;
                  Self.m_degenerateSimplex    := 6;
                  checkSimplex                := True;

                  exit;
               end if;

               previousSquaredDistance := squaredDistance;
               squaredDistance         := length2 (newCachedSeparatingAxis);


               --  redundant m_simplexSolver->compute_points(pointOnA, pointOnB);

               --  are we getting any closer ?
               --
               if previousSquaredDistance - squaredDistance <= SIMD_EPSILON * previousSquaredDistance then
                  --  // m_simplexSolver->backup_closest(m_cachedSeparatingAxis);
                  checkSimplex             := True;
                  Self.m_degenerateSimplex := 12;

                  exit;
               end if;

               Self.m_cachedSeparatingAxis := newCachedSeparatingAxis;

               --  degeneracy, this is typically due to invalid/uninitialized worldtransforms for a impact.d3.Object
               --
               if Self.m_curIter > gGjkMaxIter then
                  Self.m_curIter := Self.m_curIter + 1;

                  exit;
               else
                  Self.m_curIter := Self.m_curIter + 1;
               end if;


               --  // bool check = (!m_simplexSolver->fullSimplex() && squaredDistance > SIMD_EPSILON * m_simplexSolver->maxVertex());
               check := not Self.m_simplexSolver.fullSimplex;

               if not check then
                  --  do we need this backup_closest here ?
                  --  m_simplexSolver->backup_closest(m_cachedSeparatingAxis);
                  Self.m_degenerateSimplex := 13;

                  exit;
               end if;

            end;
         end loop;



         if checkSimplex then
            declare
               use impact.d3.Vector, impact.d3.Scalar,  math.Functions;

               lenSqr,
               rlen,
               s     : math.Real;
            begin
               Self.m_simplexSolver.compute_points (pointOnA, pointOnB);

               normalInB := Self.m_cachedSeparatingAxis;
               lenSqr    := length2 (Self.m_cachedSeparatingAxis);

               --  valid normal
               --
               if lenSqr < 0.0001 then
                  Self.m_degenerateSimplex := 5;
               end if;


               if lenSqr > SIMD_EPSILON * SIMD_EPSILON then
                  rlen      := 1.0 / sqRt (lenSqr);
                  normalInB := normalInB * rlen;            -- normalize
                  s         := sqRt (squaredDistance);     pragma Assert (s > 0.0);

                  pointOnA := pointOnA  -  Self.m_cachedSeparatingAxis * (marginA / s);
                  pointOnB := pointOnB  +  Self.m_cachedSeparatingAxis * (marginB / s);
                  distance := (1.0 / rlen) - margin;
                  isValid  := True;

                  Self.m_lastUsedMethod := 1;
               else
                  Self.m_lastUsedMethod := 2;
               end if;

            end;
         end if;


         declare
            use impact.d3.Vector, impact.d3.Scalar,  math.Functions;

            catchDegeneratePenetrationCase : constant Boolean := (       Self.m_catchDegeneracies
                                                         and then Self.m_penetrationDepthSolver /= null
                                                         and then Self.m_degenerateSimplex      /= 0
                                                         and then ((distance + margin) < 0.01));
            tmpPointOnA,
            tmpPointOnB : aliased math.Vector_3;

            distance2  : math.Real;
            isValid2   : Boolean;

            tmpNormalInB : math.Vector_3;
            lenSqr       : math.Real;

         begin
            --  // if (checkPenetration && !isValid)
            if         checkPenetration
              and then (      not isValid
                        or else catchDegeneratePenetrationCase)
            then
               --  penetration case

               --  if there is no way to handle penetrations, bail out
               if Self.m_penetrationDepthSolver /= null then
                  --  Penetration depth case.

                  gNumDeepPenetrationChecks   := gNumDeepPenetrationChecks + 1;
                  Self.m_cachedSeparatingAxis := (0.0, 0.0, 0.0);

                  isValid2 := Self.m_penetrationDepthSolver.calcPenDepth (Self.m_simplexSolver,
                                                                          impact.d3.Shape.convex.view (Self.m_minkowskiA),
                                                                          impact.d3.Shape.convex.view (Self.m_minkowskiB),
                                                                          localTransA,        localTransB,
                                                                          Self.m_cachedSeparatingAxis'Access,
                                                                          tmpPointOnA'Access, tmpPointOnB'Access);
                  if isValid2 then
                     tmpNormalInB := tmpPointOnB - tmpPointOnA;
                     lenSqr       := length2 (tmpNormalInB);

                     if lenSqr  <=  SIMD_EPSILON * SIMD_EPSILON then
                        tmpNormalInB := Self.m_cachedSeparatingAxis;
                        lenSqr       := length2 (Self.m_cachedSeparatingAxis);
                     end if;

                     if lenSqr > SIMD_EPSILON * SIMD_EPSILON then
                        tmpNormalInB :=  tmpNormalInB / sqRt (lenSqr);
                        distance2    := -length (tmpPointOnA - tmpPointOnB);

                        --  only replace valid penetrations when the result is deeper (check)
                        if        not isValid
                          or else (distance2 < distance)
                        then
                           distance  := distance2;
                           pointOnA  := tmpPointOnA;
                           pointOnB  := tmpPointOnB;
                           normalInB := tmpNormalInB;
                           isValid   := True;
                           Self.m_lastUsedMethod := 3;
                        else
                           Self.m_lastUsedMethod := 8;
                        end if;

                     else
                        Self.m_lastUsedMethod := 9;
                     end if;

                  else
                     --  this is another degenerate case, where the initial GJK calculation reports a degenerate case
                     --  EPA reports no penetration, and the second GJK (using the supporting vector without margin)
                     --  reports a valid positive distance. Use the results of the second GJK instead of failing.
                     --  thanks to Jacob.Langford for the reproduction case
                     --  http://code.google.com/p/bullet/issues/detail?id=250

                     if length2 (Self.m_cachedSeparatingAxis)  >  0.0 then
                        declare
                           distance2 : math.Real := length (tmpPointOnA - tmpPointOnB) - margin;
                        begin
                           --  only replace valid distances when the distance is less
                           if        not isValid
                             or else distance2 < distance
                           then
                              distance  := distance2;
                              pointOnA  := tmpPointOnA;
                              pointOnB  := tmpPointOnB;

                              pointOnA  := pointOnA  -  Self.m_cachedSeparatingAxis * marginA;
                              pointOnB  := pointOnB  +  Self.m_cachedSeparatingAxis * marginB;

                              normalInB := Self.m_cachedSeparatingAxis;
                              normalize (normalInB);

                              isValid   := True;

                              Self.m_lastUsedMethod := 6;
                           else
                              Self.m_lastUsedMethod := 5;
                           end if;
                        end;
                     end if;

                  end if;

               end if;

            end if;
         end;




         if         isValid
           and then (      distance             <  0.0
                     or else distance * distance  <  input.m_maximumDistanceSquared)
         then
            Self.m_cachedSeparatingAxis     := normalInB;
            Self.m_cachedSeparatingDistance := distance;

            output.addContactPoint (normalInB,
                                    pointOnB + positionOffset,
                                    distance);
         end if;

      end;

   end getClosestPointsNonVirtual;



end impact.d3.collision.Detector.discrete.gjk_pair;










--  #ifdef __SPU__
--  void impact.d3.collision.Detector.discrete.gjk_pair::getClosestPointsNonVirtual(const ClosestPointInput& input,Result& output,class btIDebugDraw* debugDraw)
--  #else
--  void impact.d3.collision.Detector.discrete.gjk_pair::getClosestPointsNonVirtual(const ClosestPointInput& input,Result& output,class btIDebugDraw* debugDraw)
--  #endif
--  {
--          m_cachedSeparatingDistance = 0.f;
--
--          impact.d3.Scalar distance=impact.d3.Scalar(0.);
--          impact.d3.Vector        normalInB(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.));
--          impact.d3.Vector pointOnA,pointOnB;
--          impact.d3.Transform        localTransA = input.m_transformA;
--          impact.d3.Transform localTransB = input.m_transformB;
--          impact.d3.Vector positionOffset = (localTransA.getOrigin() + localTransB.getOrigin()) * impact.d3.Scalar(0.5);
--          localTransA.getOrigin() -= positionOffset;
--          localTransB.getOrigin() -= positionOffset;
--
--          bool check2d = m_minkowskiA->isConvex2d() && m_minkowskiB->isConvex2d();
--
--          impact.d3.Scalar marginA = m_marginA;
--          impact.d3.Scalar marginB = m_marginB;
--
--          gNumGjkChecks++;
--
--  #ifdef DEBUG_SPU_COLLISION_DETECTION
--          spu_printf("inside gjk\n");
--  #endif
--          //for CCD we don't use margins
--          if (m_ignoreMargin)
--          {
--                  marginA = impact.d3.Scalar(0.);
--                  marginB = impact.d3.Scalar(0.);
--  #ifdef DEBUG_SPU_COLLISION_DETECTION
--                  spu_printf("ignoring margin\n");
--  #endif
--          }
--
--          m_curIter = 0;
--          int gGjkMaxIter = 1000;//this is to catch invalid input, perhaps check for #NaN?
--          m_cachedSeparatingAxis.setValue(0,1,0);
--
--          bool isValid = false;
--          bool checkSimplex = false;
--          bool checkPenetration = true;
--          m_degenerateSimplex = 0;
--
--          m_lastUsedMethod = -1;
--
--          {
--                  impact.d3.Scalar squaredDistance = BT_LARGE_FLOAT;
--                  impact.d3.Scalar delta = impact.d3.Scalar(0.);
--
--                  impact.d3.Scalar margin = marginA + marginB;
--
--
--
--                  m_simplexSolver->reset();
--
--                  for ( ; ; )
--                  //while (true)
--                  {
--
--                          impact.d3.Vector seperatingAxisInA = (-m_cachedSeparatingAxis)* input.m_transformA.getBasis();
--                          impact.d3.Vector seperatingAxisInB = m_cachedSeparatingAxis* input.m_transformB.getBasis();
--
--  #if 1
--
--                          impact.d3.Vector pInA = m_minkowskiA->localGetSupportVertexWithoutMarginNonVirtual(seperatingAxisInA);
--                          impact.d3.Vector qInB = m_minkowskiB->localGetSupportVertexWithoutMarginNonVirtual(seperatingAxisInB);
--
--  //                        impact.d3.Vector pInA  = localGetSupportingVertexWithoutMargin(m_shapeTypeA, m_minkowskiA, seperatingAxisInA,input.m_convexVertexData[0]);//, &featureIndexA);
--  //                        impact.d3.Vector qInB  = localGetSupportingVertexWithoutMargin(m_shapeTypeB, m_minkowskiB, seperatingAxisInB,input.m_convexVertexData[1]);//, &featureIndexB);
--
--  #else
--  #ifdef __SPU__
--                          impact.d3.Vector pInA = m_minkowskiA->localGetSupportVertexWithoutMarginNonVirtual(seperatingAxisInA);
--                          impact.d3.Vector qInB = m_minkowskiB->localGetSupportVertexWithoutMarginNonVirtual(seperatingAxisInB);
--  #else
--                          impact.d3.Vector pInA = m_minkowskiA->localGetSupportingVertexWithoutMargin(seperatingAxisInA);
--                          impact.d3.Vector qInB = m_minkowskiB->localGetSupportingVertexWithoutMargin(seperatingAxisInB);
--  #ifdef TEST_NON_VIRTUAL
--                          impact.d3.Vector pInAv = m_minkowskiA->localGetSupportingVertexWithoutMargin(seperatingAxisInA);
--                          impact.d3.Vector qInBv = m_minkowskiB->localGetSupportingVertexWithoutMargin(seperatingAxisInB);
--                          btAssert((pInAv-pInA).length() < 0.0001);
--                          btAssert((qInBv-qInB).length() < 0.0001);
--  #endif //
--  #endif //__SPU__
--  #endif
--
--
--                          impact.d3.Vector  pWorld = localTransA(pInA);
--                          impact.d3.Vector  qWorld = localTransB(qInB);
--
--  #ifdef DEBUG_SPU_COLLISION_DETECTION
--                  spu_printf("got local supporting vertices\n");
--  #endif
--
--                          if (check2d)
--                          {
--                                  pWorld[2] = 0.f;
--                                  qWorld[2] = 0.f;
--                          }
--
--                          impact.d3.Vector w        = pWorld - qWorld;
--                          delta = m_cachedSeparatingAxis.dot(w);
--
--                          // potential exit, they don't overlap
--                          if ((delta > impact.d3.Scalar(0.0)) && (delta * delta > squaredDistance * input.m_maximumDistanceSquared))
--                          {
--                                  m_degenerateSimplex = 10;
--                                  checkSimplex=true;
--                                  //checkPenetration = false;
--                                  break;
--                          }
--
--                          //exit 0: the new point is already in the simplex, or we didn't come any closer
--                          if (m_simplexSolver->inSimplex(w))
--                          {
--                                  m_degenerateSimplex = 1;
--                                  checkSimplex = true;
--                                  break;
--                          }
--                          // are we getting any closer ?
--                          impact.d3.Scalar f0 = squaredDistance - delta;
--                          impact.d3.Scalar f1 = squaredDistance * REL_ERROR2;
--
--                          if (f0 <= f1)
--                          {
--                                  if (f0 <= impact.d3.Scalar(0.))
--                                  {
--                                          m_degenerateSimplex = 2;
--                                  } else
--                                  {
--                                          m_degenerateSimplex = 11;
--                                  }
--                                  checkSimplex = true;
--                                  break;
--                          }
--
--  #ifdef DEBUG_SPU_COLLISION_DETECTION
--                  spu_printf("addVertex 1\n");
--  #endif
--                          //add current vertex to simplex
--                          m_simplexSolver->addVertex(w, pWorld, qWorld);
--  #ifdef DEBUG_SPU_COLLISION_DETECTION
--                  spu_printf("addVertex 2\n");
--  #endif
--                          impact.d3.Vector newCachedSeparatingAxis;
--
--                          //calculate the closest point to the origin (update vector v)
--                          if (!m_simplexSolver->closest(newCachedSeparatingAxis))
--                          {
--                                  m_degenerateSimplex = 3;
--                                  checkSimplex = true;
--                                  break;
--                          }
--
--                          if(newCachedSeparatingAxis.length2()<REL_ERROR2)
--              {
--                                  m_cachedSeparatingAxis = newCachedSeparatingAxis;
--                  m_degenerateSimplex = 6;
--                  checkSimplex = true;
--                  break;
--              }
--
--                          impact.d3.Scalar previousSquaredDistance = squaredDistance;
--                          squaredDistance = newCachedSeparatingAxis.length2();
--  #if 0
--  ///warning: this termination condition leads to some problems in 2d test case see Bullet/Demos/Box2dDemo
--                          if (squaredDistance>previousSquaredDistance)
--                          {
--                                  m_degenerateSimplex = 7;
--                                  squaredDistance = previousSquaredDistance;
--                  checkSimplex = false;
--                  break;
--                          }
--  #endif //
--
--
--                          //redundant m_simplexSolver->compute_points(pointOnA, pointOnB);
--
--                          //are we getting any closer ?
--                          if (previousSquaredDistance - squaredDistance <= SIMD_EPSILON * previousSquaredDistance)
--                          {
--  //                                m_simplexSolver->backup_closest(m_cachedSeparatingAxis);
--                                  checkSimplex = true;
--                                  m_degenerateSimplex = 12;
--
--                                  break;
--                          }
--
--                          m_cachedSeparatingAxis = newCachedSeparatingAxis;
--
--                            //degeneracy, this is typically due to invalid/uninitialized worldtransforms for a impact.d3.Object
--                if (m_curIter++ > gGjkMaxIter)
--                {
--                        #if defined(DEBUG) || defined (_DEBUG) || defined (DEBUG_SPU_COLLISION_DETECTION)
--
--                                printf("impact.d3.collision.Detector.discrete.gjk_pair maxIter exceeded:%i\n",m_curIter);
--                                printf("sepAxis=(%f,%f,%f), squaredDistance = %f, shapeTypeA=%i,shapeTypeB=%i\n",
--                                m_cachedSeparatingAxis.getX(),
--                                m_cachedSeparatingAxis.getY(),
--                                m_cachedSeparatingAxis.getZ(),
--                                squaredDistance,
--                                m_minkowskiA->getShapeType(),
--                                m_minkowskiB->getShapeType());
--
--                        #endif
--                        break;
--
--                }
--
--
--                          bool check = (!m_simplexSolver->fullSimplex());
--                          //bool check = (!m_simplexSolver->fullSimplex() && squaredDistance > SIMD_EPSILON * m_simplexSolver->maxVertex());
--
--                          if (!check)
--                          {
--                                  //do we need this backup_closest here ?
--  //                                m_simplexSolver->backup_closest(m_cachedSeparatingAxis);
--                                  m_degenerateSimplex = 13;
--                                  break;
--                          }
--                  }
--
--                  if (checkSimplex)
--                  {
--                          m_simplexSolver->compute_points(pointOnA, pointOnB);
--                          normalInB = m_cachedSeparatingAxis;
--                          impact.d3.Scalar lenSqr =m_cachedSeparatingAxis.length2();
--
--                          //valid normal
--                          if (lenSqr < 0.0001)
--                          {
--                                  m_degenerateSimplex = 5;
--                          }
--                          if (lenSqr > SIMD_EPSILON*SIMD_EPSILON)
--                          {
--                                  impact.d3.Scalar rlen = impact.d3.Scalar(1.) / btSqrt(lenSqr );
--                                  normalInB *= rlen; //normalize
--                                  impact.d3.Scalar s = btSqrt(squaredDistance);
--
--                                  btAssert(s > impact.d3.Scalar(0.0));
--                                  pointOnA -= m_cachedSeparatingAxis * (marginA / s);
--                                  pointOnB += m_cachedSeparatingAxis * (marginB / s);
--                                  distance = ((impact.d3.Scalar(1.)/rlen) - margin);
--                                  isValid = true;
--
--                                  m_lastUsedMethod = 1;
--                          } else
--                          {
--                                  m_lastUsedMethod = 2;
--                          }
--                  }
--
--                  bool catchDegeneratePenetrationCase =
--                          (m_catchDegeneracies && m_penetrationDepthSolver && m_degenerateSimplex && ((distance+margin) < 0.01));
--
--                  //if (checkPenetration && !isValid)
--                  if (checkPenetration && (!isValid || catchDegeneratePenetrationCase ))
--                  {
--                          //penetration case
--
--                          //if there is no way to handle penetrations, bail out
--                          if (m_penetrationDepthSolver)
--                          {
--                                  // Penetration depth case.
--                                  impact.d3.Vector tmpPointOnA,tmpPointOnB;
--
--                                  gNumDeepPenetrationChecks++;
--                                  m_cachedSeparatingAxis.setZero();
--
--                                  bool isValid2 = m_penetrationDepthSolver->calcPenDepth(
--                                          *m_simplexSolver,
--                                          m_minkowskiA,m_minkowskiB,
--                                          localTransA,localTransB,
--                                          m_cachedSeparatingAxis, tmpPointOnA, tmpPointOnB,
--                                          debugDraw,input.m_stackAlloc
--                                          );
--
--
--                                  if (isValid2)
--                                  {
--                                          impact.d3.Vector tmpNormalInB = tmpPointOnB-tmpPointOnA;
--                                          impact.d3.Scalar lenSqr = tmpNormalInB.length2();
--                                          if (lenSqr <= (SIMD_EPSILON*SIMD_EPSILON))
--                                          {
--                                                  tmpNormalInB = m_cachedSeparatingAxis;
--                                                  lenSqr = m_cachedSeparatingAxis.length2();
--                                          }
--
--                                          if (lenSqr > (SIMD_EPSILON*SIMD_EPSILON))
--                                          {
--                                                  tmpNormalInB /= btSqrt(lenSqr);
--                                                  impact.d3.Scalar distance2 = -(tmpPointOnA-tmpPointOnB).length();
--                                                  //only replace valid penetrations when the result is deeper (check)
--                                                  if (!isValid || (distance2 < distance))
--                                                  {
--                                                          distance = distance2;
--                                                          pointOnA = tmpPointOnA;
--                                                          pointOnB = tmpPointOnB;
--                                                          normalInB = tmpNormalInB;
--                                                          isValid = true;
--                                                          m_lastUsedMethod = 3;
--                                                  } else
--                                                  {
--                                                          m_lastUsedMethod = 8;
--                                                  }
--                                          } else
--                                          {
--                                                  m_lastUsedMethod = 9;
--                                          }
--                                  } else
--
--                                  {
--                                          ///this is another degenerate case, where the initial GJK calculation reports a degenerate case
--                                          ///EPA reports no penetration, and the second GJK (using the supporting vector without margin)
--                                          ///reports a valid positive distance. Use the results of the second GJK instead of failing.
--                                          ///thanks to Jacob.Langford for the reproduction case
--                                          ///http://code.google.com/p/bullet/issues/detail?id=250
--
--
--                                          if (m_cachedSeparatingAxis.length2() > impact.d3.Scalar(0.))
--                                          {
--                                                  impact.d3.Scalar distance2 = (tmpPointOnA-tmpPointOnB).length()-margin;
--                                                  //only replace valid distances when the distance is less
--                                                  if (!isValid || (distance2 < distance))
--                                                  {
--                                                          distance = distance2;
--                                                          pointOnA = tmpPointOnA;
--                                                          pointOnB = tmpPointOnB;
--                                                          pointOnA -= m_cachedSeparatingAxis * marginA ;
--                                                          pointOnB += m_cachedSeparatingAxis * marginB ;
--                                                          normalInB = m_cachedSeparatingAxis;
--                                                          normalInB.normalize();
--                                                          isValid = true;
--                                                          m_lastUsedMethod = 6;
--                                                  } else
--                                                  {
--                                                          m_lastUsedMethod = 5;
--                                                  }
--                                          }
--                                  }
--
--                          }
--
--                  }
--          }
--
--
--
--          if (isValid && ((distance < 0) || (distance*distance < input.m_maximumDistanceSquared)))
--          {
--  #if 0
--  ///some debugging
--  //                if (check2d)
--                  {
--                          printf("n = %2.3f,%2.3f,%2.3f. ",normalInB[0],normalInB[1],normalInB[2]);
--                          printf("distance = %2.3f exit=%d deg=%d\n",distance,m_lastUsedMethod,m_degenerateSimplex);
--                  }
--  #endif
--
--                  m_cachedSeparatingAxis = normalInB;
--                  m_cachedSeparatingDistance = distance;
--
--                  output.addContactPoint(
--                          normalInB,
--                          pointOnB+positionOffset,
--                          distance);
--
--          }
--
--
--  }
