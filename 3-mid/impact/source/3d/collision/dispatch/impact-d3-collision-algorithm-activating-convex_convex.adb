with impact.d3.collision.convex_Raycast.gjk;
with impact.d3.collision.simplex_Solver.voronoi;
with impact.d3.collision.convex_Raycast;
with impact.d3.Shape.convex.internal.sphere;
with impact.d3.collision.polyhedral_contact_Clipping;
with impact.d3.Quaternions;
with impact.d3.Shape.convex.internal.polyhedral.triangle;
with impact.d3.Shape.convex.internal.polyhedral;
with impact.d3.collision.Detector.discrete.gjk_pair;
with impact.d3.Vector,
     impact.d3.Matrix;

with impact.d3.Shape.convex;
with impact.d3.Transform;
with impact.d3.Scalar;
with impact.d3.collision.Detector.discrete;
with impact.d3.collision.Proxy;



package body impact.d3.collision.Algorithm.activating.convex_convex
--
--  Specialized capsule-capsule collision algorithm has been added for Bullet 2.75 release to increase ragdoll performance.
--
--  If you experience problems with capsule-capsule collision, try to define BT_DISABLE_CAPSULE_CAPSULE_COLLIDER and report it in the Bullet forums
--  with reproduction case.
--
is


--  //define BT_DISABLE_CAPSULE_CAPSULE_COLLIDER 1
--  //#define ZERO_MARGIN
--


   ------------
   --- Globals
   --

   disableCcd                : constant Boolean  := False;
   gContactBreakingThreshold :          math.Real;






   ---------------
   --- CreateFunc
   --


   function to_CreateFunc (simplexSolver : access impact.d3.collision.simplex_Solver.item'Class;
                           pdSolver      : access impact.d3.collision.convex_penetration_depth_Solver.item'Class) return CreateFunc
   is
      Self : CreateFunc;
   begin
      self.m_numPerturbationIterations          := 0;
      self.m_minimumPointsPerturbationThreshold := 3;

      self.m_simplexSolver := simplexSolver;
      self.m_pdSolver      := pdSolver;

      return Self;
   end to_CreateFunc;







   function new_CreateFunc (simplexSolver : access impact.d3.collision.simplex_Solver.item'Class;
                            pdSolver      : access impact.d3.collision.convex_penetration_depth_Solver.item'Class) return access CreateFunc'Class
   is
   begin
      return new CreateFunc'(to_CreateFunc (simplexSolver, pdSolver));
   end new_CreateFunc;





   overriding procedure destruct                 (Self : in out CreateFunc)
   is
   begin
      null;
   end destruct;



   overriding function  CreateCollisionAlgorithm (Self : in     CreateFunc;   info  : in     AlgorithmConstructionInfo;
                                                                   body0,
                                       body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
   begin
      return new impact.d3.collision.Algorithm.activating.convex_convex.item'
        (to_convex_convex_Algorithm
           (info.m_manifold,                    info,
            body0,                              body1,
            Self.m_simplexSolver,               Self.m_pdSolver,
            Self.m_numPerturbationIterations,   Self.m_minimumPointsPerturbationThreshold));
   end CreateCollisionAlgorithm;





   ------------
   --- Utility
   --


   procedure segmentsClosestPoints (ptsVector,
                                    offsetA, offsetB : in out math.Vector_3;
                                    tA,      tB      : in out math.Real;
                                    translation,
                                    dirA,    dirB    : in math.Vector_3;
                                    hlenA,   hlenB   : in math.Real)
   is
      use impact.d3.Vector;

      dirA_dot_dirB  : constant math.Real := dot (dirA,  dirB);
      dirA_dot_trans : constant math.Real := dot (dirA,  translation);
      dirB_dot_trans : constant math.Real := dot (dirB,  translation);

      denom          : constant math.Real := 1.0  -  dirA_dot_dirB * dirA_dot_dirB;

   begin
      --  Compute the parameters of the closest points on each line segment.
      --

      if denom = 0.0  then
         tA := 0.0;
      else
         tA := (dirA_dot_trans - dirB_dot_trans * dirA_dot_dirB) / denom;

         if    tA < -hlenA then   tA := -hlenA;
         elsif tA >  hlenA then   tA :=  hlenA;
         end if;
      end if;

      tB := tA * dirA_dot_dirB - dirB_dot_trans;


      if tB < -hlenB then
         tB := -hlenB;
         tA := tB * dirA_dot_dirB  +  dirA_dot_trans;

         if    tA < -hlenA then   tA := -hlenA;
         elsif tA >  hlenA then   tA := hlenA;
         end if;

      elsif  tB > hlenB  then
         tB := hlenB;
         tA := tB * dirA_dot_dirB  +  dirA_dot_trans;

         if    tA < -hlenA then   tA := -hlenA;
         elsif tA >  hlenA then   tA := hlenA;
         end if;
      end if;


      --  compute the closest points relative to segment centers.
      --

      offsetA := dirA * tA;
      offsetB := dirB * tB;

      ptsVector := translation - offsetA + offsetB;

   end segmentsClosestPoints;





   function capsuleCapsuleDistance (normalOnB,      pointOnB       : in out math.Vector_3;
                                    capsuleLengthA, capsuleRadiusA,
                                    capsuleLengthB, capsuleRadiusB : in     math.Real;
                                    capsuleAxisA,   capsuleAxisB   : in     Integer;
                                    transformA,     transformB     : in     Transform_3d;
                                    distanceThreshold              : in     math.Real) return math.Real
   is
      use impact.d3.Vector, impact.d3.Transform, impact.d3.Matrix;

      directionA   : constant math.Vector_3 := getColumn (getBasis (transformA),  capsuleAxisA);
      directionB   : constant math.Vector_3 := getColumn (getBasis (transformB),  capsuleAxisB);

      translationA : constant math.Vector_3 := getOrigin (transformA);
      translationB : constant math.Vector_3 := getOrigin (transformB);

      translation  : constant math.Vector_3 := translationB - translationA;      -- translation between centers
      ptsVector    : math.Vector_3;                                     -- the vector between the closest points

      offsetA,
      offsetB      : math.Vector_3;                                     -- offsets from segment centers to their closest points

      tA, tB       : math.Real;                                         -- parameters on line segment
      distance,
      lenSqr       : math.Real;

      q            : math.Vector_3;

   begin
      --  Compute the closest points of the capsule line segments.
      --

      segmentsClosestPoints (ptsVector,
                             offsetA, offsetB,
                             tA,      tB,
                             translation,
                             directionA,     directionB,
                             capsuleLengthA, capsuleLengthB);

      distance := length (ptsVector) - capsuleRadiusA - capsuleRadiusB;

      if distance > distanceThreshold then
         return distance;
      end if;

      lenSqr := length2 (ptsVector);

      if lenSqr <= impact.d3.Scalar.SIMD_EPSILON * impact.d3.Scalar.SIMD_EPSILON then      -- degenerate case where 2 capsules are likely at the same location: take a vector tangential to 'directionA'
         btPlaneSpace1 (directionA, normalOnB, q);
      else                                            -- compute the contact normal
         normalOnB := ptsVector * (-impact.d3.Scalar.btRecipSqrt (lenSqr));
      end if;

      pointOnB := getOrigin (transformB)  +  offsetB  +  normalOnB * capsuleRadiusB;

      return distance;

   end capsuleCapsuleDistance;








   -----------------------------
   --- btPerturbedContactResult
   --




   type btPerturbedContactResult is new impact.d3.collision.manifold_Result.item with
      record
         m_originalManifoldResult : access impact.d3.collision.manifold_Result.item;

         m_transformA,
         m_transformB,
         m_unPerturbedTransform   : Transform_3d;

         m_perturbA               : Boolean;
      end record;


   overriding procedure destruct (Self : in out btPerturbedContactResult);

   overriding procedure addContactPoint (Self : in out btPerturbedContactResult;   normalOnBInWorld : in math.Vector_3;
                                                                        pointInWorld     : in math.Vector_3;
                                                                        orgDepth         : in math.Real);



   function to_btPerturbedContactResult (originalResult         : access impact.d3.collision.manifold_Result.item;
                                         transformA, transformB : in     Transform_3d;
                                         unPerturbedTransform   : in     Transform_3d;
                                         perturbA               : in     Boolean) return btPerturbedContactResult
   is
      Self : constant btPerturbedContactResult := (impact.d3.collision.manifold_Result.item with
                                          m_originalManifoldResult => originalResult,
                                          m_transformA             => transformA,
                                          m_transformB             => transformB,
                                          m_unPerturbedTransform   => unPerturbedTransform,
                                          m_perturbA               => perturbA);
   begin
      return Self;
   end to_btPerturbedContactResult;




   overriding procedure destruct (Self : in out btPerturbedContactResult)
   is
   begin
      null;
   end destruct;






   overriding procedure addContactPoint (Self : in out btPerturbedContactResult;   normalOnBInWorld : in math.Vector_3;
                                                                        pointInWorld     : in math.Vector_3;
                                                                        orgDepth         : in math.Real)
   is
      use linear_Algebra_3d,  impact.d3.Transform, impact.d3.Vector;

      endPt,
      startPt   : math.Vector_3;

      newDepth  : math.Real;

--        newNormal : math.Vector_3;
      endPtOrg  : math.Vector_3;

   begin
      if Self.m_perturbA then
         endPtOrg := pointInWorld + normalOnBInWorld * orgDepth;
         endPt    := (Self.m_unPerturbedTransform * inverse (Self.m_transformA)) * endPtOrg;
         newDepth := dot (endPt - pointInWorld,  normalOnBInWorld);
         startPt  := endPt  +  normalOnBInWorld * newDepth;

      else
         endPt    := pointInWorld + normalOnBInWorld*orgDepth;
         startPt  := (Self.m_unPerturbedTransform * inverse (Self.m_transformB)) * pointInWorld;
         newDepth := dot (endPt - startPt,  normalOnBInWorld);
      end if;


      Self.m_originalManifoldResult.addContactPoint (normalOnBInWorld, startPt, newDepth);
   end addContactPoint;








   ----------------------------
   --- impact.d3.collision.Algorithm.activating.convex_convex
   --

   function to_convex_convex_Algorithm (mf                                 : access impact.d3.Manifold.item'Class;
                                        ci                                 : in     AlgorithmConstructionInfo;
                                        body0, body1                       : access impact.d3.Object.item'Class;
                                        simplexSolver                      : access impact.d3.collision.simplex_Solver.item'Class;
                                        pdSolver                           : access impact.d3.collision.convex_penetration_depth_Solver.item'Class;
                                        numPerturbationIterations          : in     Integer;
                                        minimumPointsPerturbationThreshold : in     Integer) return impact.d3.collision.Algorithm.activating.convex_convex.item
   is
      Self : Item := (Algorithm.activating.item with -- impact.d3.collision.Algorithm.activating.Forge.to_activating_Algorithm (ci, body0, body1) with
                      m_simplexSolver    => simplexSolver,
                      m_pdSolver         => pdSolver,
                      m_ownManifold      => False,
                      m_manifoldPtr      => mf,
                      m_lowLevelOfDetail => False,
                      m_sepDistance      => to_btConvexSeparatingDistanceUtil (impact.d3.Shape.convex.view (body0.getCollisionShape).getAngularMotionDisc,
                                                                               impact.d3.Shape.convex.view (body1.getCollisionShape).getAngularMotionDisc),
                      m_numPerturbationIterations          => numPerturbationIterations,
                      m_minimumPointsPerturbationThreshold => minimumPointsPerturbationThreshold);
   begin
      Self.define (ci, body0, body1);
      return Self;
   end to_convex_convex_Algorithm;








   overriding procedure destruct (Self : in out Item)
   is
   begin
      if Self.m_ownManifold then

         if Self.m_manifoldPtr /= null then
            Self.get_m_dispatcher.releaseManifold (Self.m_manifoldPtr);
         end if;

      end if;
   end destruct;






   overriding procedure processCollision (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                           dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                               resultOut    :    out impact.d3.collision.manifold_Result.item)
   is
      use impact.d3.Vector, math.Vectors;

      min0,
      min1 : access impact.d3.Shape.convex.item'Class;

--        normalOnB,
--        pointOnBWorld : math.Vector_3;

      input           : impact.d3.collision.Detector.discrete.ClosestPointInput;
      gjkPairDetector : impact.d3.collision.Detector.discrete.gjk_pair.Item;

      sepDist         : math.Real;
   begin

      if Self.m_manifoldPtr = null then
         --  swapped?
         Self.m_manifoldPtr := Self.get_m_dispatcher.getNewManifold (body0, body1);
         Self.m_ownManifold := True;
      end if;

      resultOut.setPersistentManifold (Self.m_manifoldPtr);

      --  comment-out next line to test multi-contact generation
      --  resultOut->getPersistentManifold()->clearManifold();


      min0 := impact.d3.Shape.convex.View (body0.getCollisionShape);
      min1 := impact.d3.Shape.convex.View (body1.getCollisionShape);


      --  #ifndef BT_DISABLE_CAPSULE_CAPSULE_COLLIDER
      --          if ((min0->getShapeType() == CAPSULE_SHAPE_PROXYTYPE) && (min1->getShapeType() == CAPSULE_SHAPE_PROXYTYPE))
      --          {
      --                  btCapsuleShape* capsuleA = (btCapsuleShape*) min0;
      --                  btCapsuleShape* capsuleB = (btCapsuleShape*) min1;
      --                  impact.d3.Vector localScalingA = capsuleA->getLocalScaling();
      --                  impact.d3.Vector localScalingB = capsuleB->getLocalScaling();
      --
      --                  impact.d3.Scalar threshold = m_manifoldPtr->getContactBreakingThreshold();
      --
      --                  impact.d3.Scalar dist = capsuleCapsuleDistance(normalOnB,        pointOnBWorld,capsuleA->getHalfHeight(),capsuleA->getRadius(),
      --                          capsuleB->getHalfHeight(),capsuleB->getRadius(),capsuleA->getUpAxis(),capsuleB->getUpAxis(),
      --                          body0->getWorldTransform(),body1->getWorldTransform(),threshold);
      --
      --                  if (dist<threshold)
      --                  {
      --                          btAssert(normalOnB.length2()>=(SIMD_EPSILON*SIMD_EPSILON));
      --                          resultOut->addContactPoint(normalOnB,pointOnBWorld,dist);
      --                  }
      --                  resultOut->refreshContactPoints();
      --                  return;
      --          }
      --  #endif //BT_DISABLE_CAPSULE_CAPSULE_COLLIDER




      if dispatchInfo.m_useConvexConservativeDistanceUtil then
         Self.m_sepDistance.updateSeparatingDistance (body0.getWorldTransform.all,
                                                      body1.getWorldTransform.all);
      end if;



      if        not dispatchInfo.m_useConvexConservativeDistanceUtil
        or else Self.m_sepDistance.getConservativeSeparatingDistance <= 0.0
      then
         gjkPairDetector := impact.d3.collision.Detector.discrete.gjk_pair.to_gjk_pair_Detector (min0, min1, Self.m_simplexSolver, Self.m_pdSolver);
         --  TODO: if (dispatchInfo.m_useContinuous)
         gjkPairDetector.setMinkowskiA (min0);
         gjkPairDetector.setMinkowskiB (min1);

         if dispatchInfo.m_useConvexConservativeDistanceUtil then
            input.m_maximumDistanceSquared := BT_LARGE_FLOAT;
         else
            input.m_maximumDistanceSquared := min0.getMargin + min1.getMargin + Self.m_manifoldPtr.getContactBreakingThreshold;
            input.m_maximumDistanceSquared := input.m_maximumDistanceSquared * input.m_maximumDistanceSquared;
         end if;

         input.m_transformA := body0.getWorldTransform.all;
         input.m_transformB := body1.getWorldTransform.all;

         sepDist := 0.0;

         if dispatchInfo.m_useConvexConservativeDistanceUtil then
            sepDist := gjkPairDetector.getCachedSeparatingDistance;

            if sepDist > impact.d3.Scalar.SIMD_EPSILON then
               sepDist := sepDist + dispatchInfo.m_convexConservativeDistanceThreshold;
               --  now perturbe directions to get multiple contact points
            end if;
         end if;


         if         min0.isPolyhedral
           and then min1.isPolyhedral
         then
            declare
               use type impact.d3.collision.Proxy.BroadphaseNativeTypes;

               type btDummyResult is new impact.d3.collision.Detector.discrete.Result with null record;

               overriding procedure addContactPoint (Self : in out btDummyResult;   normalOnBInWorld : in math.Vector_3;
                                                                         pointInWorld     : in math.Vector_3;
                                                                         depth            : in math.Real  ) is null;
--                  struct btDummyResult : public impact.d3.collision.Detector.discrete::Result
--                  {
--                          virtual void setShapeIdentifiersA(int partId0,int index0){}
--                          virtual void setShapeIdentifiersB(int partId1,int index1){}
--                          virtual void addContactPoint(const impact.d3.Vector& normalOnBInWorld,const impact.d3.Vector& pointInWorld,impact.d3.Scalar depth)
--                          {
--                          }
--                  };

               dummy : btDummyResult;


               polyhedronA : constant impact.d3.Shape.convex.internal.polyhedral.view := impact.d3.Shape.convex.internal.polyhedral.view (min0);
               polyhedronB : constant impact.d3.Shape.convex.internal.polyhedral.view := impact.d3.Shape.convex.internal.polyhedral.view (min1);

            begin
               if         polyhedronA.getConvexPolyhedron /= null
                 and then polyhedronB.getConvexPolyhedron /= null
      then
                  declare
                     threshold           : math.Real := Self.m_manifoldPtr.getContactBreakingThreshold;

                     minDist             : math.Real := -1.0e30;
                     sepNormalWorldSpace : math.Vector_3;
                     foundSepAxis        : Boolean   := True;
                     l2                  : math.Real;
                  begin
                     if dispatchInfo.m_enableSatConvex then
                        foundSepAxis := impact.d3.collision.polyhedral_contact_Clipping.findSeparatingAxis (polyhedronA.getConvexPolyhedron.all, polyhedronB.getConvexPolyhedron.all,
                                                                                        body0.getWorldTransform.all,
                                                                                        body1.getWorldTransform.all,
                                                                                        sepNormalWorldSpace);
                     else
                        gjkPairDetector.getClosestPoints (input, dummy);
                        l2 := length2 (gjkPairDetector.getCachedSeparatingAxis);

                        if l2 > impact.d3.Scalar.SIMD_EPSILON then
                           sepNormalWorldSpace := gjkPairDetector.getCachedSeparatingAxis * (1.0 / l2);
                           minDist             := gjkPairDetector.getCachedSeparatingDistance - min0.getMargin - min1.getMargin;
                           foundSepAxis        := gjkPairDetector.getCachedSeparatingDistance < min0.getMargin + min1.getMargin;
                        end if;
                     end if;

                     if foundSepAxis then
                        impact.d3.collision.polyhedral_contact_Clipping.clipHullAgainstHull (sepNormalWorldSpace, polyhedronA.getConvexPolyhedron.all,
                                                                         polyhedronB.getConvexPolyhedron.all,
                                                                         body0.getWorldTransform.all,
                                                                         body1.getWorldTransform.all,
                                                                         minDist - threshold,
                                                                         threshold,
                                                                         resultOut);
                     end if;

                     if Self.m_ownManifold then
                        resultOut.refreshContactPoints;
                     end if;

                     return;
                  end;

               else
                        --  we can also deal with convex versus triangle (without connectivity data)
                     if         polyhedronA.getConvexPolyhedron /= null
                       and then polyhedronB.getShapeType = impact.d3.collision.Proxy.TRIANGLE_SHAPE_PROXYTYPE
                     then
                     declare
                        use linear_Algebra_3d, impact.d3.Transform;
                        vertices : impact.d3.collision.polyhedral_contact_Clipping.btVertexArray;
                        tri      : constant impact.d3.Shape.convex.internal.polyhedral.triangle.view := impact.d3.Shape.convex.internal.polyhedral.triangle.view (polyhedronB);

                        threshold : math.Real;

                        sepNormalWorldSpace : math.Vector_3;
                        minDist,
                                maxDist : math.Real;

                        foundSepAxis : Boolean;

                        l2 : math.Real;
                     begin
                        vertices.append (body1.getWorldTransform.all * tri.m_vertices1 (1));
                        vertices.append (body1.getWorldTransform.all * tri.m_vertices1 (2));
                        vertices.append (body1.getWorldTransform.all * tri.m_vertices1 (3));

                        -- tri->initializePolyhedralFeatures();

                        threshold := Self.m_manifoldPtr.getContactBreakingThreshold;

                                 minDist := -1.0e30;
                                 maxDist :=  threshold;

                                 foundSepAxis := False;

                        gjkPairDetector.getClosestPoints (input, dummy);

                        l2 := length2 (gjkPairDetector.getCachedSeparatingAxis);
                        if l2 > impact.d3.Scalar.SIMD_EPSILON then
                                                sepNormalWorldSpace := gjkPairDetector.getCachedSeparatingAxis * (1.0 / l2);
                                                minDist             := gjkPairDetector.getCachedSeparatingDistance - min0.getMargin - min1.getMargin;
                                                foundSepAxis        := True;
                        end if;


                        if foundSepAxis then
                           impact.d3.collision.polyhedral_contact_Clipping.clipFaceAgainstHull (sepNormalWorldSpace, polyhedronA.getConvexPolyhedron.all,
                                                        body0.getWorldTransform.all, vertices, minDist - threshold, maxDist, resultOut);
                        end if;


                        if Self.m_ownManifold then
                           resultOut.refreshContactPoints;
                        end if;

                        return;
                     end;
                     end if;

               end if;

            end;
         end if;

         gjkPairDetector.getClosestPoints (input, resultOut);

         --  now perform 'm_numPerturbationIterations' collision queries with the perturbated collision objects

         --  perform perturbation when more then 'm_minimumPointsPerturbationThreshold' points
         --
         if         Self.m_numPerturbationIterations /= 0
           and then resultOut.getPersistentManifold.getNumContacts < Self.m_minimumPointsPerturbationThreshold
         then
            declare
               v0, v1              : math.Vector_3;
               sepNormalWorldSpace : math.Vector_3;
               l2                  : constant math.Real    := length2 (gjkPairDetector.getCachedSeparatingAxis);
            begin
               if l2 > impact.d3.Scalar.SIMD_EPSILON then
                  sepNormalWorldSpace := gjkPairDetector.getCachedSeparatingAxis * (1.0 / l2);

                  btPlaneSpace1 (sepNormalWorldSpace, v0, v1);

                  declare
                     perturbeA            : Boolean            := True;
                     angleLimit           : constant math.Real := 0.125 * impact.d3.Scalar.SIMD_PI;

                     perturbeAngle        : math.Real;

                     radiusA              : math.Real := min0.getAngularMotionDisc;
                     radiusB              : constant math.Real := min1.getAngularMotionDisc;

                     unPerturbedTransform : Transform_3d;
                  begin
                        if radiusA < radiusB then

                        perturbeAngle := gContactBreakingThreshold / radiusA;
                        perturbeA     := True;
                        else

                        perturbeAngle := gContactBreakingThreshold / radiusB;
                        perturbeA     := False;
                        end if;
                        if  perturbeAngle > angleLimit then
                        perturbeAngle := angleLimit;
                        end if;

                        if perturbeA then
                        unPerturbedTransform := input.m_transformA;
                        else
                        unPerturbedTransform := input.m_transformB;
                        end if;

                        for i in 1 .. Self.m_numPerturbationIterations
                        loop
                           if length2 (v0) > impact.d3.Scalar.SIMD_EPSILON then
                              declare
                                 use impact.d3.Quaternions, impact.d3.Transform;
                              perturbeRot    : constant math.Quaternion := to_Quaternion (v0, perturbeAngle);
                              iterationAngle : constant math.Real       := math.Real (i - 1) * (impact.d3.Scalar.SIMD_2_PI / math.Real (Self.m_numPerturbationIterations));
                              rotq           : constant math.Quaternion := to_Quaternion (sepNormalWorldSpace, iterationAngle);

                                 perturbedResultOut : btPerturbedContactResult;
                              begin
                              if perturbeA then
                                 setBasis (input.m_transformA,    linear_Algebra_3d.to_Matrix (multiply (inverse (rotq),
                                                                                               multiply (perturbeRot, rotq)))
                                                               *  getBasis (body0.getWorldTransform).all);
                                 input.m_transformB := body1.getWorldTransform.all;
                              else
                                 input.m_transformA := body0.getWorldTransform.all;
                                 setBasis (input.m_transformB,     linear_Algebra_3d.to_Matrix (multiply (inverse (rotq),
                                                                                                multiply (perturbeRot, rotq)))
                                                                *  getBasis (body1.getWorldTransform).all);
                              end if;

                              perturbedResultOut := to_btPerturbedContactResult (resultOut'Access, input.m_transformA, input.m_transformB,
                                                                                   unPerturbedTransform, perturbeA);
                              gjkPairDetector.getClosestPoints (input, perturbedResultOut);
                              end;
                           end if;
                        end loop;
                  end;
               end if;
            end;
         end if;



         if         dispatchInfo.m_useConvexConservativeDistanceUtil
           and then sepDist > impact.d3.Scalar.SIMD_EPSILON
         then
            Self.m_sepDistance.initSeparatingDistance (gjkPairDetector.getCachedSeparatingAxis,
                                                       sepDist,
                                                       body0.getWorldTransform.all,
                                                       body1.getWorldTransform.all);
         end if;

      end if;


      if Self.m_ownManifold then
         resultOut.refreshContactPoints;
      end if;

   end processCollision;










   overriding function  calculateTimeOfImpact (Self : in     Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                          dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                          resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real
   is
      pragma Unreferenced (Self, dispatchInfo, resultOut);
      use impact.d3.Vector, impact.d3.Transform;

      col0 : access impact.d3.Object.item'Class renames body0;
      col1 : access impact.d3.Object.item'Class renames body1;


      resultFraction : math.Real := 1.0;

      squareMot0     : math.Real := length2 (getOrigin (col0.getInterpolationWorldTransform).all - getOrigin (col0.getWorldTransform).all);
      squareMot1     : math.Real := length2 (getOrigin (col1.getInterpolationWorldTransform).all - getOrigin (col1.getWorldTransform).all);

   begin
      --  Rather then checking ALL pairs, only calculate TOI when motion exceeds threshold

      --  Linear motion for one of objects needs to exceed m_ccdSquareMotionThreshold
      --  col0->m_worldTransform,

      if         squareMot0 < col0.getCcdSquareMotionThreshold
        and then squareMot1 < col1.getCcdSquareMotionThreshold
      then
         return resultFraction;
      end if;


      if disableCcd then
         return 1.0;
      end if;


      --  An adhoc way of testing the Continuous Collision Detection algorithms
      --  One object is approximated as a sphere, to simplify things
      --  Starting in penetration should report no time of impact
      --  For proper CCD, better accuracy and handling of 'allowed' penetration should be added
      --  also the mainloop of the physics should have a kind of toi queue (something like Brian Mirtich's application of Timewarp for Rigidbodies)


      --  Convex0 against sphere for Convex1
      --
      declare
         convex0        : constant impact.d3.Shape.convex.view     := impact.d3.Shape.convex.view (col0.getCollisionShape);

         sphere1        : aliased impact.d3.Shape.convex.internal.sphere.item     := impact.d3.Shape.convex.internal.sphere.to_sphere_Shape (col1.getCcdSweptSphereRadius);    -- todo: allow non-zero sphere sizes, for better approximation
         result         : aliased impact.d3.collision.convex_Raycast.CastResult;

         voronoiSimplex : aliased impact.d3.collision.simplex_Solver.voronoi.item;

         --  SubsimplexConvexCast ccd0(&sphere,min0,&voronoiSimplex);
         --  Simplification, one object is simplified as a sphere

         ccd1 : aliased impact.d3.collision.convex_Raycast.gjk.item := impact.d3.collision.convex_Raycast.gjk.to_gjk_convex_Raycast (convex0, sphere1'Access, voronoiSimplex'Access);
         --  ContinuousConvexCollision ccd(min0,min1,&voronoiSimplex,0);

      begin
         if ccd1.calcTimeOfImpact (col0.getWorldTransform.all,  col0.getInterpolationWorldTransform,
                                   col1.getWorldTransform.all,  col1.getInterpolationWorldTransform,   result'Access)
         then   -- store result.m_fraction in both bodies

            if col0.getHitFraction > result.m_fraction then
               col0.setHitFraction (result.m_fraction);
            end if;

            if col1.getHitFraction > result.m_fraction then
               col1.setHitFraction (result.m_fraction);
            end if;

            if resultFraction > result.m_fraction then
               resultFraction := result.m_fraction;
            end if;

         end if;
      end;

      --  Sphere (for convex0) against Convex1
      --
      declare
         convex1        : constant impact.d3.Shape.convex.view := impact.d3.Shape.convex.view (col1.getCollisionShape);
         sphere0        : aliased impact.d3.Shape.convex.internal.sphere.item := impact.d3.Shape.convex.internal.sphere.to_sphere_Shape (col0.getCcdSweptSphereRadius);    -- todo: allow non-zero sphere sizes, for better approximation

         result         : aliased impact.d3.collision.convex_Raycast.CastResult;
         voronoiSimplex : aliased impact.d3.collision.simplex_Solver.voronoi.item;

         --  SubsimplexConvexCast ccd0(&sphere,min0,&voronoiSimplex);
         --  Simplification, one object is simplified as a sphere

         ccd1          : aliased impact.d3.collision.convex_Raycast.gjk.item := impact.d3.collision.convex_Raycast.gjk.to_gjk_convex_Raycast (sphere0'Access, convex1, voronoiSimplex'Access);
         --  ContinuousConvexCollision ccd(min0,min1,&voronoiSimplex,0);
      begin
         if ccd1.calcTimeOfImpact (col0.getWorldTransform.all, col0.getInterpolationWorldTransform,
                                    col1.getWorldTransform.all, col1.getInterpolationWorldTransform,   result'Access)
         then

            --  store result.m_fraction in both bodies

            if col0.getHitFraction > result.m_fraction then
               col0.setHitFraction (result.m_fraction);
            end if;

            if col1.getHitFraction > result.m_fraction then
               col1.setHitFraction (result.m_fraction);
            end if;

            if resultFraction > result.m_fraction then
               resultFraction := result.m_fraction;
            end if;
         end if;
      end;

      return resultFraction;

   end calculateTimeOfImpact;








   overriding procedure getAllContactManifolds (Self : in out Item;   manifoldArray :    out impact.d3.collision.Algorithm.btManifoldArray)
   is
   begin
      --  Should we use m_ownManifold to avoid adding duplicates ?

      if         Self.m_manifoldPtr /= null
        and then Self.m_ownManifold
      then
         manifoldArray.append (Self.m_manifoldPtr);
      end if;

   end getAllContactManifolds;




   procedure setLowLevelOfDetail    (Self : in out Item;   useLowLevel : in Boolean)
   is
   begin
      Self.m_lowLevelOfDetail := useLowLevel;
   end setLowLevelOfDetail;





   function  getManifold            (Self : in     Item) return access impact.d3.Manifold.item'Class
   is
   begin
      return Self.m_manifoldPtr;
   end getManifold;




end impact.d3.collision.Algorithm.activating.convex_convex;











--  impact.d3.Scalar        impact.d3.collision.Algorithm.activating.convex_convex::calculateTimeOfImpact(impact.d3.Object* col0,impact.d3.Object* col1,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.collision.manifold_Result* resultOut)
--  {
--          (void)resultOut;
--          (void)dispatchInfo;
--          ///Rather then checking ALL pairs, only calculate TOI when motion exceeds threshold
--
--          ///Linear motion for one of objects needs to exceed m_ccdSquareMotionThreshold
--          ///col0->m_worldTransform,
--          impact.d3.Scalar resultFraction = impact.d3.Scalar(1.);
--
--
--          impact.d3.Scalar squareMot0 = (col0->getInterpolationWorldTransform().getOrigin() - col0->getWorldTransform().getOrigin()).length2();
--          impact.d3.Scalar squareMot1 = (col1->getInterpolationWorldTransform().getOrigin() - col1->getWorldTransform().getOrigin()).length2();
--
--          if (squareMot0 < col0->getCcdSquareMotionThreshold() &&
--                  squareMot1 < col1->getCcdSquareMotionThreshold())
--                  return resultFraction;
--
--          if (disableCcd)
--                  return impact.d3.Scalar(1.);
--
--
--          //An adhoc way of testing the Continuous Collision Detection algorithms
--          //One object is approximated as a sphere, to simplify things
--          //Starting in penetration should report no time of impact
--          //For proper CCD, better accuracy and handling of 'allowed' penetration should be added
--          //also the mainloop of the physics should have a kind of toi queue (something like Brian Mirtich's application of Timewarp for Rigidbodies)
--
--
--          /// Convex0 against sphere for Convex1
--          {
--                  impact.d3.Shape.convex* convex0 = static_cast<impact.d3.Shape.convex*>(col0->getCollisionShape());
--
--                  impact.d3.Shape.convex.internal.sphere        sphere1(col1->getCcdSweptSphereRadius()); //todo: allow non-zero sphere sizes, for better approximation
--                  impact.d3.collision.convex_Raycast::CastResult result;
--                  impact.d3.collision.simplex_Solver.voronoi voronoiSimplex;
--                  //SubsimplexConvexCast ccd0(&sphere,min0,&voronoiSimplex);
--                  ///Simplification, one object is simplified as a sphere
--                  impact.d3.collision.convex_Raycast.gjk ccd1( convex0 ,&sphere1,&voronoiSimplex);
--                  //ContinuousConvexCollision ccd(min0,min1,&voronoiSimplex,0);
--                  if (ccd1.calcTimeOfImpact(col0->getWorldTransform(),col0->getInterpolationWorldTransform(),
--                          col1->getWorldTransform(),col1->getInterpolationWorldTransform(),result))
--                  {
--
--                          //store result.m_fraction in both bodies
--
--                          if (col0->getHitFraction()> result.m_fraction)
--                                  col0->setHitFraction( result.m_fraction );
--
--                          if (col1->getHitFraction() > result.m_fraction)
--                                  col1->setHitFraction( result.m_fraction);
--
--                          if (resultFraction > result.m_fraction)
--                                  resultFraction = result.m_fraction;
--
--                  }
--
--
--
--
--          }
--
--          /// Sphere (for convex0) against Convex1
--          {
--                  impact.d3.Shape.convex* convex1 = static_cast<impact.d3.Shape.convex*>(col1->getCollisionShape());
--
--                  impact.d3.Shape.convex.internal.sphere        sphere0(col0->getCcdSweptSphereRadius()); //todo: allow non-zero sphere sizes, for better approximation
--                  impact.d3.collision.convex_Raycast::CastResult result;
--                  impact.d3.collision.simplex_Solver.voronoi voronoiSimplex;
--                  //SubsimplexConvexCast ccd0(&sphere,min0,&voronoiSimplex);
--                  ///Simplification, one object is simplified as a sphere
--                  impact.d3.collision.convex_Raycast.gjk ccd1(&sphere0,convex1,&voronoiSimplex);
--                  //ContinuousConvexCollision ccd(min0,min1,&voronoiSimplex,0);
--                  if (ccd1.calcTimeOfImpact(col0->getWorldTransform(),col0->getInterpolationWorldTransform(),
--                          col1->getWorldTransform(),col1->getInterpolationWorldTransform(),result))
--                  {
--
--                          //store result.m_fraction in both bodies
--
--                          if (col0->getHitFraction()        > result.m_fraction)
--                                  col0->setHitFraction( result.m_fraction);
--
--                          if (col1->getHitFraction() > result.m_fraction)
--                                  col1->setHitFraction( result.m_fraction);
--
--                          if (resultFraction > result.m_fraction)
--                                  resultFraction = result.m_fraction;
--
--                  }
--          }
--
--          return resultFraction;
--
--  }

