



package body impact.d3.collision.Algorithm.activating.convex_concave
is




   --- btConvexTriangleCallback
   --


   function to_btConvexTriangleCallback (dispatcher   : access impact.d3.Dispatcher.item'Class;
                                         body0, body1 : access impact.d3.Object.item'Class;
                                         isSwapped    : in     Boolean                   ) return btConvexTriangleCallback
   is
      Self : btConvexTriangleCallback;
   begin
      Self.m_dispatcher := dispatcher;

      if isSwapped then
         Self.m_convexBody := body1;
         Self.m_triBody    := body0;
      else
         Self.m_convexBody := body0;
         Self.m_triBody    := body1;
      end if;


      --  create the manifold from the dispatcher 'manifold pool'
      --
      Self.m_manifoldPtr := Self.m_dispatcher.getNewManifold (Self.m_convexBody, Self.m_triBody);
      Self.clearCache;


      return Self;
   end to_btConvexTriangleCallback;









   overriding procedure destruct (Self : in out btConvexTriangleCallback)
   is
   begin
      Self.clearCache;
      Self.m_dispatcher.releaseManifold (Self.m_manifoldPtr);
   end destruct;





   procedure setTimeStepAndCounters (Self : in out btConvexTriangleCallback;   collisionMarginTriangle : in     math.Real;
                                                                               dispatchInfo            : in     impact.d3.Dispatcher.DispatcherInfo;
                                     resultOut               : access impact.d3.collision.manifold_Result.item)
   is
   begin
      null;
   end setTimeStepAndCounters;




   type Object_view is access all impact.d3.Object.item'Class;



   overriding procedure processTriangle (Self : in out btConvexTriangleCallback;   triangle      : access math.Matrix_3x3;
                                                                        partId        : in     Integer;
                                                                        triangleIndex : in     Integer)
   is
      pragma Unreferenced (triangle, partId, triangleIndex);
      ci :        AlgorithmConstructionInfo;
      pragma Unreferenced (ci);
      ob : access impact.d3.Object.item'Class;
      pragma Unreferenced (ob);
   begin
        -- just for debugging purposes
        -- printf("triangle %d",m_triangleCount++);


      -- aabb filter is already applied!

      ci.m_dispatcher1 := Self.m_dispatcher;

      ob := Object_view (Self.m_triBody);


--        if Self.m_convexBody.getCollisionShape.isConvex then
--           declare
--                  tm       :        impact.d3.Shape.convex.internal.polyhedral.triangle.item            := to_impact.d3.Shape.convex.internal.polyhedral.triangle (triangle (1), triangle (2), triangle (3));
--                  tmpShape : access impact.d3.Shape.item    'Class;
--                  colAlgo  : access impact.d3.collision.Algorithm.item'Class;
--           begin
--                  tm.setMargin (Self.m_collisionMarginTriangle);
--
--                  tmpShape := ob.getCollisionShape;
--                  ob.internalSetTemporaryCollisionShape (tm'access);
--
--                  colAlgo := ci.m_dispatcher1.findAlgorithm (Self.m_convexBody, Self.m_triBody, Self.m_manifoldPtr);
--
--                  if Self.m_resultOut.getBody0Internal = Self.m_triBody then
--                          Self.m_resultOut.setShapeIdentifiersA (partId, triangleIndex);
--                  else
--                          Self.m_resultOut.setShapeIdentifiersB (partId, triangleIndex);
--                  end if;
--
--                  colAlgo.processCollision (Self.m_convexBody,  Self.m_triBody,  Self.m_dispatchInfoPtr.all,  Self.m_resultOut.all);
--                  colAlgo.destruct;   -- colAlgo->~impact.d3.collision.Algorithm();   -- tbd
--                  ci.m_dispatcher1.freeCollisionAlgorithm (colAlgo);
--              ob.internalSetTemporaryCollisionShape (tmpShape);
--              end;
--          end if;

   end processTriangle;


--  void btConvexTriangleCallback::processTriangle(impact.d3.Vector* triangle,int partId, int triangleIndex)
--  {
--
--          //just for debugging purposes
--          //printf("triangle %d",m_triangleCount++);
--
--
--          //aabb filter is already applied!
--
--          impact.d3.collision.AlgorithmConstructionInfo ci;
--          ci.m_dispatcher1 = m_dispatcher;
--
--          impact.d3.Object* ob = static_cast<impact.d3.Object*>(m_triBody);
--
--
--  #if 0
--          ///debug drawing of the overlapping triangles
--          if (m_dispatchInfoPtr && m_dispatchInfoPtr->m_debugDraw && (m_dispatchInfoPtr->m_debugDraw->getDebugMode() &btIDebugDraw::DBG_DrawWireframe ))
--          {
--                  impact.d3.Vector color(1,1,0);
--                  impact.d3.Transform& tr = ob->getWorldTransform();
--                  m_dispatchInfoPtr->m_debugDraw->drawLine(tr(triangle[0]),tr(triangle[1]),color);
--                  m_dispatchInfoPtr->m_debugDraw->drawLine(tr(triangle[1]),tr(triangle[2]),color);
--                  m_dispatchInfoPtr->m_debugDraw->drawLine(tr(triangle[2]),tr(triangle[0]),color);
--          }
--  #endif
--
--          if (m_convexBody->getCollisionShape()->isConvex())
--          {
--                  impact.d3.Shape.convex.internal.polyhedral.triangle tm(triangle[0],triangle[1],triangle[2]);
--                  tm.setMargin(m_collisionMarginTriangle);
--
--                  impact.d3.Shape* tmpShape = ob->getCollisionShape();
--                  ob->internalSetTemporaryCollisionShape( &tm );
--
--                  impact.d3.collision.Algorithm* colAlgo = ci.m_dispatcher1->findAlgorithm(m_convexBody,m_triBody,m_manifoldPtr);
--
--                  if (m_resultOut->getBody0Internal() == m_triBody)
--                  {
--                          m_resultOut->setShapeIdentifiersA(partId,triangleIndex);
--                  }
--                  else
--                  {
--                          m_resultOut->setShapeIdentifiersB(partId,triangleIndex);
--                  }
--
--                  colAlgo->processCollision(m_convexBody,m_triBody,*m_dispatchInfoPtr,m_resultOut);
--                  colAlgo->~impact.d3.collision.Algorithm();
--                  ci.m_dispatcher1->freeCollisionAlgorithm(colAlgo);
--                  ob->internalSetTemporaryCollisionShape( tmpShape);
--          }
--
--
--  }
--








   procedure clearCache             (Self : in out btConvexTriangleCallback)
   is
   begin
      Self.m_dispatcher.clearManifold (Self.m_manifoldPtr);
   end clearCache;







   function getAabbMin              (Self : in btConvexTriangleCallback) return math.Vector_3
   is
   begin
      return self.m_aabbMin;
   end getAabbMin;



   function getAabbMax              (Self : in btConvexTriangleCallback) return math.Vector_3
   is
   begin
      return self.m_aabbMax;
   end getAabbMax;





   function m_triangleCount         (Self : in btConvexTriangleCallback) return Integer
   is
   begin
      return self.m_triangleCount;
   end m_triangleCount;



   function m_manifoldPtr (Self : in btConvexTriangleCallback) return access impact.d3.Manifold.item'Class
   is
   begin
      return self.m_manifoldPtr;
   end m_manifoldPtr;






   --- impact.d3.collision.Algorithm.activating.convex_concave
   --


   function to_convex_concave_Algorithm (ci           : in     AlgorithmConstructionInfo;
                                         body0, body1 : access impact.d3.Object.item'Class;
                                         isSwapped    : in     Boolean                                                ) return Item'Class
   is
      use impact.d3.collision.Algorithm.activating;

      Self : Item := (Algorithm.activating.item with -- to_activating_Algorithm (ci, body0, body1) with
                      m_isSwapped                => isSwapped,
                      m_btConvexTriangleCallback => to_btConvexTriangleCallback (ci.m_dispatcher1,  body0, body1,  isSwapped),
                      others                     => <>);
   begin
      Self.define (ci,  body0, body1);
      return Self;
   end to_convex_concave_Algorithm;





   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;




   overriding procedure processCollision (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                               resultOut    :    out impact.d3.collision.manifold_Result.item)
   is
   begin
      null;
   end processCollision;




   overriding function calculateTimeOfImpact (Self : in Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                   resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real
   is
      pragma Unreferenced (Self, body0, body1, dispatchInfo, resultOut);
   begin
      return 0.0;
   end calculateTimeOfImpact;







   overriding procedure getAllContactManifolds (Self : in out Item;   manifoldArray : out impact.d3.collision.Algorithm.btManifoldArray)
   is
   begin
      if Self.m_btConvexTriangleCallback.m_manifoldPtr /= null then
         manifoldArray.append (Self.m_btConvexTriangleCallback.m_manifoldPtr.all'Access);
      end if;
   end getAllContactManifolds;








   procedure clearCache (Self : in out Item)
   is
   begin
      null;
   end clearCache;






   --- Create Functions
   --

   overriding function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
      pragma Unreferenced (Self);
   begin
      return new impact.d3.collision.Algorithm.activating.convex_concave.item'(Item (to_convex_concave_Algorithm (ci,  body0, body1,  False)));
   end CreateCollisionAlgorithm;




   overriding function CreateCollisionAlgorithm (Self : in SwappedCreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                      body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
      pragma Unreferenced (Self);
   begin
      return new impact.d3.collision.Algorithm.activating.convex_concave.item'(Item (to_convex_concave_Algorithm (ci,  body0, body1,  True)));
   end CreateCollisionAlgorithm;






end impact.d3.collision.Algorithm.activating.convex_concave;






--
--  void        btConvexTriangleCallback::setTimeStepAndCounters(impact.d3.Scalar collisionMarginTriangle,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.collision.manifold_Result* resultOut)
--  {
--          m_dispatchInfoPtr = &dispatchInfo;
--          m_collisionMarginTriangle = collisionMarginTriangle;
--          m_resultOut = resultOut;
--
--          //recalc aabbs
--          impact.d3.Transform convexInTriangleSpace;
--          convexInTriangleSpace = m_triBody->getWorldTransform().inverse() * m_convexBody->getWorldTransform();
--          impact.d3.Shape* convexShape = static_cast<impact.d3.Shape*>(m_convexBody->getCollisionShape());
--          //CollisionShape* triangleShape = static_cast<impact.d3.Shape*>(triBody->m_collisionShape);
--          convexShape->getAabb(convexInTriangleSpace,m_aabbMin,m_aabbMax);
--          impact.d3.Scalar extraMargin = collisionMarginTriangle;
--          impact.d3.Vector extra(extraMargin,extraMargin,extraMargin);
--
--          m_aabbMax += extra;
--          m_aabbMin -= extra;
--
--  }



--  void impact.d3.collision.Algorithm.activating.convex_concave::clearCache()
--  {
--          m_btConvexTriangleCallback.clearCache();
--
--  }



--  void impact.d3.collision.Algorithm.activating.convex_concave::processCollision (impact.d3.Object* body0,impact.d3.Object* body1,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.collision.manifold_Result* resultOut)
--  {
--
--
--          impact.d3.Object* convexBody = m_isSwapped ? body1 : body0;
--          impact.d3.Object* triBody = m_isSwapped ? body0 : body1;
--
--          if (triBody->getCollisionShape()->isConcave())
--          {
--
--
--                  impact.d3.Object*        triOb = triBody;
--                  impact.d3.Shape.concave* concaveShape = static_cast<impact.d3.Shape.concave*>( triOb->getCollisionShape());
--
--                  if (convexBody->getCollisionShape()->isConvex())
--                  {
--                          impact.d3.Scalar collisionMarginTriangle = concaveShape->getMargin();
--
--                          resultOut->setPersistentManifold(m_btConvexTriangleCallback.m_manifoldPtr);
--                          m_btConvexTriangleCallback.setTimeStepAndCounters(collisionMarginTriangle,dispatchInfo,resultOut);
--
--                          //Disable persistency. previously, some older algorithm calculated all contacts in one go, so you can clear it here.
--                          //m_dispatcher->clearManifold(m_btConvexTriangleCallback.m_manifoldPtr);
--
--                          m_btConvexTriangleCallback.m_manifoldPtr->setBodies(convexBody,triBody);
--
--                          concaveShape->processAllTriangles( &m_btConvexTriangleCallback,m_btConvexTriangleCallback.getAabbMin(),m_btConvexTriangleCallback.getAabbMax());
--
--                          resultOut->refreshContactPoints();
--
--                  }
--
--          }
--
--  }





--
--  impact.d3.Scalar impact.d3.collision.Algorithm.activating.convex_concave::calculateTimeOfImpact(impact.d3.Object* body0,impact.d3.Object* body1,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.collision.manifold_Result* resultOut)
--  {
--          (void)resultOut;
--          (void)dispatchInfo;
--          impact.d3.Object* convexbody = m_isSwapped ? body1 : body0;
--          impact.d3.Object* triBody = m_isSwapped ? body0 : body1;
--
--
--          //quick approximation using raycast, todo: hook up to the continuous collision detection (one of the impact.d3.collision.convex_Raycast)
--
--          //only perform CCD above a certain threshold, this prevents blocking on the long run
--          //because object in a blocked ccd state (hitfraction<1) get their linear velocity halved each frame...
--          impact.d3.Scalar squareMot0 = (convexbody->getInterpolationWorldTransform().getOrigin() - convexbody->getWorldTransform().getOrigin()).length2();
--          if (squareMot0 < convexbody->getCcdSquareMotionThreshold())
--          {
--                  return impact.d3.Scalar(1.);
--          }
--
--          //const impact.d3.Vector& from = convexbody->m_worldTransform.getOrigin();
--          //impact.d3.Vector to = convexbody->m_interpolationWorldTransform.getOrigin();
--          //todo: only do if the motion exceeds the 'radius'
--
--          impact.d3.Transform triInv = triBody->getWorldTransform().inverse();
--          impact.d3.Transform convexFromLocal = triInv * convexbody->getWorldTransform();
--          impact.d3.Transform convexToLocal = triInv * convexbody->getInterpolationWorldTransform();
--
--          struct LocalTriangleSphereCastCallback        : public impact.d3.triangle_Callback
--          {
--                  impact.d3.Transform m_ccdSphereFromTrans;
--                  impact.d3.Transform m_ccdSphereToTrans;
--                  impact.d3.Transform        m_meshTransform;
--
--                  impact.d3.Scalar        m_ccdSphereRadius;
--                  impact.d3.Scalar        m_hitFraction;
--
--
--                  LocalTriangleSphereCastCallback(const impact.d3.Transform& from,const impact.d3.Transform& to,impact.d3.Scalar ccdSphereRadius,impact.d3.Scalar hitFraction)
--                          :m_ccdSphereFromTrans(from),
--                          m_ccdSphereToTrans(to),
--                          m_ccdSphereRadius(ccdSphereRadius),
--                          m_hitFraction(hitFraction)
--                  {
--                  }
--
--
--                  virtual void processTriangle(impact.d3.Vector* triangle, int partId, int triangleIndex)
--                  {
--                          (void)partId;
--                          (void)triangleIndex;
--                          //do a swept sphere for now
--                          impact.d3.Transform ident;
--                          ident.setIdentity();
--                          impact.d3.collision.convex_Raycast::CastResult castResult;
--                          castResult.m_fraction = m_hitFraction;
--                          impact.d3.Shape.convex.internal.sphere        pointShape(m_ccdSphereRadius);
--                          impact.d3.Shape.convex.internal.polyhedral.triangle        triShape(triangle[0],triangle[1],triangle[2]);
--                          impact.d3.collision.simplex_Solver.voronoi        simplexSolver;
--                          impact.d3.collision.convex_Raycast.subsimplex convexCaster(&pointShape,&triShape,&simplexSolver);
--                          //GjkConvexCast        convexCaster(&pointShape,convexShape,&simplexSolver);
--                          //ContinuousConvexCollision convexCaster(&pointShape,convexShape,&simplexSolver,0);
--                          //local space?
--
--                          if (convexCaster.calcTimeOfImpact(m_ccdSphereFromTrans,m_ccdSphereToTrans,
--                                  ident,ident,castResult))
--                          {
--                                  if (m_hitFraction > castResult.m_fraction)
--                                          m_hitFraction = castResult.m_fraction;
--                          }
--
--                  }
--
--          };
--
--
--
--
--
--          if (triBody->getCollisionShape()->isConcave())
--          {
--                  impact.d3.Vector rayAabbMin = convexFromLocal.getOrigin();
--                  rayAabbMin.setMin(convexToLocal.getOrigin());
--                  impact.d3.Vector rayAabbMax = convexFromLocal.getOrigin();
--                  rayAabbMax.setMax(convexToLocal.getOrigin());
--                  impact.d3.Scalar ccdRadius0 = convexbody->getCcdSweptSphereRadius();
--                  rayAabbMin -= impact.d3.Vector(ccdRadius0,ccdRadius0,ccdRadius0);
--                  rayAabbMax += impact.d3.Vector(ccdRadius0,ccdRadius0,ccdRadius0);
--
--                  impact.d3.Scalar curHitFraction = impact.d3.Scalar(1.); //is this available?
--                  LocalTriangleSphereCastCallback raycastCallback(convexFromLocal,convexToLocal,
--                          convexbody->getCcdSweptSphereRadius(),curHitFraction);
--
--                  raycastCallback.m_hitFraction = convexbody->getHitFraction();
--
--                  impact.d3.Object* concavebody = triBody;
--
--                  impact.d3.Shape.concave* triangleMesh = (impact.d3.Shape.concave*) concavebody->getCollisionShape();
--
--                  if (triangleMesh)
--                  {
--                          triangleMesh->processAllTriangles(&raycastCallback,rayAabbMin,rayAabbMax);
--                  }
--
--
--
--                  if (raycastCallback.m_hitFraction < convexbody->getHitFraction())
--                  {
--                          convexbody->setHitFraction( raycastCallback.m_hitFraction);
--                          return raycastCallback.m_hitFraction;
--                  }
--          }
--
--          return impact.d3.Scalar(1.);
--
--  }
