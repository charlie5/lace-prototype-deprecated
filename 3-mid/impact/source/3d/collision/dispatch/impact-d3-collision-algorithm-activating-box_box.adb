--  with btboxShape;
with impact.d3.collision.Detector.discrete;
with impact.d3.collision.Detector.discrete.box_box;
with impact.d3.Shape.convex.internal.polyhedral.box;



package body impact.d3.collision.Algorithm.activating.box_box
is

   USE_PERSISTENT_CONTACTS : constant Boolean := True;



   --- Forge
   --

   package body Forge
   is

      function  to_box_box_Algorithm (mf         : access impact.d3.Manifold.Item;
                                      ci         : in     AlgorithmConstructionInfo;
                                      col0, col1 : access impact.d3.Object.item'Class) return Item
      is
         use impact.d3.collision.Algorithm.activating;

         Self : Item; -- := (to_activating_Algorithm (ci,  col0, col1) with others => <>);
      begin
         Self.define (ci,  col0, col1);

         if mf = null then
            Self.m_manifoldPtr := Self.get_m_dispatcher.getNewManifold (col0, col1);
            Self.m_ownManifold := True;
         else
            Self.m_manifoldPtr := mf;
            Self.m_ownManifold := False;
         end if;


         return Self;
      end to_box_box_Algorithm;




      function  to_box_box_Algorithm (ci           : in     AlgorithmConstructionInfo) return Item
      is
         use impact.d3.collision.Algorithm.activating;

         Self : Item; --  := (to_activating_Algorithm (ci) with others => <>);
      begin
         Self.define (ci);
         return Self;
      end to_box_box_Algorithm;

   end Forge;




   overriding procedure destruct (Self : in out Item)
   is
   begin
      if Self.m_ownManifold then

         if Self.m_manifoldPtr /= null then
            Self.get_m_dispatcher.releaseManifold (Self.m_manifoldPtr);
         end if;

      end if;
   end destruct;








   --- Attributes
   --

   overriding procedure processCollision (Self : in out Item;   col0, col1   : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    :    out impact.d3.collision.manifold_Result.item)
   is
      pragma Unreferenced (dispatchInfo);
      use impact.d3.Shape.convex.internal.polyhedral,
          impact.d3.collision.Detector.discrete.box_box,  math.Vectors;

--        type BoxShape_view is access all impact.d3.Shape.convex.internal.polyhedral.box.Item'Class;

      box0 : constant impact.d3.Shape.convex.internal.polyhedral.box.view := Box.view (col0.getCollisionShape);
      box1 : constant impact.d3.Shape.convex.internal.polyhedral.box.view := Box.view (col1.getCollisionShape);

      input    : impact.d3.collision.Detector.discrete.ClosestPointInput;
      detector : impact.d3.collision.Detector.discrete.box_box.item;

   begin
      if Self.m_manifoldPtr = null then
         return;
      end if;


      --  Report a contact. internally this will be kept persistent, and contact reduction is done.
      --
      resultOut.setPersistentManifold (Self.m_manifoldPtr);

      if not USE_PERSISTENT_CONTACTS then
         Self.m_manifoldPtr.clearManifold;
      end if;


      input.m_maximumDistanceSquared := math.Real'Last;
      input.m_transformA             := col0.getWorldTransform.all;
      input.m_transformB             := col1.getWorldTransform.all;


      detector := to_box_box_Detector (box0, box1);
      detector.getClosestPoints (input, resultOut);

      if USE_PERSISTENT_CONTACTS then                   -- refreshContactPoints is only necessary when using persistent contact points. otherwise all points are newly added
         if Self.m_ownManifold then
            resultOut.refreshContactPoints;
         end if;
      end if;

   end processCollision;







   overriding function  calculateTimeOfImpact (Self : in     Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                          dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                          resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real
   is
      pragma Unreferenced (Self, body0, body1, dispatchInfo, resultOut);
   begin
      return 1.0;
   end calculateTimeOfImpact;





   overriding procedure getAllContactManifolds       (Self : in out Item;   manifoldArray : out impact.d3.collision.Algorithm.btManifoldArray)
   is
      use impact.d3.collision.Algorithm;
   begin
      if         Self.m_manifoldPtr /= null
        and then Self.m_ownManifold
      then
         manifoldArray.append (Manifold_view (Self.m_manifoldPtr));
      end if;
   end getAllContactManifolds;






   --- Create Functions
   --

   overriding function CreateCollisionAlgorithm (Self : in CreateFunc;   info         : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
      pragma Unreferenced (Self);
      new_Algorithm : constant impact.d3.collision.Algorithm.activating.box_box.view
        := new impact.d3.collision.Algorithm.activating.box_box.Item'(Forge.to_box_box_Algorithm (null, info,  body0, body1));
   begin
      return dispatcher.Algorithm_view (new_Algorithm);
   end CreateCollisionAlgorithm;

--        return new impact.d3.collision.Algorithm.activating.box_box.Item'Class' (to_box_box_Algorithm (null, info,  body0, body1));



end impact.d3.collision.Algorithm.activating.box_box;





--
--  void impact.d3.collision.Algorithm.activating.box_box::processCollision (impact.d3.Object* body0,impact.d3.Object* body1,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.collision.manifold_Result* resultOut)
--  {
--          if (!m_manifoldPtr)
--                  return;
--
--          impact.d3.Object*        col0 = body0;
--          impact.d3.Object*        col1 = body1;
--          impact.d3.Shape.convex.internal.polyhedral.box* box0 = (impact.d3.Shape.convex.internal.polyhedral.box*)col0->getCollisionShape();
--          impact.d3.Shape.convex.internal.polyhedral.box* box1 = (impact.d3.Shape.convex.internal.polyhedral.box*)col1->getCollisionShape();
--
--
--
--          /// report a contact. internally this will be kept persistent, and contact reduction is done
--          resultOut->setPersistentManifold(m_manifoldPtr);
--  #ifndef USE_PERSISTENT_CONTACTS
--          m_manifoldPtr->clearManifold();
--  #endif //USE_PERSISTENT_CONTACTS
--
--          impact.d3.collision.Detector.discrete::ClosestPointInput input;
--          input.m_maximumDistanceSquared = BT_LARGE_FLOAT;
--          input.m_transformA = body0->getWorldTransform();
--          input.m_transformB = body1->getWorldTransform();
--
--          impact.d3.collision.Detector.discrete.box_box detector(box0,box1);
--          detector.getClosestPoints(input,*resultOut,dispatchInfo.m_debugDraw);
--
--  #ifdef USE_PERSISTENT_CONTACTS
--          //  refreshContactPoints is only necessary when using persistent contact points. otherwise all points are newly added
--          if (m_ownManifold)
--          {
--                  resultOut->refreshContactPoints();
--          }
--  #endif //USE_PERSISTENT_CONTACTS
--
--  }

