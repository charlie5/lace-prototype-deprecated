with impact.d3.Shape.convex.internal.sphere;
with impact.d3.Vector;
with impact.d3.Scalar;


package body impact.d3.collision.Algorithm.activating.sphere_sphere
is


   --- Forge
   --

   function  to_sphere_sphere_Algorithm (mf         : access impact.d3.Manifold.Item;
                                         ci         : in     AlgorithmConstructionInfo;
                                         col0, col1 : access impact.d3.Object.item'Class) return Item
   is
      use impact.d3.collision.Algorithm.activating;

      Self : Item; --  := (to_activating_Algorithm (ci,  col0, col1) with others => <>);
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
   end to_sphere_sphere_Algorithm;





   function  to_sphere_sphere_Algorithm (ci : in AlgorithmConstructionInfo) return Item
   is
      use impact.d3.collision.Algorithm.activating;

      Self : Item; -- := (to_activating_Algorithm (ci) with others => <>);
   begin
      Self.define (ci);
      return Self;
   end to_sphere_sphere_Algorithm;





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
      use impact.d3.Scalar, impact.d3.Vector, Math;

      type SphereShape_view is access all impact.d3.Shape.convex.internal.sphere.Item'Class;

      sphere0,
      sphere1 : access impact.d3.Shape.convex.internal.sphere.item;

      diff,
      pos1,
      normalOnSurfaceB : math.Vector_3;

      len,
      dist,
      radius0,
      radius1 : math.Real;

   begin
      if Self.m_manifoldPtr = null then
         return;
      end if;


      resultOut.setPersistentManifold (Self.m_manifoldPtr);

      sphere0 := SphereShape_view (col0.getCollisionShape);
      sphere1 := SphereShape_view (col1.getCollisionShape);

      diff    := col0.getWorldTransform.Translation - col1.getWorldTransform.Translation;
      len     := length (diff);
      radius0 := sphere0.getRadius;
      radius1 := sphere1.getRadius;


      --  #ifdef CLEAR_MANIFOLD
      --          m_manifoldPtr->clearManifold();            -- Don't do this, it disables warmstarting.
      --  #endif


      if len > radius0 + radius1 then                      -- If distance positive, don't generate a new contact.
         --  #ifndef CLEAR_MANIFOLD
         resultOut.refreshContactPoints;
         --  #endif

         return;
      end if;



      dist             := len - (radius0 + radius1);       -- Calculate distance (negative means penetration).
      normalOnSurfaceB := (1.0,  0.0,  0.0);

      if len > SIMD_EPSILON then
         normalOnSurfaceB := diff / len;
      end if;


      --  point on A (worldspace)
      --  impact.d3.Vector pos0 = col0->getWorldTransform().getOrigin() - radius0 * normalOnSurfaceB;
      --  point on B (worldspace)
      --
      pos1 := col1.getWorldTransform.Translation  +  radius1 * normalOnSurfaceB;




      resultOut.addContactPoint (normalOnSurfaceB, pos1, dist);     -- Report a contact. internally this will be kept persistent,
                                                                    -- and contact reduction is done.

      --  #ifndef CLEAR_MANIFOLD
      resultOut.refreshContactPoints;
      --  #endif //CLEAR_MANIFOLD

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

   overriding function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
      pragma Unreferenced (Self);
      new_Algorithm : constant impact.d3.collision.Algorithm.activating.sphere_sphere.view
        := new impact.d3.collision.Algorithm.activating.sphere_sphere.item'(to_sphere_sphere_Algorithm (null, ci,  body0, body1));
   begin
      return dispatcher.Algorithm_view (new_Algorithm);
   end CreateCollisionAlgorithm;




end impact.d3.collision.Algorithm.activating.sphere_sphere;
