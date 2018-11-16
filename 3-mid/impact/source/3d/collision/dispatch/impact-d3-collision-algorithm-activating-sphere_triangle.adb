with impact.d3.collision.Detector.discrete.sphere_triangle;
use impact.d3.collision.Detector.discrete.sphere_triangle;
with impact.d3.Shape.convex.internal.sphere;
with impact.d3.Shape.convex.internal.polyhedral.triangle;
with impact.d3.collision.Detector.discrete;


package body impact.d3.collision.Algorithm.activating.sphere_triangle
is


   --- Forge
   --

   package body Forge
   is

      function  to_sphere_triangle_Algorithm (mf         : access impact.d3.Manifold.Item;
                                              ci         : in     AlgorithmConstructionInfo;
                                              col0, col1 : access impact.d3.Object.item'Class;
                                              swapped    : in     Boolean                  ) return Item
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

         Self.m_swapped := swapped;

         return Self;
      end to_sphere_triangle_Algorithm;




      function to_sphere_triangle_Algorithm (ci : in AlgorithmConstructionInfo) return Item
      is
         use impact.d3.collision.Algorithm.activating;

         Self : Item; -- := (to_activating_Algorithm (ci) with others => <>);
      begin
         Self.define (ci);
         return Self;
      end to_sphere_triangle_Algorithm;

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
      use math.Vectors;

      type SphereShape_view   is access all impact.d3.Shape.convex.internal.sphere  .Item'Class;
      type TraingleShape_view is access all impact.d3.Shape.convex.internal.polyhedral.triangle.Item'Class;

      sphereObj : access impact.d3.Object.item;
      triObj    : access impact.d3.Object.item;

      sphere    : access impact.d3.Shape.convex.internal.sphere  .item;
      triangle  : access impact.d3.Shape.convex.internal.polyhedral.triangle.item;

      detector    : impact.d3.collision.Detector.discrete.sphere_triangle.item;
      input       : impact.d3.collision.Detector.discrete.ClosestPointInput;
      swapResults : Boolean;

   begin
      if Self.m_manifoldPtr = null then
         return;
      end if;


      if Self.m_swapped then
         sphereObj := col1;
         triObj    := col0;
      else
         sphereObj := col0;
         triObj    := col1;
      end if;


      sphere   := SphereShape_view   (sphereObj.getCollisionShape);
      triangle := TraingleShape_view (triObj   .getCollisionShape);


      --  Report a contact. Internally this will be kept persistent, and contact reduction is done.
      --
      resultOut.setPersistentManifold (Self.m_manifoldPtr);

      detector := to_sphere_triangle_Detector (sphere, triangle,  Self.m_manifoldPtr.getContactBreakingThreshold);


      input.m_maximumDistanceSquared := math.real'Last; -- BT_LARGE_FLOAT;                  --@todo: tighter bounds
      input.m_transformA             := sphereObj.getWorldTransform.all;
      input.m_transformB             := triObj   .getWorldTransform.all;

      swapResults                    := Self.m_swapped;

      detector.getClosestPoints (input, resultOut, swapResults);

      if Self.m_ownManifold then
         resultOut.refreshContactPoints;
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

   overriding function CreateCollisionAlgorithm (Self : in CreateFunc;   info  : in     impact.d3.Collision.Algorithm.AlgorithmConstructionInfo;
                                                              body0,
                                                              body1 : access Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
      new_Algorithm : constant Algorithm.activating.sphere_triangle.view
        := new Item'(Forge.to_sphere_triangle_Algorithm (info.m_manifold, info,  body0, body1, Self.m_swapped));
   begin
      return Dispatcher.Algorithm_view (new_Algorithm);
   end CreateCollisionAlgorithm;




end impact.d3.collision.Algorithm.activating.sphere_triangle;
