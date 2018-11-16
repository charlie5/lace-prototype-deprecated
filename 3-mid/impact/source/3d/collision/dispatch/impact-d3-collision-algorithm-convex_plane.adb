with impact.d3.Shape.concave.static_plane;
with impact.d3.Vector;
with impact.d3.Shape.convex;
with impact.d3.Transform;
with impact.d3.Quaternions;



package body impact.d3.collision.Algorithm.convex_plane
is

   use Math;



   --- Forge
   --

   function  to_convex_plane_Algorithm (mf                                 : access impact.d3.Manifold.Item;
                                        ci                                 : in     AlgorithmConstructionInfo;
                                        col0, col1                         : access impact.d3.Object.item'Class;
                                        is_Swapped                         : in     Boolean;
                                        numPerturbationIterations,
                                        minimumPointsPerturbationThreshold : in Integer) return Item'Class
   is
      Self : Item; -- := (to_impact.d3.collision.Algorithm (ci,  col0, col1) with others => <>);

      convexObj,
      planeObj  : access impact.d3.Object.item'Class;
   begin
      define (Self, ci);

      Self.m_ownManifold := False;
      Self.m_manifoldPtr := mf;
      Self.m_isSwapped   := is_Swapped;

      Self.m_numPerturbationIterations          := numPerturbationIterations;
      Self.m_minimumPointsPerturbationThreshold := minimumPointsPerturbationThreshold;


      if is_Swapped then
         convexObj := col1;
         planeObj  := col0;
      else
         convexObj := col0;
         planeObj  := col1;
      end if;


      if         mf = null
        and then Self.get_m_dispatcher.needsCollision (convexObj, planeObj)
      then
         Self.m_manifoldPtr := Self.get_m_dispatcher.getNewManifold (convexObj, planeObj);
         Self.m_ownManifold := True;
      end if;


      return Self;
   end to_convex_plane_Algorithm;









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
      use impact.d3.Vector;

      type Convex_view is access all impact.d3.Shape.convex     .Item'Class;
      type Plane_view  is access all impact.d3.Shape.concave.static_plane.Item'Class;

      convexObj   : access impact.d3.Object.item;
      planeObj    : access impact.d3.Object.item;

      convexShape : Convex_view;
      planeShape  : Plane_view;

   begin
      if Self.m_manifoldPtr = null then
         return;
      end if;


      if Self.m_isSwapped then
         convexObj := col1;
         planeObj  := col0;
      else
         convexObj := col0;
         planeObj  := col1;
      end if;


      convexShape := Convex_view (convexObj.getCollisionShape);
      planeShape  := Plane_view  (planeObj .getCollisionShape);


      declare
         use impact.d3.Transform, linear_Algebra_3d;
         hasCollision  : Boolean := False;

         planeNormal   : math.Vector_3 renames planeShape.getPlaneNormal;
         planeConstant : math.Real     renames planeShape.getPlaneConstant;

         planeInConvex,
         convexInPlaneTrans : Transform_3d;

      begin
         planeInConvex      := inverse (convexObj.getWorldTransform.all) * planeObj.getWorldTransform.all;
         convexInPlaneTrans := inverse (planeObj .getWorldTransform.all) * convexObj.getWorldTransform.all;

         declare
            vtx        : constant math.Vector_3 := convexShape.localGetSupportingVertex (planeInConvex.Rotation * (-planeNormal));
            vtxInPlane : constant math.Vector_3 := convexInPlaneTrans * vtx;
            distance   : constant math.Real     := dot (planeNormal, vtxInPlane)  -  planeConstant;

            vtxInPlaneProjected : constant math.Vector_3 := vtxInPlane  -  distance * planeNormal;
            vtxInPlaneWorld     : constant math.Vector_3 := planeObj.getWorldTransform.all * vtxInPlaneProjected;
         begin
            hasCollision := distance  <  Self.m_manifoldPtr.getContactBreakingThreshold;

            resultOut.setPersistentManifold (Self.m_manifoldPtr);

            if hasCollision then        -- Report a contact. internally this will be kept persistent, and contact reduction is done.
               declare
                  normalOnSurfaceB : constant math.Vector_3 := planeObj.getWorldTransform.Rotation * planeNormal;
                  pOnB             : constant math.Vector_3 := vtxInPlaneWorld;
               begin
                  resultOut.addContactPoint (normalOnSurfaceB, pOnB, distance);
               end;
            end if;


            --  The perturbation algorithm doesn't work well with implicit surfaces such as spheres, cylinder and cones:
            --  they keep on rolling forever because of the additional off-center contact points
            --  so only enable the feature for polyhedral shapes (impact.d3.Shape.convex.internal.polyhedral.box, impact.d3.convex_HullShape etc)
            --

            if         convexShape.isPolyhedral
              and then resultOut.getPersistentManifold.getNumContacts < Self.m_minimumPointsPerturbationThreshold
            then
               declare
                  v0, v1 : math.Vector_3;
               begin
                  btPlaneSpace1 (planeNormal, v0, v1);

                  --  now perform 'm_numPerturbationIterations' collision queries with the perturbated collision objects
                  --
                  declare
                     use impact.d3.Quaternions;

                     angleLimit    : constant math.Real := 0.125 * math.Pi;
                     perturbeAngle : math.Real;
                     radius        : constant math.Real := convexShape.getAngularMotionDisc;

                     perturbeRot   : math.Quaternion;
                  begin
                     perturbeAngle := impact.d3.Manifold.gContactBreakingThreshold / radius;

                     if perturbeAngle > angleLimit then
                        perturbeAngle := angleLimit;
                     end if;

                     perturbeRot := to_Quaternion (v0 (1),  v0 (2),  v0 (3),
                                                   perturbeAngle);

                     for i in 1 .. Self.m_numPerturbationIterations
                     loop
                        declare
                           iterationAngle : constant math.Real       := math.Real (i) * (math.Pi * 2.0 / math.Real (Self.m_numPerturbationIterations));
                           rotq           : constant math.Quaternion := to_Quaternion (planeNormal, iterationAngle);
                        begin
                           Self.collideSingleContact (multiply (inverse (rotq),
                                                                multiply (perturbeRot, rotq)),
                                                      col0, col1,
                                                      dispatchInfo,
                                                      resultOut'Access);
                        end;
                     end loop;
                  end;
               end;
            end if;



            if Self.m_ownManifold then
               if Self.m_manifoldPtr.getNumContacts /= 0 then
                  resultOut.refreshContactPoints;
               end if;
            end if;

         end;
      end;

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










   procedure collideSingleContact (Self : in out Item;   perturbeRot  : in     math.Quaternion;
                                                         body0, body1 : access impact.d3.Object.item'Class;
                                                         dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                   resultOut    : access impact.d3.collision.manifold_Result.item)
   is
      pragma Unreferenced (dispatchInfo);
      convexObj : access impact.d3.Object.item'Class;
      planeObj  : access impact.d3.Object.item'Class;
   begin
      if Self.m_isSwapped then
         convexObj := body1;
         planeObj  := body0;
      else
         convexObj := body0;
         planeObj  := body1;
      end if;


      declare
         use impact.d3.Vector,               impact.d3.Transform,
             impact.d3.quaternions,
             math.algebra.linear.d3,
             math.Vectors;

         type Convex_view is access all impact.d3.Shape.convex     .Item'Class;
         type Plane_view  is access all impact.d3.Shape.concave.static_plane.Item'Class;

         convexShape : constant Convex_view := Convex_view (convexObj.getCollisionShape);
         planeShape  : constant Plane_view  := Plane_view  (planeObj .getCollisionShape);

         hasCollision : Boolean := False;

         planeNormal   : math.Vector_3 renames planeShape.getPlaneNormal;
         planeConstant : math.Real     renames planeShape.getPlaneConstant;

         convexWorldTransform : Transform_3d := convexObj.getWorldTransform.all;
         convexInPlaneTrans   : constant Transform_3d := Inverse (planeObj.getWorldTransform.all) * convexWorldTransform;

         planeInConvex : Transform_3d;

      begin
         convexWorldTransform.Rotation := convexWorldTransform.Rotation   *  to_Matrix (perturbeRot);   -- Now perturbe the convex-world transform.
         planeInConvex                 := Inverse (convexWorldTransform)  *  planeObj.getWorldTransform.all;

         declare
            vtx                 : constant math.Vector_3 := convexShape.localGetSupportingVertex (planeInConvex.Rotation * (-planeNormal));

            vtxInPlane          : constant math.Vector_3 := convexInPlaneTrans * vtx;
            distance            : Math.Real     := dot (planeNormal, vtxInPlane) - planeConstant;

            vtxInPlaneProjected : constant math.Vector_3 := vtxInPlane  -  distance * planeNormal;
            vtxInPlaneWorld     : constant math.Vector_3 := planeObj.getWorldTransform.all * vtxInPlaneProjected;
         begin

            hasCollision := distance  <  Self.m_manifoldPtr.getContactBreakingThreshold;

            resultOut.setPersistentManifold (Self.m_manifoldPtr);

            if hasCollision then     -- Report a contact. Internally this will be kept persistent, and contact reduction is done.
               declare
                  normalOnSurfaceB : constant math.Vector_3 := planeObj.getWorldTransform.Rotation * planeNormal;
                  pOnB             : constant math.Vector_3 := vtxInPlaneWorld;
               begin
                  resultOut.addContactPoint (normalOnSurfaceB, pOnB, distance);
               end;
            end if;
         end;

      end;

   end collideSingleContact;











   --- Create Functions
   --

   overriding function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
   begin

      if not Self.m_swapped then
         return new impact.d3.collision.Algorithm.convex_plane.item'
           (Item (to_convex_plane_Algorithm (null, ci,  body0, body1, False,
                                             Self.m_numPerturbationIterations, Self.m_minimumPointsPerturbationThreshold)));
      else
         return new impact.d3.collision.Algorithm.convex_plane.item'
           (Item (to_convex_plane_Algorithm (null, ci,  body0, body1, True,
                                             Self.m_numPerturbationIterations, Self.m_minimumPointsPerturbationThreshold)));
      end if;

   end CreateCollisionAlgorithm;




end impact.d3.collision.Algorithm.convex_plane;
