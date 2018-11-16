with impact.d3.Vector;
with impact.d3.Transform;
with impact.d3.Scalar,
     impact.d3.Matrix;
with Ada.Containers;
with impact.d3.solver_Constraint;
use impact.d3.solver_Constraint;
with interfaces.C;



package body impact.d3.constraint_Solver.sequential_impulse
is

   --  //#define COMPUTE_IMPULSE_DENOM 1
   --  //It is not necessary (redundant) to refresh contact manifolds, this refresh has been moved to the collision algorithms.


   use Interfaces;


   gNumSplitImpulseRecoveries : Integer := 0;





   --- Utility
   --



   procedure applyAnisotropicFriction (colObj            : access impact.d3.Object.item'Class;
                                       frictionDirection : in out math.Vector_3)
   is
      use impact.d3.Vector, impact.d3.Transform, impact.d3.Matrix, Math;
   begin
      if         colObj /= null
        and then colObj.hasAnisotropicFriction
      then
         declare
            --  transform to local coordinates
            loc_lateral      :          math.Vector_3 := frictionDirection * getBasis (colObj.getWorldTransform).all;
            friction_scaling : constant math.Vector_3 := colObj.getAnisotropicFriction;
         begin
            --  apply anisotropic friction
            loc_lateral := Scaled (loc_lateral, by => friction_scaling);

            --  ... and transform it back to global coordinates
            frictionDirection := getBasis (colObj.getWorldTransform).all * loc_lateral;
         end;
      end if;
   end applyAnisotropicFriction;












   --- Forge
   --

   function  to_constraint_Solver return Item
   is
      Self : Item;
   begin
      Self.m_btSeed2 := 0;

      return Self;
   end to_constraint_Solver;




   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;





   --- Operations
   --

--     procedure prepareSolve (Self : in out Item;   numBodies    : in Integer;
--                                                   numManifolds : in Integer)
--     is
--     begin
--        return ;
--     end;





   --  Sequentially applies impulses.
   --
   overriding function  solveGroup (Self : access     Item;   bodies      : access impact.d3.Object.Vector;
                                                   manifold    : access impact.d3.Manifold.Vector;
                                                   constraints : access impact.d3.Joint.Vector;
                                                   info        : in     impact.d3.contact_solver_Info.Item'Class;
                                                   dispatcher  : in     impact.d3.Dispatcher.item'Class) return math.Real
   is
      pragma Unreferenced (dispatcher);
      --  You need to provide at least some bodies.

      pragma Assert (not bodies.is_Empty);

      unused : math.Real;
      pragma Unreferenced (unused);

   begin
      unused := Self.solveGroupCacheFriendlySetup      (bodies, manifold, constraints, info);
      unused := Self.solveGroupCacheFriendlyIterations (bodies, manifold, constraints, info);
      unused := Self.solveGroupCacheFriendlyFinish     (bodies, manifold, constraints, info);

      return 0.0;
   end solveGroup;









--     procedure allSolved    (Self : in out Item;   info : in impact.d3.contact_solver_Info.Item'Class)
--     is
--     begin
--        return ;
--     end;






   overriding procedure reset (Self : in out Item)
   is
   begin
      Self.m_btSeed2 := 0;
   end reset;






   function btRand2    (Self : access Item                ) return interfaces.Unsigned_64
   is
   begin
      Self.m_btSeed2 := (1664525 * Self.m_btSeed2 + 1013904223) and 16#ffffffff#;

      return Self.m_btSeed2;
   end btRand2;








   --  //See ODE: adam's all-int straightforward(?) dRandInt (0..n-1)
   --

   function btRandInt2 (Self : access Item;   n : in Integer) return Integer
   is

      --  seems good; xor-fold and modulus
      un : constant interfaces.Unsigned_64 := interfaces.Unsigned_64 (n);
      r  : interfaces.Unsigned_64 := Self.btRand2;

   begin
      --  note: probably more aggressive than it needs to be -- might be
      --       able to get away without one or two of the innermost branches.

      if un <= 16#00010000# then
         r := r xor (Shift_Right (r, 16));

         if un <= 16#00000100# then
            r := r xor (Shift_Right (r, 8));

            if un <= 16#00000010# then
               r := r xor (Shift_Right (r, 4));

               if un <= 16#00000004# then
                  r := r xor (Shift_Right (r, 2));

                  if un <= 16#00000002# then
                     r := r xor (Shift_Right (r, 1));
                  end if;
               end if;
            end if;
         end if;
      end if;

      return Integer (r mod un);
   end btRandInt2;







   procedure setRandSeed (Self :    out Item;   seed : in interfaces.Unsigned_64)
   is
   begin
      Self.m_btSeed2 := seed;
   end setRandSeed;





   function  getRandSeed (Self : in     Item)      return interfaces.Unsigned_64
   is
   begin
      return Self.m_btSeed2;
   end getRandSeed;









   procedure setupFrictionConstraint (Self : in out Item;   solverConstraint         : in out impact.d3.solver_Constraint.item;
                                                            normalAxis               : in     math.Vector_3;
                                                            solverBodyA, solverBodyB : in     impact.d3.Object.rigid.View;
                                                            cp                       : in out impact.d3.manifold_Point.item;
                                                            rel_pos1, rel_pos2       : in     math.Vector_3;
                                                            colObj0,  colObj1        : in     impact.d3.Object.view;
                                                            relaxation               : in     math.Real;
                                                            desiredVelocity          : in     math.Real := 0.0;
                                      cfmSlip                  : in     math.Real := 0.0)
   is
      pragma Unreferenced (Self, solverBodyA, solverBodyB);
      use impact.d3.Vector, impact.d3.Matrix, Math;
      use type impact.d3.Object.rigid.view;

      body0        : constant impact.d3.Object.rigid.View := impact.d3.Object.rigid.view (colObj0);
      body1        : constant impact.d3.Object.rigid.View := impact.d3.Object.rigid.view (colObj1);

      ftorqueAxis1 : math.Vector_3;
      vec          : math.Vector_3;
      denom,
      denom0,
      denom1       : math.Real;
   begin
      solverConstraint.m_contactNormal := to_C (normalAxis);

      solverConstraint.union1.m_solverBodyA := (if body0 /= null then body0 else getFixedBody);
      solverConstraint.union2.m_solverBodyB := (if body1 /= null then body1 else getFixedBody);

      solverConstraint.m_friction             := cp.m_combinedFriction;
      solverConstraint.m_originalContactPoint := null;

      solverConstraint.m_appliedImpulse     := 0.0;
      solverConstraint.m_appliedPushImpulse := 0.0;


      ftorqueAxis1                          := cross (rel_pos1, to_Math (solverConstraint.m_contactNormal));
      solverConstraint.m_relpos1CrossNormal := to_C  (ftorqueAxis1);
      solverConstraint.m_angularComponentA  := (if body0 /= null then Scaled (body0.getInvInertiaTensorWorld * ftorqueAxis1, by => body0.getAngularFactor)
                                                                 else math.Vector_3'(0.0, 0.0, 0.0));

      ftorqueAxis1                          := cross (rel_pos2, -to_Math (solverConstraint.m_contactNormal));
      solverConstraint.m_relpos2CrossNormal := to_C (ftorqueAxis1);
      solverConstraint.m_angularComponentB  := (if body1 /= null then Scaled (body1.getInvInertiaTensorWorld * ftorqueAxis1, by => body1.getAngularFactor)
                                                                 else math.Vector_3'(0.0, 0.0, 0.0));


      denom0 := 0.0;
      denom1 := 0.0;

      if body0 /= null then
         vec    := cross (solverConstraint.m_angularComponentA, rel_pos1);
         denom0 := body0.getInvMass + dot (normalAxis, vec);
      end if;

      if body1 /= null then
         vec    := cross (-solverConstraint.m_angularComponentB, rel_pos2);
         denom1 := body1.getInvMass + dot (normalAxis, vec);
      end if;


      denom                           := relaxation / (denom0 + denom1);
      solverConstraint.m_jacDiagABInv := denom;


      declare
         vel1Dotn : math.Real :=   dot (to_Math (solverConstraint.m_contactNormal),       (if body0 /= null then body0.getLinearVelocity  else (0.0, 0.0, 0.0)))
                                 + dot (to_Math (solverConstraint.m_relpos1CrossNormal),  (if body0 /= null then body0.getAngularVelocity else (0.0, 0.0, 0.0)));

         vel2Dotn : constant math.Real :=   dot (-to_Math (solverConstraint.m_contactNormal),       (if body1 /= null then body1.getLinearVelocity  else (0.0, 0.0, 0.0)))
                                 + dot (to_Math (solverConstraint.m_relpos2CrossNormal),  (if body1 /= null then body1.getAngularVelocity else (0.0, 0.0, 0.0)));

         rel_vel  : constant math.Real := vel1Dotn + vel2Dotn;
         --                impact.d3.Scalar positionalError = 0.f;

         velocityError   : constant math.Real := desiredVelocity - rel_vel;
         velocityImpulse : constant math.Real := velocityError * solverConstraint.m_jacDiagABInv;
      begin
         solverConstraint.m_rhs        := velocityImpulse;
         solverConstraint.m_cfm        := cfmSlip;
         solverConstraint.m_lowerLimit := 0.0;
         solverConstraint.m_upperLimit := 1.0e10;
      end;
   end setupFrictionConstraint;









   function  addFrictionConstraint (Self : access Item;   normalAxis               : in     math.Vector_3;
                                                          solverBodyA, solverBodyB : in     impact.d3.Object.rigid.View;
                                                          frictionIndex            : in     Integer;
                                                          cp                       : in out impact.d3.manifold_Point.item;
                                                          rel_pos1, rel_pos2       : in     math.Vector_3;
                                                          colObj0,  colObj1        : in     impact.d3.Object.view;
                                                          relaxation               : in     math.Real;
                                                          desiredVelocity          : in     math.Real := 0.0;
                                                          cfmSlip                  : in     math.Real := 0.0) return access impact.d3.solver_Constraint.item
   is
      solverConstraint : constant impact.d3.solver_Constraint.view := new_solver_Constraint; -- Self.m_tmpSolverContactFrictionConstraintPool.expandNonInitializing;
   begin
      solverConstraint.m_frictionIndex := frictionIndex;

      Self.setupFrictionConstraint (solverConstraint.all,
                                    normalAxis,
                                    solverBodyA, solverBodyB,
                                    cp,
                                    rel_pos1, rel_pos2,
                                    colObj0, colObj1,
                                    relaxation,
                                    desiredVelocity,
                                    cfmSlip);

      Self.m_tmpSolverContactFrictionConstraintPool.append (solverConstraint);

      return solverConstraint;
   end addFrictionConstraint;






   procedure setupContactConstraint (Self : in out Item;   solverConstraint         : in out impact.d3.solver_Constraint.item;
                                                            colObj0,  colObj1        : in     impact.d3.Object.view;
                                                            cp                       : in out impact.d3.manifold_Point.item;
                                                            infoGlobal               : in     impact.d3.contact_solver_Info.item'Class;
                                                            vel                      : in out math.Vector_3;
                                                            rel_vel                  : in out math.Real;
                                                            relaxation               : in out math.Real;
                                                            rel_pos1, rel_pos2       : in out math.Vector_3)
   is
      use impact.d3.Transform, impact.d3.Vector, Math;
      use type impact.d3.Object.rigid.View;

      rb0 : constant impact.d3.Object.rigid.View := impact.d3.Object.rigid.view (colObj0);
      rb1 : constant impact.d3.Object.rigid.View := impact.d3.Object.rigid.view (colObj1);

      pos1 : constant math.Vector_3 := cp.getPositionWorldOnA;
      pos2 : constant math.Vector_3 := cp.getPositionWorldOnB;

      --                        impact.d3.Vector rel_pos1 = pos1 - colObj0->getWorldTransform().getOrigin();
      --                        impact.d3.Vector rel_pos2 = pos2 - colObj1->getWorldTransform().getOrigin();

      torqueAxis0,
      torqueAxis1 : math.Vector_3;

   begin
      rel_pos1 := pos1 - getOrigin (colObj0.getWorldTransform).all;
      rel_pos2 := pos2 - getOrigin (colObj1.getWorldTransform).all;

      relaxation := 1.0;

      torqueAxis0                          := cross (rel_pos1, cp.m_normalWorldOnB);
      solverConstraint.m_angularComponentA := (if rb0 /= null then Scaled (rb0.getInvInertiaTensorWorld * torqueAxis0, by => rb0.getAngularFactor)
                                                              else math.Vector_3'(0.0, 0.0, 0.0));

      torqueAxis1                          := cross (rel_pos2, cp.m_normalWorldOnB);
      solverConstraint.m_angularComponentB := (if rb1 /= null then Scaled (rb1.getInvInertiaTensorWorld * (-torqueAxis1), by => rb1.getAngularFactor)
                                                              else math.Vector_3'(0.0, 0.0, 0.0));

      declare
         vec    : math.Vector_3;
         denom0 : math.Real := 0.0;
         denom1 : math.Real := 0.0;
         denom  : math.Real;
      begin
         if rb0 /= null then
            vec    := cross (solverConstraint.m_angularComponentA, rel_pos1);
            denom0 := rb0.getInvMass + dot (cp.m_normalWorldOnB, vec);
         end if;

         if rb1 /= null then
            vec    := cross (-solverConstraint.m_angularComponentB, rel_pos2);
            denom1 := rb1.getInvMass + dot (cp.m_normalWorldOnB, vec);
         end if;

         denom                           := relaxation / (denom0 + denom1);
         solverConstraint.m_jacDiagABInv := denom;
      end;


      solverConstraint.m_contactNormal      := to_C (cp.m_normalWorldOnB);
      solverConstraint.m_relpos1CrossNormal := to_C (cross (rel_pos1,  cp.m_normalWorldOnB));
      solverConstraint.m_relpos2CrossNormal := to_C (cross (rel_pos2, -cp.m_normalWorldOnB));


      declare
         use impact.d3.Matrix;
         use type Flags; --, math.Vector_3;

         vel1 : constant math.Vector_3 := (if rb0 /= null then rb0.getVelocityInLocalPoint (rel_pos1) else math.Vector_3'(0.0, 0.0, 0.0));
         vel2 : constant math.Vector_3 := (if rb1 /= null then rb1.getVelocityInLocalPoint (rel_pos2) else math.Vector_3'(0.0, 0.0, 0.0));

         penetration,
         restitution : math.Real;
      begin
         vel     := vel1 - vel2;
         rel_vel := dot (cp.m_normalWorldOnB, vel);

         penetration                 := cp.getDistance + infoGlobal.m_linearSlop;
         solverConstraint.m_friction := cp.m_combinedFriction;

         restitution := 0.0;

         if cp.m_lifeTime > infoGlobal.m_restingContactRestitutionThreshold then
            restitution := 0.0;
         else
            restitution := Self.restitutionCurve (rel_vel, cp.m_combinedRestitution);

            if restitution <= 0.0 then
               restitution := 0.0;
            end if;
         end if;


         --  warm starting (or zero if disabled)
         if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_WARMSTARTING) /= 0 then
            solverConstraint.m_appliedImpulse := cp.m_appliedImpulse * infoGlobal.m_warmstartingFactor;

            if rb0 /= null then   -- tbd: check these Scaled operations are correct ...
               rb0.internalApplyImpulse (Scaled (to_Math (solverConstraint.m_contactNormal) * rb0.getInvMass, by => rb0.getLinearFactor),
                                          solverConstraint.m_angularComponentA,
                                          solverConstraint.m_appliedImpulse);
            end if;

            if rb1 /= null then
               rb1.internalApplyImpulse (Scaled (to_Math (solverConstraint.m_contactNormal) * rb1.getInvMass, by => rb1.getLinearFactor),
                                         -(solverConstraint.m_angularComponentB),
                                         -solverConstraint.m_appliedImpulse);
            end if;

         else
            solverConstraint.m_appliedImpulse := 0.0;
         end if;


         solverConstraint.m_appliedPushImpulse := 0.0;

         declare
            vel1Dotn : math.Real :=  dot (to_Math (solverConstraint.m_contactNormal),
                                          (if rb0 /= null then rb0.getLinearVelocity  else math.Vector_3'(0.0, 0.0, 0.0)))
                                   + dot (to_Math (solverConstraint.m_relpos1CrossNormal),
                                          (if rb0 /= null then rb0.getAngularVelocity else math.Vector_3'(0.0, 0.0, 0.0)));

            vel2Dotn : constant math.Real :=  dot (-(to_Math (solverConstraint.m_contactNormal)),
                                          (if rb1 /= null then rb1.getLinearVelocity  else math.Vector_3'(0.0, 0.0, 0.0)))
                                   + dot (to_Math (solverConstraint.m_relpos2CrossNormal),
                                          (if rb1 /= null then rb1.getAngularVelocity else math.Vector_3'(0.0, 0.0, 0.0)));

            rel_vel : constant math.Real := vel1Dotn + vel2Dotn;

            positionalError : math.Real := 0.0;
            velocityError   : math.Real := restitution - rel_vel;  -- * damping;

            penetrationImpulse,
            velocityImpulse    : math.Real;

         begin

            if penetration > 0.0 then
               velocityError   := velocityError  -  penetration / infoGlobal.m_timeStep;
               positionalError := 0.0;
            else
               positionalError := -penetration * infoGlobal.m_erp/infoGlobal.m_timeStep;
            end if;

            penetrationImpulse := positionalError * solverConstraint.m_jacDiagABInv;
            velocityImpulse    := velocityError   * solverConstraint.m_jacDiagABInv;

            if        not infoGlobal.m_splitImpulse
              or else penetration > infoGlobal.m_splitImpulsePenetrationThreshold
            then

               --  combine position and velocity into rhs
               solverConstraint.m_rhs            := penetrationImpulse + velocityImpulse;
               solverConstraint.m_rhsPenetration := 0.0;
            else

               --  split position and velocity into rhs and m_rhsPenetration
               solverConstraint.m_rhs            := velocityImpulse;
               solverConstraint.m_rhsPenetration := penetrationImpulse;
            end if;

            solverConstraint.m_cfm        := 0.0;
            solverConstraint.m_lowerLimit := 0.0;
            solverConstraint.m_upperLimit := 1.0e10;
         end;
      end;
   end setupContactConstraint;









   procedure setFrictionConstraintImpulse (Self : in out Item;   solverConstraint : in out impact.d3.solver_Constraint.item;
                                                                 rb0, rb1         : in     impact.d3.Object.rigid.View;
                                                                 cp               : in out impact.d3.manifold_Point.item;
                                                                 infoGlobal       : in     impact.d3.contact_solver_Info.item'Class)
   is
      use impact.d3.solver_Constraint.Vectors, impact.d3.Vector, Math, math.Vectors;
      use type Flags,  impact.d3.Object.rigid.view;

      frictionConstraint1 : access impact.d3.solver_Constraint.Item;
      frictionConstraint2 : access impact.d3.solver_Constraint.Item;

   begin
      if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_FRICTION_WARMSTARTING) /= 0 then

         frictionConstraint1 := Self.m_tmpSolverContactFrictionConstraintPool.Element (solverConstraint.m_frictionIndex);


         if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_WARMSTARTING) /= 0 then

            frictionConstraint1.m_appliedImpulse := cp.m_appliedImpulseLateral1 * infoGlobal.m_warmstartingFactor;

            if rb0 /= null then   -- tbd: check Scaled is right.
               rb0.internalApplyImpulse (Scaled (to_Math (frictionConstraint1.m_contactNormal) * rb0.getInvMass, by => rb0.getLinearFactor),
                                          frictionConstraint1.m_angularComponentA,
                                          frictionConstraint1.m_appliedImpulse);
            end if;

            if rb1 /= null then   -- tbd: check Scaled is right.
               rb1.internalApplyImpulse (Scaled (to_Math (frictionConstraint1.m_contactNormal) * rb1.getInvMass, by => rb1.getLinearFactor),
                                         -frictionConstraint1.m_angularComponentB,
                                         -frictionConstraint1.m_appliedImpulse);
            end if;

         else
            frictionConstraint1.m_appliedImpulse := 0.0;
         end if;


         if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_2_FRICTION_DIRECTIONS) /= 0 then

            frictionConstraint2 := Self.m_tmpSolverContactFrictionConstraintPool.Element (solverConstraint.m_frictionIndex + 0);  -- or +1 ?

            if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_WARMSTARTING) /= 0 then
               frictionConstraint2.m_appliedImpulse := cp.m_appliedImpulseLateral2 * infoGlobal.m_warmstartingFactor;

               if rb0 /= null then
                  rb0.internalApplyImpulse (to_Math (frictionConstraint2.m_contactNormal) * rb0.getInvMass,
                                             frictionConstraint2.m_angularComponentA,
                                             frictionConstraint2.m_appliedImpulse);
               end if;

               if rb1 /= null then
                  rb1.internalApplyImpulse (to_Math (frictionConstraint2.m_contactNormal) * rb1.getInvMass,
                                            -frictionConstraint2.m_angularComponentB,
                                            -frictionConstraint2.m_appliedImpulse);
               end if;

            else
               frictionConstraint2.m_appliedImpulse := 0.0;
            end if;
         end if;


      else
         frictionConstraint1                  := Self.m_tmpSolverContactFrictionConstraintPool.Element (solverConstraint.m_frictionIndex + 0);  -- or +1
         frictionConstraint1.m_appliedImpulse := 0.0;

         if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_2_FRICTION_DIRECTIONS) /= 0 then
            frictionConstraint2                  := Self.m_tmpSolverContactFrictionConstraintPool.Element (solverConstraint.m_frictionIndex + 1);  -- or +2
            frictionConstraint2.m_appliedImpulse := 0.0;
         end if;
      end if;
   end setFrictionConstraintImpulse;






   function  restitutionCurve             (Self : access Item;   rel_vel     : in     math.Real;
                                                                 restitution : in     math.Real) return math.Real
   is
      pragma Unreferenced (Self);
      rest : constant math.Real := restitution * (-rel_vel);
   begin
      return rest;
   end restitutionCurve;





   procedure convertContact               (Self : access Item;   manifold   : in     impact.d3.Manifold.view;
                                                                 infoGlobal : in     impact.d3.contact_solver_Info.item'Class)
   is
      use type impact.d3.Object.rigid.view;

      colObj0     : constant impact.d3.Object.View := impact.d3.Object.View (manifold.getBody0);
      colObj1     : constant impact.d3.Object.View := impact.d3.Object.View (manifold.getBody1);

      solverBodyA : constant impact.d3.Object.rigid.view := impact.d3.Object.rigid.View (colObj0);
      solverBodyB : constant impact.d3.Object.rigid.view := impact.d3.Object.rigid.View (colObj1);

      cp          : access impact.d3.manifold_Point.item;

   begin
      --  avoid collision response between two static objects
      --
      if         (solverBodyA = null or else solverBodyA.getInvMass = 0.0)
        and then (solverBodyB = null or else solverBodyB.getInvMass = 0.0)
      then
         return;
      end if;


      for j in 1 .. manifold.getNumContacts
      loop
         cp := manifold.getContactPoint (j);

         if cp.getDistance <= manifold.getContactProcessingThreshold then
            declare
               use impact.d3.Vector, Math, math.Functions;
               use type Flags;

               rel_pos1,
               rel_pos2 : math.Vector_3;

               relaxation,
               rel_vel    : math.Real;

               vel        : math.Vector_3;

               frictionIndex : constant Integer := Integer (Self.m_tmpSolverContactConstraintPool.Length) + 1;

               solverConstraint : constant impact.d3.solver_Constraint.view := new_solver_Constraint; -- m_tmpSolverContactConstraintPool.expandNonInitializing();

               rb0 : constant impact.d3.Object.rigid.View := impact.d3.Object.rigid.view (colObj0);
               rb1 : constant impact.d3.Object.rigid.View := impact.d3.Object.rigid.view (colObj1);

               lat_rel_vel : math.Real;

               unused : access impact.d3.solver_Constraint.item;
               pragma Unreferenced (unused);

            begin
               Self.m_tmpSolverContactConstraintPool.append (solverConstraint);

               solverConstraint.union1.m_solverBodyA := (if rb0 /= null then rb0 else getFixedBody);
               solverConstraint.union2.m_solverBodyB := (if rb1 /= null then rb1 else getFixedBody);

               solverConstraint.m_originalContactPoint := cp.all'Access;

               Self.setupContactConstraint (solverConstraint.all,
                                            colObj0,  colObj1,
                                            cp.all,
                                            infoGlobal,
                                            vel,      rel_vel,
                                            relaxation,
                                            rel_pos1, rel_pos2);

               --                        const impact.d3.Vector& pos1 = cp.getPositionWorldOnA();
               --                        const impact.d3.Vector& pos2 = cp.getPositionWorldOnB();

                        --- setup the friction constraints

               solverConstraint.m_frictionIndex := Integer (Self.m_tmpSolverContactFrictionConstraintPool.Length) + 1;

               if        (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_ENABLE_FRICTION_DIRECTION_CACHING) = 0
                 or else not cp.m_lateralFrictionInitialized
               then
                  cp.m_lateralFrictionDir1 := vel - cp.m_normalWorldOnB * rel_vel;
                  lat_rel_vel              := length2 (cp.m_lateralFrictionDir1);

                  if         (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_DISABLE_VELOCITY_DEPENDENT_FRICTION_DIRECTION) = 0
                    and then lat_rel_vel > impact.d3.Scalar.SIMD_EPSILON
                  then
                     cp.m_lateralFrictionDir1 := cp.m_lateralFrictionDir1 / sqRt (lat_rel_vel);

                     if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_2_FRICTION_DIRECTIONS) = 0 then
                        cp.m_lateralFrictionDir2 := cross (cp.m_lateralFrictionDir1, cp.m_normalWorldOnB);
                        normalize (cp.m_lateralFrictionDir2);   -- ??

                        applyAnisotropicFriction (colObj0, cp.m_lateralFrictionDir2);
                        applyAnisotropicFriction (colObj1, cp.m_lateralFrictionDir2);

                        unused := Self.addFrictionConstraint (cp.m_lateralFrictionDir2,
                                                              solverBodyA, solverBodyB,
                                                              frictionIndex,
                                                              cp.all,
                                                              rel_pos1,    rel_pos2,
                                                              colObj0,     colObj1,
                                                              relaxation);
                     end if;

                     applyAnisotropicFriction (colObj0, cp.m_lateralFrictionDir1);
                     applyAnisotropicFriction (colObj1, cp.m_lateralFrictionDir1);

                     unused := Self.addFrictionConstraint (cp.m_lateralFrictionDir1,
                                                           solverBodyA, solverBodyB,
                                                           frictionIndex,
                                                           cp.all,
                                                           rel_pos1,    rel_pos2,
                                                           colObj0,     colObj1,
                                                           relaxation);

                     cp.m_lateralFrictionInitialized := True;

                  else   -- re-calculate friction direction every frame, todo: check if this is really needed
                     btPlaneSpace1 (cp.m_normalWorldOnB, cp.m_lateralFrictionDir1,
                                                         cp.m_lateralFrictionDir2);

                     if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_2_FRICTION_DIRECTIONS) /= 0 then
                        applyAnisotropicFriction (colObj0, cp.m_lateralFrictionDir2);
                        applyAnisotropicFriction (colObj1, cp.m_lateralFrictionDir2);

                        unused := Self.addFrictionConstraint (cp.m_lateralFrictionDir2,
                                                              solverBodyA, solverBodyB,
                                                              frictionIndex,
                                                              cp.all,
                                                              rel_pos1,    rel_pos2,
                                                              colObj0,     colObj1,
                                                              relaxation);
                     end if;

                     applyAnisotropicFriction (colObj0, cp.m_lateralFrictionDir1);
                     applyAnisotropicFriction (colObj1, cp.m_lateralFrictionDir1);

                     unused := Self.addFrictionConstraint (cp.m_lateralFrictionDir1,
                                                           solverBodyA, solverBodyB,
                                                           frictionIndex,
                                                           cp.all,
                                                           rel_pos1,    rel_pos2,
                                                           colObj0,     colObj1,
                                                           relaxation);

                     cp.m_lateralFrictionInitialized := True;
                  end if;

               else
                  unused := Self.addFrictionConstraint (cp.m_lateralFrictionDir1,
                                                        solverBodyA,         solverBodyB,
                                                        frictionIndex,
                                                        cp.all,
                                                        rel_pos1,            rel_pos2,
                                                        colObj0,             colObj1,
                                                        relaxation,
                                                        cp.m_contactMotion1, cp.m_contactCFM1);

                  if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_2_FRICTION_DIRECTIONS) /= 0 then
                     unused := Self.addFrictionConstraint (cp.m_lateralFrictionDir2,
                                                           solverBodyA,         solverBodyB,
                                                           frictionIndex,
                                                           cp.all,
                                                           rel_pos1,            rel_pos2,
                                                           colObj0,             colObj1,
                                                           relaxation,
                                                           cp.m_contactMotion2, cp.m_contactCFM2);
                  end if;
               end if;


               Self.setFrictionConstraintImpulse (solverConstraint.all, rb0, rb1, cp.all, infoGlobal);
            end;
         end if;

      end loop;

   end convertContact;











   procedure resolveSplitPenetrationImpulseCacheFriendly (Self : in out Item;   body1, body2      : in     impact.d3.Object.rigid.View;
                                                                                contactConstraint : in out impact.d3.solver_Constraint.item)
   is
      pragma Unreferenced (Self);
      use impact.d3.Vector,
          Math, math.Vectors;

      c : impact.d3.solver_Constraint.item renames contactConstraint;

   begin
      if c.m_rhsPenetration /= 0.0 then
         gNumSplitImpulseRecoveries := gNumSplitImpulseRecoveries + 1;

         declare
            deltaImpulse  :          math.Real := c.m_rhsPenetration  -  c.m_appliedPushImpulse * c.m_cfm;

            deltaVel1Dotn : constant math.Real :=   dot (to_Math (c.m_contactNormal),      body1.internalGetPushVelocity.all)
                                                  + dot (to_Math (c.m_relpos1CrossNormal), body1.internalGetTurnVelocity.all);
            deltaVel2Dotn : constant math.Real :=   dot (-to_Math (c.m_contactNormal),      body2.internalGetPushVelocity.all)
                                                  + dot (to_Math (c.m_relpos2CrossNormal), body2.internalGetTurnVelocity.all);

            sum           :          math.Real;

         begin
            deltaImpulse := deltaImpulse  -  deltaVel1Dotn * c.m_jacDiagABInv;
            deltaImpulse := deltaImpulse  -  deltaVel2Dotn * c.m_jacDiagABInv;

            sum := c.m_appliedPushImpulse + deltaImpulse;

            if sum < c.m_lowerLimit then
               deltaImpulse           := c.m_lowerLimit - c.m_appliedPushImpulse;
               c.m_appliedPushImpulse := c.m_lowerLimit;
            else
               c.m_appliedPushImpulse := sum;
            end if;

            body1.internalApplyPushImpulse (Scaled (to_Math (c.m_contactNormal), by => body1.internalGetInvMass),  c.m_angularComponentA,  deltaImpulse);
            body2.internalApplyPushImpulse (Scaled (-to_Math (c.m_contactNormal), by => body2.internalGetInvMass),  c.m_angularComponentB,  deltaImpulse);
         end;
      end if;
   end resolveSplitPenetrationImpulseCacheFriendly;






   function  getOrInitSolverBody (Self : in Item;   the_body : access impact.d3.Object.item'Class) return Integer
   is
      pragma Unreferenced (Self, the_body);
   begin
      return 0;
   end getOrInitSolverBody;







   --  Project Gauss Seidel or the equivalent Sequential Impulse
   --

   procedure resolveSingleConstraintRowGeneric    (Self : in out Item;   body1, body2      : in     impact.d3.Object.rigid.View;
                                                                         contactConstraint : in out impact.d3.solver_Constraint.item)
   is
      pragma Unreferenced (Self);
      use impact.d3.Vector,
          Math, math.Vectors;

      c             : impact.d3.solver_Constraint.item renames contactConstraint;

      deltaImpulse  :          math.Real := c.m_rhs - math.Real (c.m_appliedImpulse) * c.m_cfm;

      deltaVel1Dotn : constant math.Real :=    dot (to_Math (c.m_contactNormal),       body1.internalGetDeltaLinearVelocity.all)
                                            +  dot (to_Math (c.m_relpos1CrossNormal), body1.internalGetDeltaAngularVelocity.all);
      deltaVel2Dotn : constant math.Real :=   -dot (to_Math (c.m_contactNormal),       body2.internalGetDeltaLinearVelocity.all)
                                            +  dot (to_Math (c.m_relpos2CrossNormal),  body2.internalGetDeltaAngularVelocity.all);

      sum           : math.Real;

   begin
      --  const impact.d3.Scalar delta_rel_vel        =        deltaVel1Dotn-deltaVel2Dotn;

      deltaImpulse := deltaImpulse  -  deltaVel1Dotn * c.m_jacDiagABInv;
      deltaImpulse := deltaImpulse  -  deltaVel2Dotn * c.m_jacDiagABInv;

      sum := math.Real (c.m_appliedImpulse) + deltaImpulse;

      if sum < c.m_lowerLimit then
         deltaImpulse       := c.m_lowerLimit - c.m_appliedImpulse;
         c.m_appliedImpulse := c.m_lowerLimit;

      elsif sum > c.m_upperLimit then
         deltaImpulse       := c.m_upperLimit - c.m_appliedImpulse;
         c.m_appliedImpulse := c.m_upperLimit;

      else
         c.m_appliedImpulse := sum;
      end if;


      --  tbd: check that 'Scaled' is the correct operation below !
      --

      body1.internalApplyImpulse (Scaled (to_Math (c.m_contactNormal), by => body1.internalGetInvMass),  c.m_angularComponentA,  deltaImpulse);
      body2.internalApplyImpulse (Scaled (-to_Math (c.m_contactNormal), by => body2.internalGetInvMass),  c.m_angularComponentB,  deltaImpulse);

      --
      --                  body1.internalApplyImpulse(c.m_contactNormal*body1.internalGetInvMass(),c.m_angularComponentA,deltaImpulse);
      --                  body2.internalApplyImpulse(-c.m_contactNormal*body2.internalGetInvMass(),c.m_angularComponentB,deltaImpulse);
   end resolveSingleConstraintRowGeneric;







   --  Project Gauss Seidel or the equivalent Sequential Impulse
   --

   procedure resolveSingleConstraintRowLowerLimit (Self : in out Item;   body1, body2      : in     impact.d3.Object.rigid.View;
                                                                         contactConstraint : in out impact.d3.solver_Constraint.item)
   is
      pragma Unreferenced (Self);
      use impact.d3.Vector, math.Vectors;

      function "-" (the_Vector : in math.Vector_3) return math.Vector_3   -- For performance.
      is
      begin
         return (-the_Vector (1), -the_Vector (2), -the_Vector (3));
      end;



      c             : impact.d3.solver_Constraint.Item renames contactConstraint;

      deltaImpulse  :          math.Real := c.m_rhs - math.Real (c.m_appliedImpulse) * c.m_cfm;

      deltaVel1Dotn : constant math.Real :=   dot (to_Math (c.m_contactNormal),      body1.internalGetDeltaLinearVelocity.all)
                                            + dot (to_Math (c.m_relpos1CrossNormal), body1.internalGetDeltaAngularVelocity.all);

      deltaVel2Dotn : constant math.Real :=   dot (-to_Math (c.m_contactNormal),       body2.internalGetDeltaLinearVelocity.all)
                                            + dot (to_Math (c.m_relpos2CrossNormal),  body2.internalGetDeltaAngularVelocity.all);
      sum           : math.Real;

   begin
      deltaImpulse := deltaImpulse  -  deltaVel1Dotn * c.m_jacDiagABInv;
      deltaImpulse := deltaImpulse  -  deltaVel2Dotn * c.m_jacDiagABInv;

      sum := c.m_appliedImpulse + deltaImpulse;

      if sum < c.m_lowerLimit then
         deltaImpulse       := c.m_lowerLimit - c.m_appliedImpulse;
         c.m_appliedImpulse := c.m_lowerLimit;
      else
         c.m_appliedImpulse := sum;
      end if;

      --  tbd: check Scaled is correct operation.
--        body1.internalApplyImpulse (Scaled (to_Math ( c.m_contactNormal), by => body1.internalGetInvMass),  c.m_angularComponentA,  deltaImpulse);
--        body2.internalApplyImpulse (Scaled (-to_Math (c.m_contactNormal), by => body2.internalGetInvMass),  c.m_angularComponentB,  deltaImpulse);
      body1.internalApplyImpulse (to_Math (c.m_contactNormal),  c.m_angularComponentA,  deltaImpulse);
      body2.internalApplyImpulse (-to_Math (c.m_contactNormal),  c.m_angularComponentB,  deltaImpulse);
   end resolveSingleConstraintRowLowerLimit;










   s_fixed : impact.d3.Object.rigid.view;

   function  getFixedBody return impact.d3.Object.rigid.view
   is
      use type impact.d3.Object.rigid.view;
   begin
      if s_fixed = null then
         s_fixed := new impact.d3.Object.rigid.Item'(Object.rigid.Forge.to_rigid_Object (0.0, null, null));
      end if;

      s_fixed.setMassProps (0.0,  (0.0, 0.0, 0.0));

      return s_fixed;
   end getFixedBody;







   procedure solveGroupCacheFriendlySplitImpulseIterations (Self : in out Item;   bodies      : access impact.d3.Object   .Vector;
                                                                                  manifoldPtr : access impact.d3.Manifold.Vector;
                                                                                  constraints : access impact.d3.Joint.Vector;
                                                                                  infoGlobal  : in     impact.d3.contact_solver_Info.item'Class)
   is
      pragma Unreferenced (bodies, manifoldPtr, constraints);
--        iteration,
      numPoolConstraints : Integer;
      solveManifold : impact.d3.solver_Constraint.view;
   begin
      if infoGlobal.m_splitImpulse then

--                  if infoGlobal.m_solverMode and SOLVER_SIMD /= 0 then
--
--                          for iteration in 1 .. infoGlobal.m_numIterations
--                          loop
--                                          numPoolConstraints := Integer (Self.m_tmpSolverContactConstraintPool.Lemgth);
--
--                                          for j in 1 .. numPoolConstraints
--                                          loop
--                                                  const impact.d3.solver_Constraint& solveManifold = m_tmpSolverContactConstraintPool[m_orderTmpConstraintPool[j]];
--                                                  resolveSplitPenetrationSIMD(*solveManifold.m_solverBodyA,*solveManifold.m_solverBodyB,solveManifold);
--                                          end loop;
--                          end loop;
--
--                  else

         for iteration in 1 .. infoGlobal.m_numIterations
         loop
            numPoolConstraints := Integer (Self.m_tmpSolverContactConstraintPool.Length);

            for j in 1 .. numPoolConstraints
            loop
               solveManifold := Self.m_tmpSolverContactConstraintPool.Element (Self.m_orderTmpConstraintPool.Element (j));

               Self.resolveSplitPenetrationImpulseCacheFriendly (solveManifold.union1.m_solverBodyA.all'Access,
                                                                 solveManifold.union2.m_solverBodyB.all'Access,
                                                                 solveManifold.all);
            end loop;
         end loop;

--                  end if;

      end if;
   end solveGroupCacheFriendlySplitImpulseIterations;












   function  solveGroupCacheFriendlyFinish (Self : in out Item;   bodies      : access impact.d3.Object   .Vector;
                                                                  manifoldPtr : access impact.d3.Manifold.Vector;
                                                                  constraints : access impact.d3.Joint   .Vector;
                                                                  infoGlobal  : in     impact.d3.contact_solver_Info .Item'Class) return math.Real
   is
      pragma Unreferenced (manifoldPtr, constraints);
      use type Flags, impact.d3.manifold_Point.view, impact.d3.Object.rigid.view;

      numPoolConstraints : Integer := Integer (Self.m_tmpSolverContactConstraintPool.Length);
      numBodies          : constant Integer := Integer (bodies.Length);

      solveManifold      : impact.d3.solver_Constraint.view;
      pt                 : impact.d3.manifold_Point.view;

      solverConstr : impact.d3.solver_Constraint.view;
      constr       : impact.d3.Joint.view;

      the_Body     : impact.d3.Object.rigid.view;

   begin
      for j in 1 .. numPoolConstraints
        loop
         solveManifold       := Self.m_tmpSolverContactConstraintPool.Element (j);
         pt                  := impact.d3.manifold_Point.view (solveManifold.m_originalContactPoint);         pragma Assert (pt /= null);
         pt.m_appliedImpulse := solveManifold.m_appliedImpulse;

         if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_USE_FRICTION_WARMSTARTING) /= 0 then
            pt.m_appliedImpulseLateral1 := Self.m_tmpSolverContactFrictionConstraintPool.Element (solveManifold.m_frictionIndex).m_appliedImpulse;
            pt.m_appliedImpulseLateral2 := Self.m_tmpSolverContactFrictionConstraintPool.Element (solveManifold.m_frictionIndex + 1).m_appliedImpulse;
         end if;

         --  do a callback here?
      end loop;



      numPoolConstraints := Integer (Self.m_tmpSolverNonContactConstraintPool.Length);

      for j in 1 .. numPoolConstraints
      loop
         solverConstr := Self.m_tmpSolverNonContactConstraintPool.Element (j);
         constr       := impact.d3.Joint.view (solverConstr.m_originalContactPoint);

         constr.internalSetAppliedImpulse (solverConstr.m_appliedImpulse);

         if abs (solverConstr.m_appliedImpulse) >= constr.getBreakingImpulseThreshold then
            constr.setEnabled (False);
         end if;
      end loop;


      if infoGlobal.m_splitImpulse then

         for i in 1 .. numBodies
         loop
            the_Body := impact.d3.Object.rigid.view (bodies.Element (i));

            if the_Body /= null then
               the_Body.internalWritebackVelocity (infoGlobal.m_timeStep);
            end if;
         end loop;

      else

         for i in 1 .. numBodies
         loop
            the_Body := impact.d3.Object.rigid.view (bodies.Element (i));

            if the_Body /= null then
               the_Body.internalWritebackVelocity;
            end if;
         end loop;

      end if;


      declare
         procedure clear (the_Pool : in out impact.d3.solver_Constraint.btConstraintArray)
         is
            the_Constraint : solver_Constraint.View;
         begin
            for Each in 1 .. Integer (the_Pool.Length)
            loop
               the_Constraint := the_Pool.Element (Each);
               free (the_Constraint);
            end loop;


            the_Pool.reserve_Capacity (0);
            the_Pool.clear;
         end clear;
      begin
         clear (Self.m_tmpSolverContactConstraintPool);
         clear (Self.m_tmpSolverNonContactConstraintPool);
         clear (Self.m_tmpSolverContactFrictionConstraintPool);
      end;


      return 0.0;
   end solveGroupCacheFriendlyFinish;














   function  solveSingleIteration          (Self : in out Item;   iteration   : in     Integer;
                                                                  bodies      : access impact.d3.Object   .Vector;
                                                                  manifoldPtr : access impact.d3.Manifold.Vector;
                                                                  constraints : access impact.d3.Joint   .Vector;
                                                                  infoGlobal  : in     impact.d3.contact_solver_Info .Item'Class) return math.Real
   is
      pragma Unreferenced (bodies, manifoldPtr);

      numConstraintPool : constant Integer := Integer (Self.m_tmpSolverContactConstraintPool.Length);
      numFrictionPool   : constant Integer := Integer (Self.m_tmpSolverContactFrictionConstraintPool.Length);

   begin
      if (infoGlobal.m_solverMode and impact.d3.contact_solver_Info.SOLVER_RANDMIZE_ORDER) /= 0
      then
         if (Interfaces.Unsigned_64 (iteration) and 7) = 0 then
            declare
               tmp   : Integer;
               swapi : Integer;
            begin
               for j in 1 .. numConstraintPool
               loop
                  tmp   := Self.m_orderTmpConstraintPool.Element (j);
                  swapi := Self.btRandInt2 (j) + 1;

                  Self.m_orderTmpConstraintPool.replace_Element (j, Self.m_orderTmpConstraintPool.Element (swapi));
                  Self.m_orderTmpConstraintPool.replace_Element (swapi, tmp);
               end loop;

               for j in 1 .. numFrictionPool
               loop
                  tmp   := Self.m_orderFrictionConstraintPool.Element (j);
                  swapi := Self.btRandInt2 (j) + 1;

                  Self.m_orderFrictionConstraintPool.Replace_Element (j, Self.m_orderFrictionConstraintPool.Element (swapi));
                  Self.m_orderFrictionConstraintPool.replace_Element (swapi, tmp);
               end loop;
            end;
         end if;
      end if;

      --  tbd: is below needed ?
      --
      --          if (infoGlobal.m_solverMode & SOLVER_SIMD)
      --          {
      --                  -- solve all joint constraints, using SIMD, if available
      --                  for j in 1 .. Integer (Self.m_tmpSolverNonContactConstraintPool.Length)
      --                  loop
      --                          impact.d3.solver_Constraint& constraint = Self.m_tmpSolverNonContactConstraintPool[j];
      --                          resolveSingleConstraintRowGenericSIMD(*constraint.m_solverBodyA,*constraint.m_solverBodyB,constraint);
      --                  end loop;
      --
      --                  for j in 1 .. numConstraints
      --                  loop
      --                          constraints (j).solveConstraintObsolete (constraints (j).getRigidBodyA, constraints (j).getRigidBodyB, infoGlobal.m_timeStep);
      --                  end loop;
      --
      --                  -- solve all contact constraints using SIMD, if available
      --                  int numPoolConstraints = m_tmpSolverContactConstraintPool.size();
      --                  for j in 1 .. numPoolConstraints
      --                  looop
      --                          const impact.d3.solver_Constraint& solveManifold = Self.m_tmpSolverContactConstraintPool[m_orderTmpConstraintPool[j]];
      --                          resolveSingleConstraintRowLowerLimitSIMD (*solveManifold.m_solverBodyA,*solveManifold.m_solverBodyB,solveManifold);
      --
      --                  end loop;
      --                  -- solve all friction constraints, using SIMD, if available
      --                  int numFrictionPoolConstraints = m_tmpSolverContactFrictionConstraintPool.size();
      --                  for j in 1 .. numFrictionPoolConstraints
      --                  loop
      --                          impact.d3.solver_Constraint& solveManifold = Self.m_tmpSolverContactFrictionConstraintPool[m_orderFrictionConstraintPool[j]];
      --                          impact.d3.Scalar totalImpulse = Self.m_tmpSolverContactConstraintPool[solveManifold.m_frictionIndex].m_appliedImpulse;
      --
      --                          if totalImpulse > 0.0 then
      --
      --                                  solveManifold.m_lowerLimit = -(solveManifold.m_friction * totalImpulse);
      --                                  solveManifold.m_upperLimit = solveManifold.m_friction * totalImpulse;
      --
      --                                  resolveSingleConstraintRowGenericSIMD (*solveManifold.m_solverBodyA,        *solveManifold.m_solverBodyB, solveManifold);
      --                          end if;
      --                  end loop;
      --          } else
      --          {

      declare
         constraint         : impact.d3.solver_Constraint.View;
         numPoolConstraints : Integer;

         solveManifold              : impact.d3.solver_Constraint.view;
         numFrictionPoolConstraints : Integer;
         pragma Unreferenced (numFrictionPoolConstraints);

         totalImpulse   :          math.Real;
         numConstraints : constant Integer  := Integer (constraints.Length);

      begin
         --  solve all joint constraints
         for j in 1 .. Integer (Self.m_tmpSolverNonContactConstraintPool.Length)
         loop
            constraint := Self.m_tmpSolverNonContactConstraintPool.Element (j);
            Self.resolveSingleConstraintRowGeneric (constraint.union1.m_solverBodyA.all'Access,
                                                    constraint.union2.m_solverBodyB.all'Access,
                                                    constraint.all);
         end loop;


         for j in 1 .. numConstraints
         loop
            constraints.Element (j).solveConstraintObsolete (constraints.Element (j).getRigidBodyA,
                                                             constraints.Element (j).getRigidBodyB,
                                                             infoGlobal.m_timeStep);
         end loop;


         --  solve all contact constraints
         numPoolConstraints := Integer (Self.m_tmpSolverContactConstraintPool.Length);

         for j in 1 .. numPoolConstraints
         loop
            solveManifold := Self.m_tmpSolverContactConstraintPool.Element (Self.m_orderTmpConstraintPool.Element (j));
            Self.resolveSingleConstraintRowLowerLimit (solveManifold.union1.m_solverBodyA.all'Access,
                                                       solveManifold.union2.m_solverBodyB.all'Access,
                                                       solveManifold.all);
         end loop;


         --  solve all friction constraints
         numFrictionPoolConstraints := Integer (Self.m_tmpSolverContactFrictionConstraintPool.Length);

         --  TBD:   add below back in ***********************

         for j in 1 .. 0 -- numFrictionPoolConstraints
         loop
            solveManifold := Self.m_tmpSolverContactFrictionConstraintPool.Element (Self.m_orderFrictionConstraintPool.Element (j));
            totalImpulse  := Self.m_tmpSolverContactConstraintPool        .Element (solveManifold.m_frictionIndex + 0).m_appliedImpulse;

            if totalImpulse > 0.0 then

               solveManifold.m_lowerLimit := -(solveManifold.m_friction * totalImpulse);
               solveManifold.m_upperLimit :=   solveManifold.m_friction * totalImpulse;

               Self.resolveSingleConstraintRowGeneric (solveManifold.union1.m_solverBodyA.all'Access,
                                                       solveManifold.union2.m_solverBodyB.all'Access,
                                                       solveManifold.all);
            end if;
         end loop;

      end;
--                } // end if


      return 0.0;
   end solveSingleIteration;








   function  solveGroupCacheFriendlySetup  (Self : in out Item;   bodies      : access impact.d3.Object   .Vector;
                                                                  manifoldPtr : access impact.d3.Manifold.Vector;
                                                                  constraints : access impact.d3.Joint   .Vector;
                                                                  infoGlobal  : in     impact.d3.contact_solver_Info .Item'Class) return math.Real
   is
      use type impact.d3.Object.rigid.view;

      the_Body   : impact.d3.Object.rigid.view;
      constraint : impact.d3.Joint.view;

      numConstraints : constant Integer := Integer (constraints.Length);
      numManifolds   : constant Integer := Integer (manifoldPtr.Length);
      numBodies      : constant Integer := Integer (bodies     .Length);

   begin
      if numConstraints + numManifolds = 0 then
         return 0.0;
      end if;


      if infoGlobal.m_splitImpulse then

         for i in 1 .. numBodies
         loop
            the_Body := impact.d3.Object.rigid.view (bodies.Element (i));

            if the_Body /= null then
               the_Body.internalGetDeltaLinearVelocity.all  := (0.0, 0.0, 0.0);
               the_Body.internalGetDeltaAngularVelocity.all := (0.0, 0.0, 0.0);
               the_Body.internalGetPushVelocity.all         := (0.0, 0.0, 0.0);
               the_Body.internalGetTurnVelocity.all         := (0.0, 0.0, 0.0);
            end if;
         end loop;

      else

         for i in 1 .. numBodies
         loop
            the_Body := impact.d3.Object.rigid.view (bodies.Element (i));

            if the_Body /= null then
               the_Body.internalGetDeltaLinearVelocity.all  := (0.0, 0.0, 0.0);
               the_Body.internalGetDeltaAngularVelocity.all := (0.0, 0.0, 0.0);
            end if;
         end loop;

      end if;





      for j in 1 .. numConstraints
      loop
         constraint := constraints.Element (j);
         constraint.buildJacobian;
         constraint.internalSetAppliedImpulse (0.0);
      end loop;




      declare
         use impact.d3.Joint.Vectors;

         totalNumRows : Integer := 0;
--           i            : Integer;

         info1        : access impact.d3.Joint.btConstraintInfo1;

         currentRow : Integer := 0;

      begin
         Self.m_tmpConstraintSizesPool.reserve_Capacity (ada.Containers.Count_Type (numConstraints));

         --  calculate the total number of contraint rows
         --
         for i in 1 .. numConstraints
         loop
            info1 := Self.m_tmpConstraintSizesPool.Element (i);

            if constraints.Element (i).isEnabled then
               constraints.Element (i).getInfo1 (info1.all);
            else
               info1.m_numConstraintRows := 0;
               info1.nub                 := 0;
            end if;

            totalNumRows := totalNumRows + info1.m_numConstraintRows;
         end loop;

         Self.m_tmpSolverNonContactConstraintPool.reserve_Capacity (ada.Containers.Count_Type (totalNumRows));


         --  setup the impact.d3.solver_Constraints
         --

         for i in 1 .. numConstraints
         loop
            info1 := Self.m_tmpConstraintSizesPool.Element (i);

            if info1.m_numConstraintRows /= 0 then
               pragma Assert (currentRow < totalNumRows);

               declare
                  use impact.d3.solver_Constraint.Vectors;
                  use type C.ptrdiff_t;

                  Cursor               : impact.d3.solver_Constraint.Vectors.Cursor := to_Cursor (Self.m_tmpSolverNonContactConstraintPool, currentRow);
                  currentConstraintRow : impact.d3.solver_Constraint.view; --  := Self.m_tmpSolverNonContactConstraintPool (currentRow);
                  constraint           : constant impact.d3.Joint .view := constraints.Element (i);


                  rbA                  : constant impact.d3.Object.rigid.View := constraint.getRigidBodyA;
                  rbB                  : constant impact.d3.Object.rigid.View := constraint.getRigidBodyB;

                  info2                : aliased impact.d3.Joint.btConstraintInfo2;
               begin

                  for j in 1 .. info1.m_numConstraintRows
                  loop
                     currentConstraintRow := Element (Cursor);

                     currentConstraintRow.all := impact.d3.solver_Constraint.null_Constraint; -- memset (&currentConstraintRow[j], 0, sizeof(impact.d3.solver_Constraint));
                     currentConstraintRow.m_lowerLimit         := -impact.d3.Scalar.SIMD_INFINITY;
                     currentConstraintRow.m_upperLimit         :=  impact.d3.Scalar.SIMD_INFINITY;
                     currentConstraintRow.m_appliedImpulse     :=  0.0;
                     currentConstraintRow.m_appliedPushImpulse :=  0.0;
                     currentConstraintRow.union1.m_solverBodyA :=  rbA;
                     currentConstraintRow.union2.m_solverBodyB :=  rbB;

                     next (Cursor);
                  end loop;

                  rbA.internalGetDeltaLinearVelocity.all  := (0.0, 0.0, 0.0);
                  rbA.internalGetDeltaAngularVelocity.all := (0.0, 0.0, 0.0);
                  rbB.internalGetDeltaLinearVelocity.all  := (0.0, 0.0, 0.0);
                  rbB.internalGetDeltaAngularVelocity.all := (0.0, 0.0, 0.0);



                  info2.fps             := 1.0 / infoGlobal.m_timeStep;
                  info2.erp             := infoGlobal.m_erp;
                  info2.m_J1linearAxis  := currentConstraintRow.m_contactNormal (0)'Access;
                  info2.m_J1angularAxis := currentConstraintRow.m_relpos1CrossNormal (0)'Access;
                  info2.m_J2linearAxis  := null;
                  info2.m_J2angularAxis := currentConstraintRow.m_relpos2CrossNormal (0)'Access;
                  info2.rowskip         := impact.d3.solver_Constraint.Item'Size / math.Real'Size;  -- check this

                  --  the size of impact.d3.solver_Constraint needs be a multiple of impact.d3.Scalar
                  pragma Assert (info2.rowskip * math.Real'Size = impact.d3.solver_Constraint.Item'Size);

                  info2.m_constraintError    := currentConstraintRow.m_rhs'Access;
                  currentConstraintRow.m_cfm := infoGlobal.m_globalCfm;
                  info2.m_damping            := infoGlobal.m_damping;
                  info2.cfm                  := currentConstraintRow.m_cfm'Access;
                  info2.m_lowerLimit         := currentConstraintRow.m_lowerLimit'Access;
                  info2.m_upperLimit         := currentConstraintRow.m_upperLimit'Access;
                  info2.m_numIterations      := infoGlobal.m_numIterations;

                  constraints.Element (i).getInfo2 (info2);


                  --  finalize the constraint setup
                  Cursor := to_Cursor (Self.m_tmpSolverNonContactConstraintPool, currentRow);

                  for j in 1 .. info1.m_numConstraintRows
                  loop
                     declare
                        use impact.d3.Vector, Math;

                        solverConstraint : constant impact.d3.solver_Constraint.view := Element (Cursor);

                        ftorqueAxis1     : math.Vector_3;
                        ftorqueAxis2     : math.Vector_3;

                        iMJlA,
                        iMJaA,
                        iMJlB,
                        iMJaB            : math.Vector_3;

                        sum              : math.Real;

                     begin
                        if solverConstraint.m_upperLimit >= constraints.Element (i).getBreakingImpulseThreshold then
                           solverConstraint.m_upperLimit := constraints.Element (i).getBreakingImpulseThreshold;
                        end if;

                        if solverConstraint.m_lowerLimit <= -constraints.Element (i).getBreakingImpulseThreshold then
                           solverConstraint.m_lowerLimit := -constraints.Element (i).getBreakingImpulseThreshold;
                        end if;

                        solverConstraint.m_originalContactPoint := constraint.all'Access;

                        ftorqueAxis1                         := to_Math (solverConstraint.m_relpos1CrossNormal);
                        solverConstraint.m_angularComponentA := Scaled (constraint.getRigidBodyA.getInvInertiaTensorWorld * ftorqueAxis1, by => constraint.getRigidBodyA.getAngularFactor);
                        --  tbd: check these 'Scaled' are correct operation.
                        ftorqueAxis2                         := to_Math (solverConstraint.m_relpos2CrossNormal);
                        solverConstraint.m_angularComponentB := Scaled (constraint.getRigidBodyB.getInvInertiaTensorWorld * ftorqueAxis2, by => constraint.getRigidBodyB.getAngularFactor);

                        iMJlA := to_Math (solverConstraint.m_contactNormal) * rbA.getInvMass;
                        iMJaA := rbA.getInvInertiaTensorWorld     * to_Math (solverConstraint.m_relpos1CrossNormal);

                        iMJlB := to_Math (solverConstraint.m_contactNormal) * rbB.getInvMass;                          -- sign of normal?
                        iMJaB := rbB.getInvInertiaTensorWorld     * to_Math (solverConstraint.m_relpos2CrossNormal);

                        sum :=   dot (iMJlA, to_Math (solverConstraint.m_contactNormal))
                               + dot (iMJaA, to_Math (solverConstraint.m_relpos1CrossNormal))
                               + dot (iMJlB, to_Math (solverConstraint.m_contactNormal))
                               + dot (iMJaB, to_Math (solverConstraint.m_relpos2CrossNormal));

                        solverConstraint.m_jacDiagABInv := 1.0 / sum;


                        --  fix rhs
                        --  todo: add force/torque accelerators
                        declare
                           vel1Dotn : math.Real := dot (to_Math (solverConstraint.m_contactNormal), rbA.getLinearVelocity) + dot (to_Math (solverConstraint.m_relpos1CrossNormal), rbA.getAngularVelocity);
                           vel2Dotn : constant math.Real := dot (-to_Math (solverConstraint.m_contactNormal), rbB.getLinearVelocity) + dot (to_Math (solverConstraint.m_relpos2CrossNormal), rbB.getAngularVelocity);

                           rel_vel  : constant math.Real := vel1Dotn + vel2Dotn;

                           restitution        : constant math.Real := 0.0;
                           positionalError    : constant math.Real := solverConstraint.m_rhs;  -- already filled in by getConstraintInfo2
                           velocityError      : constant math.Real := restitution - rel_vel * info2.m_damping;
                           penetrationImpulse : constant math.Real := positionalError * solverConstraint.m_jacDiagABInv;
                           velocityImpulse    : constant math.Real := velocityError   * solverConstraint.m_jacDiagABInv;
                        begin
                           solverConstraint.m_rhs            := penetrationImpulse + velocityImpulse;
                           solverConstraint.m_appliedImpulse := 0.0;
                        end;
                     end;

                     next (Cursor);
                  end loop;

               end;
            end if;

            currentRow := currentRow + Self.m_tmpConstraintSizesPool.Element (i).m_numConstraintRows;
         end loop;
      end;



      declare
         manifold : impact.d3.Manifold.view;
         --                        impact.d3.Object* colObj0=0,*colObj1=0;
      begin

         for i in 1 .. numManifolds
         loop
            manifold := manifoldPtr.Element (i);
            Self.convertContact (manifold, infoGlobal);
         end loop;
      end;



      declare
         info : impact.d3.contact_solver_Info.item'Class := infoGlobal;

         numConstraintPool : constant Integer := Integer (Self.m_tmpSolverContactConstraintPool.Length);
         numFrictionPool   : constant Integer := Integer (Self.m_tmpSolverContactFrictionConstraintPool.Length);
      begin
         --  @todo: use stack allocator for such temporarily memory, same for solver bodies/constraints

         Self.m_orderTmpConstraintPool     .set_Length (ada.Containers.Count_Type (numConstraintPool));
         Self.m_orderFrictionConstraintPool.set_Length (ada.Containers.Count_Type (numFrictionPool));

         for i in 1 .. numConstraintPool
         loop
            Self.m_orderTmpConstraintPool.replace_Element (i, i);
         end loop;

         for i in 1 .. numFrictionPool
         loop
            Self.m_orderFrictionConstraintPool.replace_Element (i,  i);
         end loop;
      end;



      return 0.0;
   end solveGroupCacheFriendlySetup;












   function  solveGroupCacheFriendlyIterations (Self : in out Item;   bodies      : access impact.d3.Object   .Vector;
                                                                      manifoldPtr : access impact.d3.Manifold.Vector;
                                                                      constraints : access impact.d3.Joint   .Vector;
                                                                      infoGlobal  : in     impact.d3.contact_solver_Info .Item'Class) return math.Real
   is
--        iteration : Integer;
      unused    : math.Real;
      pragma Unreferenced (unused);
   begin
      --  should traverse the contacts random order...

      Self.solveGroupCacheFriendlySplitImpulseIterations (bodies,      --   numBodies,
                                                          manifoldPtr, --   numManifolds,
                                                          constraints, --   numConstraints,
                                                          infoGlobal);

      for iteration in 1 .. infoGlobal.m_numIterations
      loop
         unused := Self.solveSingleIteration (iteration,
                                              bodies,        -- numBodies,
                                              manifoldPtr,   -- numManifolds,
                                              constraints,   -- numConstraints,
                                              infoGlobal);
      end loop;


      return 0.0;
   end solveGroupCacheFriendlyIterations;




end impact.d3.constraint_Solver.sequential_impulse;






--  tbd: do we need these SIMD functuions below ?
--


--  // Project Gauss Seidel or the equivalent Sequential Impulse
--

--  void impact.d3.constraint_Solver.sequential_impulse::resolveSingleConstraintRowGenericSIMD(impact.d3.Object.rigid& body1,impact.d3.Object.rigid& body2,const impact.d3.solver_Constraint& c)
--  {
--          resolveSingleConstraintRowGeneric(body1,body2,c);
--  }






--   void impact.d3.constraint_Solver.sequential_impulse::resolveSingleConstraintRowLowerLimitSIMD(impact.d3.Object.rigid& body1,impact.d3.Object.rigid& body2,const impact.d3.solver_Constraint& c)
--  {
--          resolveSingleConstraintRowLowerLimit(body1,body2,c);
--  }






--   void impact.d3.constraint_Solver.sequential_impulse::resolveSplitPenetrationSIMD(impact.d3.Object.rigid& body1,impact.d3.Object.rigid& body2,const impact.d3.solver_Constraint& c)
--  {
--          resolveSplitPenetrationImpulseCacheFriendly(body1,body2,c);
--  }
--

