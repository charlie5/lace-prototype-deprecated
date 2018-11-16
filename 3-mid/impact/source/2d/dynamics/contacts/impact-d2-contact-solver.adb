with ada.unchecked_Deallocation;



package body impact.d2.contact.Solver
is
   use type int32;

   B2_DEBUG_SOLVER : constant := 0;


   ---------
   --  Forge
   --

   function to_b2ContactSolver (def : in b2ContactSolverDef) return b2ContactSolver
   is
      Self : b2ContactSolver;
   begin
      Self.step  := def.step;
      Self.count := def.count;

      Self.positionConstraints := new b2ContactPositionConstraints (0 .. Self.count - 1);
      Self.velocityConstraints := new b2ContactVelocityConstraints (0 .. Self.count - 1);

      Self.positions  := def.positions;
      Self.velocities := def.velocities;
      Self.contacts   := def.contacts;

      --  Initialize position independent portions of the constraints.
      --
      for i in 0 .. Self.Count - 1
      loop
         declare
            use Fixture;

            contact     : constant access b2Contact := Self.contacts (i);

            fixtureA    : constant access b2Fixture := contact.m_fixtureA;
            fixtureB    : constant access b2Fixture := contact.m_fixtureB;

            shapeA      : constant Shape.view       := fixtureA.GetShape.all'Access;
            shapeB      : constant Shape.view       := fixtureB.GetShape.all'Access;

            radiusA     : constant float32          := shapeA.m_radius;
            radiusB     : constant float32          := shapeB.m_radius;

            bodyA       : constant Solid_view       := fixtureA.GetBody.all'Access;
            bodyB       : constant Solid_view       := fixtureB.GetBody.all'Access;

            manifold    : constant Manifold_view    := contact.GetManifold.all'Access;

            pointCount  : constant int32            := manifold.pointCount;   pragma assert (pointCount > 0);

            vc          :          b2ContactVelocityConstraint renames Self.velocityConstraints (i);
            pc          :          b2ContactPositionConstraint renames Self.positionConstraints (i);

         begin
            vc.friction     := contact.m_friction;
            vc.restitution  := contact.m_restitution;
            vc.tangentSpeed := contact.m_tangentSpeed;
            vc.indexA       := bodyA.m_islandIndex;
            vc.indexB       := bodyB.m_islandIndex;
            vc.invMassA     := bodyA.m_invMass.all;
            vc.invMassB     := bodyB.m_invMass.all;
            vc.invIA        := bodyA.m_invI.all;
            vc.invIB        := bodyB.m_invI.all;
            vc.contactIndex := i;
            vc.pointCount   := pointCount;
            vc.K            := ((0.0, 0.0),
                                (0.0, 0.0));
            vc.normalMass   := ((0.0, 0.0),
                                (0.0, 0.0));
            pc.indexA       := bodyA.m_islandIndex;
            pc.indexB       := bodyB.m_islandIndex;
            pc.invMassA     := bodyA.m_invMass.all;
            pc.invMassB     := bodyB.m_invMass.all;
            pc.localCenterA := bodyA.m_sweep.localCenter;
            pc.localCenterB := bodyB.m_sweep.localCenter;
            pc.invIA        := bodyA.m_invI.all;
            pc.invIB        := bodyB.m_invI.all;
            pc.localNormal  := manifold.localNormal;
            pc.localPoint   := manifold.localPoint;
            pc.pointCount   := pointCount;
            pc.radiusA      := radiusA;
            pc.radiusB      := radiusB;
            pc.kind         := manifold.kind;

            for j in 0 .. pointCount - 1
            loop
               declare
                  cp  : collision.b2ManifoldPoint renames manifold.points (j);
                  vcp : b2VelocityConstraintPoint renames vc      .points (uint32 (j));
               begin
                  if Self.step.warmStarting
                  then
                     vcp.normalImpulse  := Self.step.dtRatio * cp.normalImpulse;
                     vcp.tangentImpulse := Self.step.dtRatio * cp.tangentImpulse;
                  else
                     vcp.normalImpulse  := 0.0;
                     vcp.tangentImpulse := 0.0;
                  end if;

                  vcp.rA           := (0.0, 0.0);
                  vcp.rB           := (0.0, 0.0);
                  vcp.normalMass   := 0.0;
                  vcp.tangentMass  := 0.0;
                  vcp.velocityBias := 0.0;

                  pc.localPoints (j) := cp.localPoint;
               end;
            end loop;
         end;
      end loop;

      return Self;
   end to_b2ContactSolver;



   procedure destruct (Self : in out b2ContactSolver)
   is
      procedure free is new ada.unchecked_Deallocation (b2ContactVelocityConstraints, b2ContactVelocityConstraints_view);
      procedure free is new ada.unchecked_Deallocation (b2ContactPositionConstraints, b2ContactPositionConstraints_view);
   begin
      free (Self.velocityConstraints);
      free (Self.positionConstraints);
   end destruct;





   --------------
   --  Attributes

   procedure WarmStart (Self : in out b2ContactSolver)
   is
   begin
--        --  Warm start.
--        for i in 1 .. Self.m_constraintCount loop
--           declare
--              c        : b2ContactConstraint renames Self.m_constraints (i);
--
--              bodyA    : constant access Solid.b2Body'Class := c.bodyA;
--              bodyB    : constant access Solid.b2Body'Class := c.bodyB;
--
--              invMassA : float32 := bodyA.m_invMass.all;
--              invIA    : float32 := bodyA.m_invI.all;
--              invMassB : float32 := bodyB.m_invMass.all;
--              invIB    : float32 := bodyB.m_invI.all;
--              normal   : constant b2Vec2  := c.normal;
--              tangent  : constant b2Vec2  := b2Cross (normal, 1.0);
--           begin
--
--              for j in 1 .. c.pointCount loop
--                 declare
--                    ccp : b2ContactConstraintPoint renames c.points (uint32 (j));
--                    P   : constant b2Vec2                   :=      ccp.normalImpulse * normal + ccp.tangentImpulse * tangent;
--                 begin
--
--                    bodyA.m_angularVelocity.all := bodyA.m_angularVelocity.all - invIA * b2Cross (ccp.rA, P);
--                    bodyA.m_linearVelocity.all  := bodyA.m_linearVelocity.all  - invMassA * P;
--                    bodyB.m_angularVelocity.all := bodyB.m_angularVelocity.all + invIB * b2Cross (ccp.rB, P);
--                    bodyB.m_linearVelocity.all  := bodyB.m_linearVelocity.all  + invMassB * P;
--                 end;
--              end loop;
--
--           end;
--        end loop;
      null;
   end WarmStart;




   procedure SolveVelocityConstraints (Self : in out b2ContactSolver)
   is
   begin
--        for i in 1 .. Self.m_constraintCount loop
--           declare
--              c : b2ContactConstraint renames Self.m_constraints (i);
--
--              bodyA : constant Solid_view := c.bodyA;
--              bodyB : constant Solid_view := c.bodyB;
--
--              wA : float32 := bodyA.m_angularVelocity.all;
--              wB : float32 := bodyB.m_angularVelocity.all;
--              vA : b2Vec2 := bodyA.m_linearVelocity.all;
--              vB : b2Vec2 := bodyB.m_linearVelocity.all;
--
--              invMassA : float32 := bodyA.m_invMass.all;
--              invIA    : float32 := bodyA.m_invI.all;
--              invMassB : float32 := bodyB.m_invMass.all;
--              invIB    : float32 := bodyB.m_invI.all;
--              normal   : constant b2Vec2 := c.normal;
--              tangent  : constant b2Vec2 := b2Cross (normal, 1.0);
--              friction : float32 := c.friction;
--
--              pragma Assert (c.pointCount = 1 or else c.pointCount = 2);
--           begin
--
--              --  Solve tangent constraints
--              for j in 1 .. c.pointCount loop
--                 declare
--                    ccp         : b2ContactConstraintPoint renames c.points (uint32 (j));
--
--                    --  Relative velocity at contact
--                    dv          : constant b2Vec2  :=   vB + b2Cross (wB, ccp.rB)
--                                             - vA - b2Cross (wA, ccp.rA);
--
--                    --  Compute tangent force
--                    vt          : constant float32 := b2Dot (dv, tangent);
--                    lambda      : float32 := ccp.tangentMass * (-vt);
--
--                    --  b2Clamp the accumulated force
--                    maxFriction : constant float32 := friction * ccp.normalImpulse;
--                    newImpulse  : constant float32 := b2Clamp (ccp.tangentImpulse + lambda,  -maxFriction, maxFriction);
--
--                    P           : b2Vec2;
--                 begin
--
--                    lambda := newImpulse - ccp.tangentImpulse;
--
--                    --  Apply contact impulse
--                    P := lambda * tangent;
--
--                    vA := vA - invMassA * P;
--                    wA := wA - invIA * b2Cross (ccp.rA, P);
--
--                    vB := vB + invMassB * P;
--                    wB := wB + invIB * b2Cross (ccp.rB, P);
--
--                    ccp.tangentImpulse := newImpulse;
--                 end;
--              end loop;
--
--              --  Solve normal constraints
--              if c.pointCount = 1 then
--                 declare
--                    ccp        : b2ContactConstraintPoint renames c.points (1);
--
--                    --  Relative velocity at contact
--                    dv         : constant b2Vec2  :=   vB + b2Cross (wB, ccp.rB)
--                                            - vA - b2Cross (wA, ccp.rA);
--
--                    --  Compute normal impulse
--                    vn         : constant float32 := b2Dot (dv, normal);
--                    lambda     : float32 := -ccp.normalMass * (vn - ccp.velocityBias);
--
--                    --  b2Clamp the accumulated impulse
--                    newImpulse : constant float32 := float32'Max (ccp.normalImpulse + lambda, 0.0);
--
--                    P          : b2Vec2;
--                 begin
--
--                    lambda := newImpulse - ccp.normalImpulse;
--
--                    --  Apply contact impulse
--                    P := lambda * normal;
--                    vA := vA - invMassA * P;
--                    wA := wA - invIA * b2Cross (ccp.rA, P);
--
--                    vB := vB + invMassB * P;
--                    wB := wB + invIB * b2Cross (ccp.rB, P);
--                    ccp.normalImpulse := newImpulse;
--                 end;
--
--              else
--                 --  Block solver developed in collaboration with Dirk Gregorius (back in 01/07 on Box2D_Lite).
--                 --  Build the mini LCP for this contact patch
--                 --
--                 --  vn = A * x + b, vn >= 0, , vn >= 0, x >= 0 and vn_i * x_i = 0 with i = 1..2
--                 --
--                 --  A = J * W * JT and J = ( -n, -r1 x n, n, r2 x n )
--                 --  b = vn_0 - velocityBias
--                 --
--                 --  The system is solved using the "Total enumeration method" (s. Murty). The complementary constraint vn_i * x_i
--                 --  implies that we must have in any solution either vn_i = 0 or x_i = 0. So for the 2D contact problem the cases
--                 --  vn1 = 0 and vn2 = 0, x1 = 0 and x2 = 0, x1 = 0 and vn2 = 0, x2 = 0 and vn1 = 0 need to be tested. The first valid
--                 --  solution that satisfies the problem is chosen.
--                 --
--                 --  In order to account of the accumulated impulse 'a' (because of the iterative nature of the solver which only requires
--                 --  that the accumulated impulse is clamped and not the incremental impulse) we change the impulse variable (x_i).
--                 --
--                 --  Substitute:
--                 --
--                 --  x = x' - a
--                 --
--                 --  Plug into above equation:
--                 --
--                 --  vn = A * x + b
--                 --    = A * (x' - a) + b
--                 --    = A * x' + b - A * a
--                 --    = A * x' + b'
--                 --  b' = b - A * a;
--                 declare
--
--                    cp1 : b2ContactConstraintPoint renames c.points (1);
--                    cp2 : b2ContactConstraintPoint renames c.points (2);
--
--                    a   : b2Vec2  := (cp1.normalImpulse,  cp2.normalImpulse);
--                    pragma Assert (a.x >= 0.0 and then a.y >= 0.0);
--
--                    --  Relative velocity at contact
--                    dv1 : constant b2Vec2  := vB + b2Cross (wB, cp1.rB) - vA - b2Cross (wA, cp1.rA);
--                    dv2 : constant b2Vec2  := vB + b2Cross (wB, cp2.rB) - vA - b2Cross (wA, cp2.rA);
--
--                    --  Compute normal velocity
--                    vn1 : float32 := b2Dot (dv1, normal);
--                    vn2 : float32 := b2Dot (dv2, normal);
--
--                    b   : b2Vec2;
--
--                    k_errorTol : constant := 1.0e-3;
--
--                    x, d,
--                    P1, P2 : b2Vec2;
--                 begin
--                    b.x := vn1 - cp1.velocityBias;
--                    b.y := vn2 - cp2.velocityBias;
--                    b   := b   - b2Mul (c.K, a);
--
--                    loop
--                       --
--                       --  Case 1: vn = 0
--                       --
--                       --  0 = A * x' + b'
--                       --
--                       --  Solve for x':
--                       --
--                       --  x' = - inv(A) * b'
--                       --
--                       x := -b2Mul (c.normalMass, b);
--
--                       if x.x >= 0.0 and then x.y >= 0.0 then
--                          --  Resubstitute for the incremental impulse
--                          d := x - a;
--
--                          --  Apply incremental impulse
--                          P1 := d.x * normal;
--                          P2 := d.y * normal;
--                          vA := vA - invMassA * (P1 + P2);
--                          wA := wA - invIA * (b2Cross (cp1.rA, P1) + b2Cross (cp2.rA, P2));
--
--                          vB := vB + invMassB * (P1 + P2);
--                          wB := wB + invIB * (b2Cross (cp1.rB, P1) + b2Cross (cp2.rB, P2));
--
--                          --  Accumulate
--                          cp1.normalImpulse := x.x;
--                          cp2.normalImpulse := x.y;
--
--                          --  #if B2_DEBUG_SOLVER == 1
--                          --                          -- Postconditions
--                          --                          dv1 = vB + b2Cross(wB, cp1->rB) - vA - b2Cross(wA, cp1->rA);
--                          --                          dv2 = vB + b2Cross(wB, cp2->rB) - vA - b2Cross(wA, cp2->rA);
--                          --
--                          --                          -- Compute normal velocity
--                          --                          vn1 = b2Dot(dv1, normal);
--                          --                          vn2 = b2Dot(dv2, normal);
--                          --
--                          --                          b2Assert(b2Abs(vn1 - cp1->velocityBias) < k_errorTol);
--                          --                          b2Assert(b2Abs(vn2 - cp2->velocityBias) < k_errorTol);
--                          --  #endif
--                          exit;
--                       end if;
--
--                       --
--                       --  Case 2: vn1 = 0 and x2 = 0
--                       --
--                       --   0 = a11 * x1' + a12 * 0 + b1'
--                       --  vn2 = a21 * x1' + a22 * 0 + b2'
--                       --
--                       x.x := -cp1.normalMass * b.x;
--                       x.y := 0.0;
--                       vn1 := 0.0;
--                       vn2 := c.K.col1.y * x.x + b.y;
--
--                       if x.x >= 0.0 and then vn2 >= 0.0 then
--                          --  Resubstitute for the incremental impulse
--                          d  := x - a;
--
--                          --  Apply incremental impulse
--                          P1 := d.x * normal;
--                          P2 := d.y * normal;
--                          vA := vA - invMassA * (P1 + P2);
--                          wA := wA - invIA * (b2Cross (cp1.rA, P1) + b2Cross (cp2.rA, P2));
--
--                          vB := vB + invMassB * (P1 + P2);
--                          wB := wB + invIB * (b2Cross (cp1.rB, P1) + b2Cross (cp2.rB, P2));
--
--                          --  Accumulate
--                          cp1.normalImpulse := x.x;
--                          cp2.normalImpulse := x.y;
--
--                          --  #if B2_DEBUG_SOLVER == 1
--                          --                          -- Postconditions
--                          --                          dv1 = vB + b2Cross(wB, cp1->rB) - vA - b2Cross(wA, cp1->rA);
--                          --
--                          --                          -- Compute normal velocity
--                          --                          vn1 = b2Dot(dv1, normal);
--                          --
--                          --                          b2Assert(b2Abs(vn1 - cp1->velocityBias) < k_errorTol);
--                          --  #endif
--                          exit;
--                       end if;
--
--
--                       --
--                       --  Case 3: vn2 = 0 and x1 = 0
--                       --
--                       --  vn1 = a11 * 0 + a12 * x2' + b1'
--                       --   0 = a21 * 0 + a22 * x2' + b2'
--                       --
--                       x.x := 0.0;
--                       x.y := -cp2.normalMass * b.y;
--                       vn1 := c.K.col2.x * x.y + b.x;
--                       vn2 := 0.0;
--
--                       if x.y >= 0.0 and then vn1 >= 0.0 then
--                          --  Resubstitute for the incremental impulse
--                          d  := x - a;
--
--                          --  Apply incremental impulse
--                          P1 := d.x * normal;
--                          P2 := d.y * normal;
--                          vA := vA - invMassA * (P1 + P2);
--                          wA := wA - invIA * (b2Cross (cp1.rA, P1) + b2Cross (cp2.rA, P2));
--
--                          vB := vB + invMassB * (P1 + P2);
--                          wB := wB + invIB * (b2Cross (cp1.rB, P1) + b2Cross (cp2.rB, P2));
--
--                          --  Accumulate
--                          cp1.normalImpulse := x.x;
--                          cp2.normalImpulse := x.y;
--
--                          --  #if B2_DEBUG_SOLVER == 1
--                          --                          -- Postconditions
--                          --                          dv2 = vB + b2Cross(wB, cp2->rB) - vA - b2Cross(wA, cp2->rA);
--                          --
--                          --                          -- Compute normal velocity
--                          --                          vn2 = b2Dot(dv2, normal);
--                          --
--                          --                          b2Assert(b2Abs(vn2 - cp2->velocityBias) < k_errorTol);
--                          --  #endif
--                          exit;
--                       end if;
--
--                       --
--                       --  Case 4: x1 = 0 and x2 = 0
--                       --
--                       --  vn1 = b1
--                       --  vn2 = b2;
--                       x.x := 0.0;
--                       x.y := 0.0;
--                       vn1 := b.x;
--                       vn2 := b.y;
--
--                       if vn1 >= 0.0 and then vn2 >= 0.0 then
--                          --  Resubstitute for the incremental impulse
--                          d  := x - a;
--
--                          --  Apply incremental impulse
--                          P1 := d.x * normal;
--                          P2 := d.y * normal;
--                          vA := vA - invMassA * (P1 + P2);
--                          wA := wA - invIA * (b2Cross (cp1.rA, P1) + b2Cross (cp2.rA, P2));
--
--                          vB := vB + invMassB * (P1 + P2);
--                          wB := wB + invIB * (b2Cross (cp1.rB, P1) + b2Cross (cp2.rB, P2));
--
--                          --  Accumulate
--                          cp1.normalImpulse := x.x;
--                          cp2.normalImpulse := x.y;
--
--                          exit;
--                       end if;
--
--                       --  No solution, give up. This is hit sometimes, but it doesn't seem to matter.
--                       exit;
--                    end loop;
--                 end;
--              end if;
--
--              bodyA.m_linearVelocity.all  := vA;
--              bodyA.m_angularVelocity.all := wA;
--              bodyB.m_linearVelocity.all  := vB;
--              bodyB.m_angularVelocity.all := wB;
--           end;
--        end loop;
      null;

   end SolveVelocityConstraints;





   procedure StoreImpulses (Self : in out b2ContactSolver)
   is
   begin
--        for i in 1 .. Self.m_constraintCount loop
--           declare
--              c : b2ContactConstraint  renames Self.m_constraints (i);
--              m : collision.b2Manifold renames c.manifold.all;
--           begin
--              for j in 1 .. c.pointCount loop
--                 m.points (j).normalImpulse  := c.points (uint32 (j)).normalImpulse;
--                 m.points (j).tangentImpulse := c.points (uint32 (j)).tangentImpulse;
--              end loop;
--           end;
--        end loop;
      null;
   end StoreImpulses;















   type b2PositionSolverManifold is
      record
         normal     : b2Vec2;
         point      : b2Vec2;
         separation : float32;
      end record;



--     procedure Initialize (Self : in out b2PositionSolverManifold;   cc    : access b2ContactConstraint;
--                                                                     index : in     int32)
--     is
--        use Collision;
--
--        pointA,
--        pointB,
--        planePoint,
--        clipPoint : b2Vec2;
--     begin
--        pragma Assert (cc.pointCount > 0);
--
--        case cc.kind is
--        when e_circles =>
--           pointA := cc.bodyA.GetWorldPoint (cc.localPoint);
--           pointB := cc.bodyB.GetWorldPoint (cc.points (1).localPoint);
--
--           if b2DistanceSquared (pointA, pointB)  >  b2_epsilon * b2_epsilon then
--              Self.normal  := pointB - pointA;
--              Normalize (Self.normal);
--           else
--              Self.normal  := (1.0, 0.0);
--           end if;
--
--           Self.point      := 0.5 * (pointA + pointB);
--           Self.separation := b2Dot (pointB - pointA, Self.normal) - cc.radius;
--
--        when e_faceA =>
--           Self.normal     := cc.bodyA.GetWorldVector (cc.localNormal);
--           planePoint      := cc.bodyA.GetWorldPoint  (cc.localPoint);
--
--           clipPoint       := cc.bodyB.GetWorldPoint (cc.points (uint32 (index)).localPoint);
--           Self.separation := b2Dot (clipPoint - planePoint, Self.normal) - cc.radius;
--           Self.point      := clipPoint;
--
--        when e_faceB =>
--           Self.normal     := cc.bodyB.GetWorldVector (cc.localNormal);
--           planePoint      := cc.bodyB.GetWorldPoint  (cc.localPoint);
--
--           clipPoint       := cc.bodyA.GetWorldPoint (cc.points (uint32 (index)).localPoint);
--           Self.separation := b2Dot (clipPoint - planePoint, Self.normal) - cc.radius;
--           Self.point      := clipPoint;
--
--           --  Ensure normal points from A to B
--           Self.normal     := -Self.normal;
--        end case;
--
--     end Initialize;






   function SolvePositionConstraints  (Self : in     b2ContactSolver;   baumgarte : float32) return Boolean
   is
      minSeparation : float32 := 0.0;
      psm           : b2PositionSolverManifold;
   begin
--        for i in 1 .. Self.m_constraintCount loop
--           declare
--              c        : constant access b2ContactConstraint := Self.m_constraints (i)'Access;
--
--              bodyA    : constant Solid_view := c.bodyA;
--              bodyB    : constant Solid_view := c.bodyB;
--
--              invMassA : float32    := bodyA.getMass * bodyA.m_invMass.all;
--              invIA    : float32    := bodyA.getMass * bodyA.m_invI.all;
--              invMassB : float32    := bodyB.getMass * bodyB.m_invMass.all;
--              invIB    : float32    := bodyB.getMass * bodyB.m_invI.all;
--
--           begin
--
--              --  Solve normal constraints
--              for j in 1 .. c.pointCount loop
--                 Initialize (psm,  c, j);
--                 declare
--                    normal     : constant b2Vec2  := psm.normal;
--
--                    point      : constant b2Vec2  := psm.point;
--                    separation : constant float32 := psm.separation;
--
--                    rA         : constant b2Vec2  := point - bodyA.m_sweep.c;
--                    rB         : constant b2Vec2  := point - bodyB.m_sweep.c;
--
--
--                    --  Prevent large corrections and allow slop.
--                    C          : constant float32 := b2Clamp (baumgarte * (separation + b2_linearSlop),  -b2_maxLinearCorrection, 0.0);
--
--                    --  Compute the effective mass.
--                    rnA        : constant float32 := b2Cross (rA, normal);
--                    rnB        : constant float32 := b2Cross (rB, normal);
--                    K          : constant float32 := invMassA + invMassB + invIA * rnA * rnA + invIB * rnB * rnB;
--
--                    --  Compute normal impulse
--                    impulse : float32;
--
--                    P       : b2Vec2;
--                 begin
--                    --  Track max constraint error.
--                    minSeparation := float32'Min (minSeparation, separation);
--
--                    if K > 0.0 then   impulse := -C / K;
--                    else              impulse := 0.0;
--                    end if;
--
--                    P := impulse * normal;
--
--                    bodyA.m_sweep.c := bodyA.m_sweep.c - invMassA * P;
--                    bodyA.m_sweep.a := bodyA.m_sweep.a - invIA * b2Cross (rA, P);
--                    bodyA.SynchronizeTransform;
--
--                    bodyB.m_sweep.c := bodyB.m_sweep.c + invMassB * P;
--                    bodyB.m_sweep.a := bodyB.m_sweep.a + invIB * b2Cross (rB, P);
--                    bodyB.SynchronizeTransform;
--                 end;
--              end loop;
--           end;
--        end loop;
--
--        --  We can't expect minSpeparation >= -b2_linearSlop because we don't
--        --  push the separation above -b2_linearSlop.
--        return minSeparation >= -1.5 * b2_linearSlop;
      return false;
   end SolvePositionConstraints;



end impact.d2.contact.Solver;
