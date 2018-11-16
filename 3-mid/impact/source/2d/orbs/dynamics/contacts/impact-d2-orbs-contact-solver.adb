with ada.unchecked_Deallocation;
with impact.d2.orbs.Shape;



package body impact.d2.orbs.contact.Solver
is
   use type int32;

   B2_DEBUG_SOLVER : constant := 0;



   function to_b2ContactSolver (contacts : in Contact.views;
                                --                                          b2StackAllocator* allocator,
                                impulseRatio : float32) return b2ContactSolver
   is
      Self : b2ContactSolver;
   begin
      --                m_allocator = allocator;

      Self.m_constraintCount := contacts'Length;
      Self.m_constraints     := new b2ContactConstraints (1 .. Self.m_constraintCount);

      for i in 1 .. Self.m_constraintCount loop
         declare
            use Fixture, impact.d2.orbs;

            contact     : constant access b2Contact := contacts (i);

            fixtureA    : constant access item := contact.m_fixtureA;
            fixtureB    : constant access item := contact.m_fixtureB;

            shapeA      : constant Shape.view    := fixtureA.GetShape.all'Access;
            shapeB      : constant Shape.view    := fixtureB.GetShape.all'Access;
            radiusA     : float32       := shapeA.m_radius;
            radiusB     : float32       := shapeB.m_radius;
            bodyA       : constant Solid_view    := fixtureA.GetBody.all'Access;
            bodyB       : constant Solid_view    := fixtureB.GetBody.all'Access;
            manifold    : constant Manifold_view := contact.GetManifold.all'Access;

            friction    : float32 := b2MixFriction    (fixtureA.GetFriction,    fixtureB.GetFriction);
            restitution : float32 := b2MixRestitution (fixtureA.GetRestitution, fixtureB.GetRestitution);

            vA          : constant b2Vec2  := bodyA.getLinearVelocity;
            vB          : constant b2Vec2  := bodyB.getLinearVelocity;
            wA          : float32 := bodyA.getAngularVelocity;
            wB          : float32 := bodyB.getAngularVelocity;

            pragma Assert (manifold.pointCount > 0);

            worldManifold : collision.b2WorldManifold;

            cc : b2ContactConstraint renames Self.m_constraints (i);
         begin
            collision.Initialize (worldManifold,  manifold.all, bodyA.getTransform, radiusA,
                                                                bodyB.getTransform, radiusB);
            cc.bodyA       := bodyA;
            cc.bodyB       := bodyB;
            cc.manifold    := manifold;
            cc.normal      := worldManifold.normal;
            cc.pointCount  := manifold.pointCount;
            cc.friction    := friction;

            cc.localNormal := manifold.localNormal;
            cc.localPoint  := manifold.localPoint;
            cc.radius      := radiusA + radiusB;
            cc.kind        := manifold.kind;

            for j in 1 .. cc.pointCount loop
               declare
                  cp       :  collision.b2ManifoldPoint renames manifold.points (j);
                  ccp      :  b2ContactConstraintPoint  renames cc.points (uint32 (j));

                  rnA, rnB,
                  rtA, rtB,
                  vRel,
                  kTangent,
                  kNormal  : float32;

                  tangent  : b2Vec2;
               begin
                  ccp.normalImpulse  := impulseRatio * cp.normalImpulse;
                  ccp.tangentImpulse := impulseRatio * cp.tangentImpulse;

                  ccp.localPoint     := cp.localPoint;

                  ccp.rA             := worldManifold.points (j) - bodyA.m_sweep.c;
                  ccp.rB             := worldManifold.points (j) - bodyB.m_sweep.c;

                  rnA := b2Cross (ccp.rA, cc.normal);
                  rnB := b2Cross (ccp.rB, cc.normal);
                  rnA := rnA * rnA;
                  rnB := rnB * rnB;

                  kNormal := bodyA.m_invMass.all + bodyB.m_invMass.all + bodyA.m_invI.all * rnA + bodyB.m_invI.all * rnB;

                  pragma Assert (kNormal > b2_epsilon);
                  ccp.normalMass := 1.0 / kNormal;

                  tangent := b2Cross (cc.normal, 1.0);

                  rtA := b2Cross (ccp.rA, tangent);
                  rtB := b2Cross (ccp.rB, tangent);
                  rtA := rtA * rtA;
                  rtB := rtB * rtB;

                  kTangent := bodyA.m_invMass.all  +  bodyB.m_invMass.all  +  bodyA.m_invI.all * rtA  +  bodyB.m_invI.all * rtB;

                  pragma Assert (kTangent > b2_epsilon);
                  ccp.tangentMass := 1.0 / kTangent;

                  --  Setup a velocity bias for restitution.
                  ccp.velocityBias := 0.0;
                  vRel             := b2Dot (cc.normal,    vB + b2Cross (wB, ccp.rB)
                                                        -  vA - b2Cross (wA, ccp.rA));
                  if vRel < -b2_velocityThreshold then
                     ccp.velocityBias := -restitution * vRel;
                  end if;
               end;
            end loop;

            --  If we have two points, then prepare the block solver.
            if cc.pointCount = 2 then
               declare
                  ccp1 : b2ContactConstraintPoint renames cc.points (1);
                  ccp2 : b2ContactConstraintPoint renames cc.points (2);
                  --                          b2ContactConstraintPoint* ccp1 = cc.points + 0;
                  --                          b2ContactConstraintPoint* ccp2 = cc.points + 1;

                  invMassA : constant float32 := bodyA.m_invMass.all;
                  invIA    : constant float32 := bodyA.m_invI.all;
                  invMassB : constant float32 := bodyB.m_invMass.all;
                  invIB    : constant float32 := bodyB.m_invI.all;

                  rn1A     : constant float32 := b2Cross (ccp1.rA, cc.normal);
                  rn1B     : constant float32 := b2Cross (ccp1.rB, cc.normal);
                  rn2A     : constant float32 := b2Cross (ccp2.rA, cc.normal);
                  rn2B     : constant float32 := b2Cross (ccp2.rB, cc.normal);

                  k11      : constant float32 := invMassA + invMassB + invIA * rn1A * rn1A + invIB * rn1B * rn1B;
                  k22      : constant float32 := invMassA + invMassB + invIA * rn2A * rn2A + invIB * rn2B * rn2B;
                  k12      : constant float32 := invMassA + invMassB + invIA * rn1A * rn2A + invIB * rn1B * rn2B;

                  --  Ensure a reasonable condition number.
                  k_maxConditionNumber : constant := 100.0;
               begin
                  if k11 * k11  <  k_maxConditionNumber * (k11 * k22 - k12 * k12) then
                     --  K is safe to invert.
                     cc.K.col1 := (k11, k12);
                     cc.K.col2 := (k12, k22);
                     cc.normalMass := GetInverse (cc.K);
                  else
                     --  The constraints are redundant, just use one.
                     --  TODO_ERIN use deepest?
                     cc.pointCount := 1;
                  end if;
               end;

            end if;
         end;
      end loop;

      return Self;
   end to_b2ContactSolver;






   procedure destruct (Self : in out b2ContactSolver)
   is
      procedure free is new ada.unchecked_Deallocation (b2ContactConstraints, b2ContactConstraints_view);
   begin
      free (Self.m_constraints);
   end destruct;





   procedure WarmStart (Self : in out b2ContactSolver)
   is
   begin
      --  Warm start.
      for i in 1 .. Self.m_constraintCount loop
         declare
            c        : b2ContactConstraint renames Self.m_constraints (i);

            bodyA    : constant access Solid.item'Class := c.bodyA;
            bodyB    : constant access Solid.item'Class := c.bodyB;

            invMassA : float32 := bodyA.m_invMass.all;
            invIA    : float32 := bodyA.m_invI.all;
            invMassB : float32 := bodyB.m_invMass.all;
            invIB    : float32 := bodyB.m_invI.all;
            normal   : constant b2Vec2  := c.normal;
            tangent  : constant b2Vec2  := b2Cross (normal, 1.0);
         begin

            for j in 1 .. c.pointCount loop
               declare
                  ccp : b2ContactConstraintPoint renames c.points (uint32 (j));
                  P   : constant b2Vec2                   :=      ccp.normalImpulse * normal + ccp.tangentImpulse * tangent;
               begin

                  bodyA.m_angularVelocity.all := bodyA.m_angularVelocity.all - invIA * b2Cross (ccp.rA, P);
                  bodyA.m_linearVelocity.all  := bodyA.m_linearVelocity.all  - invMassA * P;
                  bodyB.m_angularVelocity.all := bodyB.m_angularVelocity.all + invIB * b2Cross (ccp.rB, P);
                  bodyB.m_linearVelocity.all  := bodyB.m_linearVelocity.all  + invMassB * P;
               end;
            end loop;

         end;
      end loop;
   end WarmStart;




   procedure SolveVelocityConstraints (Self : in out b2ContactSolver)
   is
   begin
      for i in 1 .. Self.m_constraintCount loop
         declare
            c : b2ContactConstraint renames Self.m_constraints (i);

            bodyA : constant Solid_view := c.bodyA;
            bodyB : constant Solid_view := c.bodyB;

            wA : float32 := bodyA.m_angularVelocity.all;
            wB : float32 := bodyB.m_angularVelocity.all;
            vA : b2Vec2 := bodyA.m_linearVelocity.all;
            vB : b2Vec2 := bodyB.m_linearVelocity.all;

            invMassA : float32 := bodyA.m_invMass.all;
            invIA    : float32 := bodyA.m_invI.all;
            invMassB : float32 := bodyB.m_invMass.all;
            invIB    : float32 := bodyB.m_invI.all;
            normal   : constant b2Vec2 := c.normal;
            tangent  : constant b2Vec2 := b2Cross (normal, 1.0);
            friction : float32 := c.friction;

            pragma Assert (c.pointCount = 1 or else c.pointCount = 2);
         begin

            --  Solve tangent constraints
            for j in 1 .. c.pointCount loop
               declare
                  ccp         : b2ContactConstraintPoint renames c.points (uint32 (j));

                  --  Relative velocity at contact
                  dv          : constant b2Vec2  :=   vB + b2Cross (wB, ccp.rB)
                                           - vA - b2Cross (wA, ccp.rA);

                  --  Compute tangent force
                  vt          : constant float32 := b2Dot (dv, tangent);
                  lambda      : float32 := ccp.tangentMass * (-vt);

                  --  b2Clamp the accumulated force
                  maxFriction : constant float32 := friction * ccp.normalImpulse;
                  newImpulse  : constant float32 := b2Clamp (ccp.tangentImpulse + lambda,  -maxFriction, maxFriction);

                  P           : b2Vec2;
               begin

                  lambda := newImpulse - ccp.tangentImpulse;

                  --  Apply contact impulse
                  P := lambda * tangent;

                  vA := vA - invMassA * P;
                  wA := wA - invIA * b2Cross (ccp.rA, P);

                  vB := vB + invMassB * P;
                  wB := wB + invIB * b2Cross (ccp.rB, P);

                  ccp.tangentImpulse := newImpulse;
               end;
            end loop;

            --  Solve normal constraints
            if c.pointCount = 1 then
               declare
                  ccp        : b2ContactConstraintPoint renames c.points (1);

                  --  Relative velocity at contact
                  dv         : constant b2Vec2  :=   vB + b2Cross (wB, ccp.rB)
                                          - vA - b2Cross (wA, ccp.rA);

                  --  Compute normal impulse
                  vn         : constant float32 := b2Dot (dv, normal);
                  lambda     : float32 := -ccp.normalMass * (vn - ccp.velocityBias);

                  --  b2Clamp the accumulated impulse
                  newImpulse : constant float32 := float32'Max (ccp.normalImpulse + lambda, 0.0);

                  P          : b2Vec2;
               begin

                  lambda := newImpulse - ccp.normalImpulse;

                  --  Apply contact impulse
                  P := lambda * normal;
                  vA := vA - invMassA * P;
                  wA := wA - invIA * b2Cross (ccp.rA, P);

                  vB := vB + invMassB * P;
                  wB := wB + invIB * b2Cross (ccp.rB, P);
                  ccp.normalImpulse := newImpulse;
               end;

            else
               --  Block solver developed in collaboration with Dirk Gregorius (back in 01/07 on Box2D_Lite).
               --  Build the mini LCP for this contact patch
               --
               --  vn = A * x + b, vn >= 0, , vn >= 0, x >= 0 and vn_i * x_i = 0 with i = 1..2
               --
               --  A = J * W * JT and J = ( -n, -r1 x n, n, r2 x n )
               --  b = vn_0 - velocityBias
               --
               --  The system is solved using the "Total enumeration method" (s. Murty). The complementary constraint vn_i * x_i
               --  implies that we must have in any solution either vn_i = 0 or x_i = 0. So for the 2D contact problem the cases
               --  vn1 = 0 and vn2 = 0, x1 = 0 and x2 = 0, x1 = 0 and vn2 = 0, x2 = 0 and vn1 = 0 need to be tested. The first valid
               --  solution that satisfies the problem is chosen.
               --
               --  In order to account of the accumulated impulse 'a' (because of the iterative nature of the solver which only requires
               --  that the accumulated impulse is clamped and not the incremental impulse) we change the impulse variable (x_i).
               --
               --  Substitute:
               --
               --  x = x' - a
               --
               --  Plug into above equation:
               --
               --  vn = A * x + b
               --    = A * (x' - a) + b
               --    = A * x' + b - A * a
               --    = A * x' + b'
               --  b' = b - A * a;
               declare

                  cp1 : b2ContactConstraintPoint renames c.points (1);
                  cp2 : b2ContactConstraintPoint renames c.points (2);

                  a   : b2Vec2  := (cp1.normalImpulse,  cp2.normalImpulse);
                  pragma Assert (a.x >= 0.0 and then a.y >= 0.0);

                  --  Relative velocity at contact
                  dv1 : constant b2Vec2  := vB + b2Cross (wB, cp1.rB) - vA - b2Cross (wA, cp1.rA);
                  dv2 : constant b2Vec2  := vB + b2Cross (wB, cp2.rB) - vA - b2Cross (wA, cp2.rA);

                  --  Compute normal velocity
                  vn1 : float32 := b2Dot (dv1, normal);
                  vn2 : float32 := b2Dot (dv2, normal);

                  b   : b2Vec2;

                  k_errorTol : constant := 1.0e-3;

                  x, d,
                  P1, P2 : b2Vec2;
               begin
                  b.x := vn1 - cp1.velocityBias;
                  b.y := vn2 - cp2.velocityBias;
                  b   := b   - b2Mul (c.K, a);

                  loop
                     --
                     --  Case 1: vn = 0
                     --
                     --  0 = A * x' + b'
                     --
                     --  Solve for x':
                     --
                     --  x' = - inv(A) * b'
                     --
                     x := -b2Mul (c.normalMass, b);

                     if x.x >= 0.0 and then x.y >= 0.0 then
                        --  Resubstitute for the incremental impulse
                        d := x - a;

                        --  Apply incremental impulse
                        P1 := d.x * normal;
                        P2 := d.y * normal;
                        vA := vA - invMassA * (P1 + P2);
                        wA := wA - invIA * (b2Cross (cp1.rA, P1) + b2Cross (cp2.rA, P2));

                        vB := vB + invMassB * (P1 + P2);
                        wB := wB + invIB * (b2Cross (cp1.rB, P1) + b2Cross (cp2.rB, P2));

                        --  Accumulate
                        cp1.normalImpulse := x.x;
                        cp2.normalImpulse := x.y;

                        --  #if B2_DEBUG_SOLVER == 1
                        --                          -- Postconditions
                        --                          dv1 = vB + b2Cross(wB, cp1->rB) - vA - b2Cross(wA, cp1->rA);
                        --                          dv2 = vB + b2Cross(wB, cp2->rB) - vA - b2Cross(wA, cp2->rA);
                        --
                        --                          -- Compute normal velocity
                        --                          vn1 = b2Dot(dv1, normal);
                        --                          vn2 = b2Dot(dv2, normal);
                        --
                        --                          b2Assert(b2Abs(vn1 - cp1->velocityBias) < k_errorTol);
                        --                          b2Assert(b2Abs(vn2 - cp2->velocityBias) < k_errorTol);
                        --  #endif
                        exit;
                     end if;

                     --
                     --  Case 2: vn1 = 0 and x2 = 0
                     --
                     --   0 = a11 * x1' + a12 * 0 + b1'
                     --  vn2 = a21 * x1' + a22 * 0 + b2'
                     --
                     x.x := -cp1.normalMass * b.x;
                     x.y := 0.0;
                     vn1 := 0.0;
                     vn2 := c.K.col1.y * x.x + b.y;

                     if x.x >= 0.0 and then vn2 >= 0.0 then
                        --  Resubstitute for the incremental impulse
                        d  := x - a;

                        --  Apply incremental impulse
                        P1 := d.x * normal;
                        P2 := d.y * normal;
                        vA := vA - invMassA * (P1 + P2);
                        wA := wA - invIA * (b2Cross (cp1.rA, P1) + b2Cross (cp2.rA, P2));

                        vB := vB + invMassB * (P1 + P2);
                        wB := wB + invIB * (b2Cross (cp1.rB, P1) + b2Cross (cp2.rB, P2));

                        --  Accumulate
                        cp1.normalImpulse := x.x;
                        cp2.normalImpulse := x.y;

                        --  #if B2_DEBUG_SOLVER == 1
                        --                          -- Postconditions
                        --                          dv1 = vB + b2Cross(wB, cp1->rB) - vA - b2Cross(wA, cp1->rA);
                        --
                        --                          -- Compute normal velocity
                        --                          vn1 = b2Dot(dv1, normal);
                        --
                        --                          b2Assert(b2Abs(vn1 - cp1->velocityBias) < k_errorTol);
                        --  #endif
                        exit;
                     end if;


                     --
                     --  Case 3: vn2 = 0 and x1 = 0
                     --
                     --  vn1 = a11 * 0 + a12 * x2' + b1'
                     --   0 = a21 * 0 + a22 * x2' + b2'
                     --
                     x.x := 0.0;
                     x.y := -cp2.normalMass * b.y;
                     vn1 := c.K.col2.x * x.y + b.x;
                     vn2 := 0.0;

                     if x.y >= 0.0 and then vn1 >= 0.0 then
                        --  Resubstitute for the incremental impulse
                        d  := x - a;

                        --  Apply incremental impulse
                        P1 := d.x * normal;
                        P2 := d.y * normal;
                        vA := vA - invMassA * (P1 + P2);
                        wA := wA - invIA * (b2Cross (cp1.rA, P1) + b2Cross (cp2.rA, P2));

                        vB := vB + invMassB * (P1 + P2);
                        wB := wB + invIB * (b2Cross (cp1.rB, P1) + b2Cross (cp2.rB, P2));

                        --  Accumulate
                        cp1.normalImpulse := x.x;
                        cp2.normalImpulse := x.y;

                        --  #if B2_DEBUG_SOLVER == 1
                        --                          -- Postconditions
                        --                          dv2 = vB + b2Cross(wB, cp2->rB) - vA - b2Cross(wA, cp2->rA);
                        --
                        --                          -- Compute normal velocity
                        --                          vn2 = b2Dot(dv2, normal);
                        --
                        --                          b2Assert(b2Abs(vn2 - cp2->velocityBias) < k_errorTol);
                        --  #endif
                        exit;
                     end if;

                     --
                     --  Case 4: x1 = 0 and x2 = 0
                     --
                     --  vn1 = b1
                     --  vn2 = b2;
                     x.x := 0.0;
                     x.y := 0.0;
                     vn1 := b.x;
                     vn2 := b.y;

                     if vn1 >= 0.0 and then vn2 >= 0.0 then
                        --  Resubstitute for the incremental impulse
                        d  := x - a;

                        --  Apply incremental impulse
                        P1 := d.x * normal;
                        P2 := d.y * normal;
                        vA := vA - invMassA * (P1 + P2);
                        wA := wA - invIA * (b2Cross (cp1.rA, P1) + b2Cross (cp2.rA, P2));

                        vB := vB + invMassB * (P1 + P2);
                        wB := wB + invIB * (b2Cross (cp1.rB, P1) + b2Cross (cp2.rB, P2));

                        --  Accumulate
                        cp1.normalImpulse := x.x;
                        cp2.normalImpulse := x.y;

                        exit;
                     end if;

                     --  No solution, give up. This is hit sometimes, but it doesn't seem to matter.
                     exit;
                  end loop;
               end;
            end if;

            bodyA.m_linearVelocity.all  := vA;
            bodyA.m_angularVelocity.all := wA;
            bodyB.m_linearVelocity.all  := vB;
            bodyB.m_angularVelocity.all := wB;
         end;
      end loop;

   end SolveVelocityConstraints;





   procedure StoreImpulses (Self : in out b2ContactSolver)
   is
   begin
      for i in 1 .. Self.m_constraintCount loop
         declare
            c : b2ContactConstraint  renames Self.m_constraints (i);
            m : collision.b2Manifold renames c.manifold.all;
         begin
            for j in 1 .. c.pointCount loop
               m.points (j).normalImpulse  := c.points (uint32 (j)).normalImpulse;
               m.points (j).tangentImpulse := c.points (uint32 (j)).tangentImpulse;
            end loop;
         end;
      end loop;
   end StoreImpulses;















   type b2PositionSolverManifold is
      record
         normal     : b2Vec2;
         point      : b2Vec2;
         separation : float32;
      end record;



   procedure Initialize (Self : in out b2PositionSolverManifold;   cc    : access b2ContactConstraint;
                                                                   index : in     int32)
   is
      use Collision;

      pointA,
      pointB,
      planePoint,
      clipPoint : b2Vec2;
   begin
      pragma Assert (cc.pointCount > 0);

      case cc.kind is
      when e_circles =>
         pointA := cc.bodyA.GetWorldPoint (cc.localPoint);
         pointB := cc.bodyB.GetWorldPoint (cc.points (1).localPoint);

         if b2DistanceSquared (pointA, pointB)  >  b2_epsilon * b2_epsilon then
            Self.normal  := pointB - pointA;
            Normalize (Self.normal);
         else
            Self.normal  := (1.0, 0.0);
         end if;

         Self.point      := 0.5 * (pointA + pointB);
         Self.separation := b2Dot (pointB - pointA, Self.normal) - cc.radius;

      when e_faceA =>
         Self.normal     := cc.bodyA.GetWorldVector (cc.localNormal);
         planePoint      := cc.bodyA.GetWorldPoint  (cc.localPoint);

         clipPoint       := cc.bodyB.GetWorldPoint (cc.points (uint32 (index)).localPoint);
         Self.separation := b2Dot (clipPoint - planePoint, Self.normal) - cc.radius;
         Self.point      := clipPoint;

      when e_faceB =>
         Self.normal     := cc.bodyB.GetWorldVector (cc.localNormal);
         planePoint      := cc.bodyB.GetWorldPoint  (cc.localPoint);

         clipPoint       := cc.bodyA.GetWorldPoint (cc.points (uint32 (index)).localPoint);
         Self.separation := b2Dot (clipPoint - planePoint, Self.normal) - cc.radius;
         Self.point      := clipPoint;

         --  Ensure normal points from A to B
         Self.normal     := -Self.normal;
      end case;

   end Initialize;






   function SolvePositionConstraints  (Self : in     b2ContactSolver;   baumgarte : float32) return Boolean
   is
      minSeparation : float32 := 0.0;
      psm           : b2PositionSolverManifold;
   begin
      for i in 1 .. Self.m_constraintCount loop
         declare
            c        : constant access b2ContactConstraint := Self.m_constraints (i)'Access;

            bodyA    : constant Solid_view := c.bodyA;
            bodyB    : constant Solid_view := c.bodyB;

            invMassA : float32    := bodyA.getMass * bodyA.m_invMass.all;
            invIA    : float32    := bodyA.getMass * bodyA.m_invI.all;
            invMassB : float32    := bodyB.getMass * bodyB.m_invMass.all;
            invIB    : float32    := bodyB.getMass * bodyB.m_invI.all;

         begin

            --  Solve normal constraints
            for j in 1 .. c.pointCount loop
               Initialize (psm,  c, j);
               declare
                  normal     : constant b2Vec2  := psm.normal;

                  point      : constant b2Vec2  := psm.point;
                  separation : constant float32 := psm.separation;

                  rA         : constant b2Vec2  := point - bodyA.m_sweep.c;
                  rB         : constant b2Vec2  := point - bodyB.m_sweep.c;


                  --  Prevent large corrections and allow slop.
                  C          : constant float32 := b2Clamp (baumgarte * (separation + b2_linearSlop),  -b2_maxLinearCorrection, 0.0);

                  --  Compute the effective mass.
                  rnA        : constant float32 := b2Cross (rA, normal);
                  rnB        : constant float32 := b2Cross (rB, normal);
                  K          : constant float32 := invMassA + invMassB + invIA * rnA * rnA + invIB * rnB * rnB;

                  --  Compute normal impulse
                  impulse : float32;

                  P       : b2Vec2;
               begin
                  --  Track max constraint error.
                  minSeparation := float32'Min (minSeparation, separation);

                  if K > 0.0 then   impulse := -C / K;
                  else              impulse := 0.0;
                  end if;

                  P := impulse * normal;

                  bodyA.m_sweep.c := bodyA.m_sweep.c - invMassA * P;
                  bodyA.m_sweep.a := bodyA.m_sweep.a - invIA * b2Cross (rA, P);
                  bodyA.SynchronizeTransform;

                  bodyB.m_sweep.c := bodyB.m_sweep.c + invMassB * P;
                  bodyB.m_sweep.a := bodyB.m_sweep.a + invIB * b2Cross (rB, P);
                  bodyB.SynchronizeTransform;
               end;
            end loop;
         end;
      end loop;

      --  We can't expect minSpeparation >= -b2_linearSlop because we don't
      --  push the separation above -b2_linearSlop.
      return minSeparation >= -1.5 * b2_linearSlop;
   end SolvePositionConstraints;



end impact.d2.orbs.contact.Solver;
