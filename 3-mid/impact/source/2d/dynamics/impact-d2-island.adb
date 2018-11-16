with impact.d2.Fixture,
     ada.unchecked_Deallocation;


package body impact.d2.Island
is

   --  Position Correction Notes
   --  =========================
   --  I tried the several algorithms for position correction of the 2D revolute joint.
   --  I looked at these systems:
   --  - simple pendulum (1m diameter sphere on massless 5m stick) with initial angular velocity of 100 rad/s.
   --  - suspension bridge with 30 1m long planks of length 1m.
   --  - multi-link chain with 30 1m long links.
   --
   --  Here are the algorithms:
   --
   --  Baumgarte - A fraction of the position error is added to the velocity error. There is no
   --  separate position solver.
   --
   --  Pseudo Velocities - After the velocity solver and position integration,
   --  the position error, Jacobian, and effective mass are recomputed. Then
   --  the velocity constraints are solved with pseudo velocities and a fraction
   --  of the position error is added to the pseudo velocity error. The pseudo
   --  velocities are initialized to zero and there is no warm-starting. After
   --  the position solver, the pseudo velocities are added to the positions.
   --  This is also called the First Order World method or the Position LCP method.
   --
   --  Modified Nonlinear Gauss-Seidel (NGS) - Like Pseudo Velocities except the
   --  position error is re-computed for each constraint and the positions are updated
   --  after the constraint is solved. The radius vectors (aka Jacobians) are
   --  re-computed too (otherwise the algorithm has horrible instability). The pseudo
   --  velocity states are not needed because they are effectively zero at the beginning
   --  of each iteration. Since we have the current position error, we allow the
   --  iterations to terminate early if the error becomes smaller than b2_linearSlop.
   --
   --  Full NGS or just NGS - Like Modified NGS except the effective mass are re-computed
   --  each time a constraint is solved.
   --
   --  Here are the results:
   --  Baumgarte - this is the cheapest algorithm but it has some stability problems,
   --  especially with the bridge. The chain links separate easily close to the root
   --  and they jitter as they struggle to pull together. This is one of the most common
   --  methods in the field. The big drawback is that the position correction artificially
   --  affects the momentum, thus leading to instabilities and false bounce. I used a
   --  bias factor of 0.2. A larger bias factor makes the bridge less stable, a smaller
   --  factor makes joints and contacts more spongy.
   --
   --  Pseudo Velocities - the is more stable than the Baumgarte method. The bridge is
   --  stable. However, joints still separate with large angular velocities. Drag the
   --  simple pendulum in a circle quickly and the joint will separate. The chain separates
   --  easily and does not recover. I used a bias factor of 0.2. A larger value lead to
   --  the bridge collapsing when a heavy cube drops on it.
   --
   --  Modified NGS - this algorithm is better in some ways than Baumgarte and Pseudo
   --  Velocities, but in other ways it is worse. The bridge and chain are much more
   --  stable, but the simple pendulum goes unstable at high angular velocities.
   --
   --  Full NGS - stable in all tests. The joints display good stiffness. The bridge
   --  still sags, but this is better than infinite forces.
   --
   --  Recommendations
   --  Pseudo Velocities are not really worthwhile because the bridge and chain cannot
   --  recover from joint separation. In other cases the benefit over Baumgarte is small.
   --
   --  Modified NGS is not a robust method for the revolute joint due to the violent
   --  instability seen in the simple pendulum. Perhaps it is viable with other constraint
   --  types, especially scalar constraints where the effective mass is a scalar.
   --
   --  This leaves Baumgarte and Full NGS. Baumgarte has small, but manageable instabilities
   --  and is very fast. I don't think we can escape Baumgarte, especially in highly
   --  demanding cases where high constraint fidelity is not needed.
   --
   --  Full NGS is robust and easy on the eyes. I recommend this as an option for
   --  higher fidelity simulation and certainly for suspension bridges and long chains.
   --  Full NGS might be a good choice for ragdolls, especially motorized ragdolls where
   --  joint separation can be problematic. The number of NGS iterations can be reduced
   --  for better performance without harming robustness much.
   --
   --  Each joint in a can be handled differently in the position solver. So I recommend
   --  a system where the user can select the algorithm on a per joint basis. I would
   --  probably default to the slower Full NGS and let the user select the faster
   --  Baumgarte method in performance critical scenarios.
   --
   --
   --
   --  Cache Performance
   --
   --  The Box2D solvers are dominated by cache misses. Data structures are designed
   --  to increase the number of cache hits. Much of misses are due to random access
   --  to body data. The constraint structures are iterated over linearly, which leads
   --  to few cache misses.
   --
   --  The bodies are not accessed during iteration. Instead read only data, such as
   --  the mass values are stored with the constraints. The mutable data are the constraint
   --  impulses and the bodies velocities/positions. The impulses are held inside the
   --  constraint structures. The body velocities/positions are held in compact, temporary
   --  arrays to increase the number of cache hits. Linear and angular velocity are
   --  stored in a single array since multiple arrays lead to multiple misses.
   --
   --
   --
   --  2D Rotation
   --
   --  R = [cos(theta) -sin(theta)]
   --      [sin(theta) cos(theta) ]
   --
   --  thetaDot = omega
   --
   --  Let q1 = cos(theta), q2 = sin(theta).
   --  R = [q1 -q2]
   --      [q2  q1]
   --
   --  q1Dot = -thetaDot * q2
   --  q2Dot = thetaDot * q1
   --
   --  q1_new = q1_old - dt * w * q2
   --  q2_new = q2_old + dt * w * q1
   --  then normalize.
   --
   --  This might be faster than computing sin+cos.
   --  However, we can compute sin+cos of the same angle fast.
   --


   use type int32;




   function to_b2Island (bodyCapacity    : int32;
                         contactCapacity : int32;
                         jointCapacity   : int32;
                         --                          b2StackAllocator* allocator,
                         listener        : access world_callbacks.b2ContactListener'Class) return b2Island
   is
      Self : b2Island;
   begin
      Self.m_bodyCapacity    := bodyCapacity;
      Self.m_contactCapacity := contactCapacity;
      Self.m_jointCapacity   := jointCapacity;

      Self.m_bodyCount       := 0;
      Self.m_contactCount    := 0;
      Self.m_jointCount      := 0;

      --          Self.m_allocator  := allocator;
      Self.m_listener   := listener;

      Self.m_bodies     := new Solid_views   (1 .. bodyCapacity);
      Self.m_contacts   := new Contact.views (1 .. contactCapacity);
      Self.m_joints     := new Joint_views   (1 .. jointCapacity);

      Self.m_velocities := new Velocity_views (1 .. bodyCapacity);
      Self.m_positions  := new Position_views (1 .. bodyCapacity);

      return Self;
   end to_b2Island;






   procedure destruct (Self : in out b2Island)
   is
      procedure free is new ada.unchecked_Deallocation (Solid_views,   access_Solid_views);
      procedure free is new ada.unchecked_Deallocation (Joint_views,   access_Joint_views);
      procedure free is new ada.unchecked_Deallocation (Contact.views, access_Contact_views);

      procedure free is new ada.unchecked_Deallocation (Position_views, access_Position_views);
      procedure free is new ada.unchecked_Deallocation (Velocity_views, access_Velocity_views);
   begin
      --  Warning: the order should reverse the constructor order.
      free (Self.m_positions);
      free (Self.m_velocities);
      free (Self.m_joints);
      free (Self.m_contacts);
      free (Self.m_bodies);
   end destruct;






   procedure Clear    (Self : in out b2Island)
   is
   begin
      Self.m_bodyCount    := 0;
      Self.m_contactCount := 0;
      Self.m_jointCount   := 0;
   end Clear;








   procedure Add (Self : in out b2Island;   Solid : access impact.d2.Solid.b2Body'Class)
   is
   begin
      pragma Assert (Self.m_bodyCount < Self.m_bodyCapacity);

      Self.m_bodyCount                 := Self.m_bodyCount + 1;
      Self.m_bodies (Self.m_bodyCount) := Solid.all'Access;

      Solid.m_islandIndex_is (Self.m_bodyCount);
   end Add;




   procedure Add (Self : in out b2Island;   Contact     : access impact.d2.Contact.b2Contact'Class)
   is
   begin
      pragma Assert (Self.m_contactCount < Self.m_contactCapacity);

      Self.m_contactCount                   := Self.m_contactCount + 1;
      Self.m_contacts (Self.m_contactCount) := Contact.all'Access;
   end Add;





   procedure Add (Self : in out b2Island;   Joint       : access impact.d2.Joint.b2Joint'Class)
   is
   begin
      pragma Assert (Self.m_jointCount < Self.m_jointCapacity);

      Self.m_jointCount                 := Self.m_jointCount + 1;
      Self.m_joints (Self.m_jointCount) := Joint.all'Access;
   end Add;





--     procedure Report (Self : in out b2Island;   constraints : access contact.solver.b2ContactConstraints)
--     is
--        c       :                 Contact.view;
--        cc      : access constant contact.Solver.b2ContactConstraint;
--        impulse :                 world_callbacks.b2ContactImpulse;
--     begin
--        if Self.m_listener = null then
--           return;
--        end if;
--
--        for i in 1 .. Self.m_contactCount loop
--           c  := Self.m_contacts (i);
--           cc := constraints (i)'Access;
--
--           for j in 1 .. cc.pointCount loop
--              impulse.normalImpulses  (j) := cc.points (uint32 (j)).normalImpulse;
--              impulse.tangentImpulses (j) := cc.points (uint32 (j)).tangentImpulse;
--           end loop;
--
--           Self.m_listener.PostSolve (c, impulse);
--        end loop;
--     end Report;





--     procedure Solve (Self : in out b2Island;   step       : in b2TimeStep;
--                                                gravity    : in b2Vec2;
--                                                allowSleep : in Boolean)
--     is
--        use type solid.b2BodyType;
--     begin
--        --  Integrate velocities and apply damping.
--        for i in 1 .. Self.m_bodyCount loop
--           declare
--              b : Solid_view renames Self.m_bodies (i);
--           begin
--              if b.GetType = Solid.b2_dynamicBody then
--                 --  Integrate velocities.
--                 b.m_linearVelocity.all  := b.m_linearVelocity.all  + step.dt * (gravity + b.m_invMass.all * b.m_force.all);
--                 b.m_angularVelocity.all := b.m_angularVelocity.all + step.dt * b.m_invI.all * b.m_torque.all;
--
--                 --  Apply damping.
--                 --  ODE: dv/dt + c * v = 0
--                 --  Solution: v(t) = v0 * exp(-c * t)
--                 --  Time step: v(t + dt) = v0 * exp(-c * (t + dt)) = v0 * exp(-c * t) * exp(-c * dt) = v * exp(-c * dt)
--                 --  v2 = exp(-c * dt) * v1
--                 --  Taylor expansion:
--                 --  v2 = (1.0f - c * dt) * v1
--                 --
--                 b.m_linearVelocity.all  := b.m_linearVelocity.all  * b2Clamp (1.0 - step.dt * b.m_linearDamping.all,   0.0, 1.0);
--                 b.m_angularVelocity.all := b.m_angularVelocity.all * b2Clamp (1.0 - step.dt * b.m_angularDamping.all,  0.0, 1.0);
--              end if;
--           end;
--        end loop;
--
--        --  Partition contacts so that contacts with static bodies are solved last.
--        declare
--           i1 : int32 := 0;
--
--           fixtureA,
--           fixtureB : access Fixture.b2Fixture;
--
--           bodyA,
--           bodyB : Solid_view;
--
--           nonStatic : Boolean;
--
--           procedure swap is new swap_any (Contact.view);
--
--        begin
--           for i2 in 1 .. Self.m_contactCount loop
--              fixtureA  := Self.m_contacts (i2).GetFixtureA;
--              fixtureB  := Self.m_contacts (i2).GetFixtureB;
--
--              bodyA     := fixtureA.GetBody.all'Access;
--              bodyB     := fixtureB.GetBody.all'Access;
--
--              nonStatic :=          bodyA.GetType /= solid.b2_staticBody
--                           and then bodyB.GetType /= solid.b2_staticBody;
--
--              if nonStatic then
--                 i1 := i1 + 1;
--                 swap (Self.m_contacts (i1),  Self.m_contacts (i2));
--              end if;
--           end loop;
--        end;
--
--        --  Initialize velocity constraints.
--        declare
--           contactSolver : contact.solver.b2ContactSolver := contact.solver.to_b2ContactSolver (Self.m_contacts (1 .. Self.m_contactCount),
--                                                                                                -- Self.m_allocator,
--                                                                                                step.dtRatio);
--        begin
--           contactSolver.WarmStart;
--
--           for i in 1 .. Self.m_jointCount loop
--              Self.m_joints (i).InitVelocityConstraints (step);
--           end loop;
--
--           --  Solve velocity constraints.
--           for i in 1 .. step.velocityIterations loop
--              for j in 1 .. Self.m_jointCount loop
--                 Self.m_joints (j).SolveVelocityConstraints (step);
--              end loop;
--
--              contactSolver.SolveVelocityConstraints;
--           end loop;
--
--           --  Post-solve (store impulses for warm starting).
--           contactSolver.StoreImpulses;
--
--           --  Integrate positions.
--           for i in 1 .. Self.m_bodyCount loop
--              declare
--                 b           : constant Solid_view := Self.m_bodies (i);
--
--                 translation : b2Vec2;
--
--                 rotation,
--                 ratio       : float32;
--              begin
--
--                 if b.GetType /= solid.b2_staticBody then
--
--                    --  Check for large velocities.
--                    translation := step.dt * b.m_linearVelocity.all;
--
--                    if b2Dot (translation, translation) > b2_maxTranslationSquared then
--                       ratio                  := b2_maxTranslation / Length (translation);
--                       b.m_linearVelocity.all := b.m_linearVelocity.all * ratio;
--                    end if;
--
--                    rotation := step.dt * b.m_angularVelocity.all;
--
--                    if rotation * rotation > b2_maxRotationSquared then
--                       ratio                   := b2_maxRotation / abs (rotation);
--                       b.m_angularVelocity.all := b.m_angularVelocity.all * ratio;
--                    end if;
--
--                    --  Store positions for continuous collision.
--                    b.m_sweep.c0 := b.m_sweep.c;
--                    b.m_sweep.a0 := b.m_sweep.a;
--
--                    --  Integrate
--                    b.m_sweep.c := b.m_sweep.c + step.dt * b.m_linearVelocity.all;
--                    b.m_sweep.a := b.m_sweep.a + step.dt * b.m_angularVelocity.all;
--
--                    --  Compute new transform
--                    b.SynchronizeTransform;
--
--                    --  Note: shapes are synchronized later.
--                 end if;
--              end;
--           end loop;
--
--           --  Iterate over constraints.
--           for i in 1 .. step.positionIterations loop
--              declare
--                 contactsOkay : constant Boolean := contactSolver.SolvePositionConstraints (b2_contactBaumgarte);
--                 jointsOkay   : Boolean := True;
--                 jointOkay    : Boolean;
--              begin
--                 for i in 1 .. Self.m_jointCount loop
--                    jointOkay  := Self.m_joints (i).SolvePositionConstraints (b2_contactBaumgarte);
--                    jointsOkay := jointsOkay and then jointOkay;
--                 end loop;
--
--                 if contactsOkay and then jointsOkay then
--                    exit;    -- Exit early if the position errors are small.
--                 end if;
--              end;
--           end loop;
--
--           Self.Report (contactSolver.m_constraints);
--        end;
--
--        if allowSleep then
--           declare
--              minSleepTime : float32 := b2_maxFloat;
--
--              linTolSqr : constant float32 := b2_linearSleepTolerance  * b2_linearSleepTolerance;
--              angTolSqr : constant float32 := b2_angularSleepTolerance * b2_angularSleepTolerance;
--           begin
--
--              for i in 1 .. Self.m_bodyCount loop
--                 declare
--                    use type solid.Flag;
--                    b : constant Solid_view := Self.m_bodies (i);
--                 begin
--                    if b.GetType /= solid.b2_staticBody then
--
--                       if (b.m_flags and Solid.e_autoSleepFlag) = 0 then
--                          b.m_sleepTime.all := 0.0;
--                          minSleepTime      := 0.0;
--                       end if;
--
--                       if       (b.m_flags and Solid.e_autoSleepFlag)                   = 0
--                         or else b.m_angularVelocity.all * b.m_angularVelocity.all      > angTolSqr
--                         or else b2Dot (b.m_linearVelocity.all, b.m_linearVelocity.all) > linTolSqr
--                       then
--                          b.m_sleepTime.all := 0.0;
--                          minSleepTime      := 0.0;
--                       else
--                          b.m_sleepTime.all := b.m_sleepTime.all + step.dt;
--                          minSleepTime      := float32'Min (minSleepTime, b.m_sleepTime.all);
--                       end if;
--
--                    end if;
--                 end;
--              end loop;
--
--              if minSleepTime >= b2_timeToSleep then
--                 for i in 1 .. Self.m_bodyCount loop
--                    Self.m_bodies (i).SetAwake (False);
--                 end loop;
--              end if;
--           end;
--        end if;
--
--     end Solve;




--     procedure SolveTOI (Self : in out b2Island;   subStep    : in b2TimeStep;
--                                                   toiIndexA,
--                                                   toiIndexB  : in int32)
--     is
--        pragma assert (toiIndexA < Self.m_bodyCount);
--        pragma assert (toiIndexB < Self.m_bodyCount);
--
--        use contact.Solver;
--
--        contactSolverDef : b2ContactSolverDef;
--        contactSolver    : b2ContactSolver;
--
--        h                : float32;
--
--     begin
--        -- Initialize the body state.
--        for i in 0 .. Self.m_bodyCount - 1
--        loop
--           declare
--              b : constant Solid_view := Self.m_bodies (i);
--           begin
--              Self.m_positions  (i).c := b.m_sweep.c;
--              Self.m_positions  (i).a := b.m_sweep.a;
--              Self.m_velocities (i).v := b.m_linearVelocity.all;
--              Self.m_velocities (i).w := b.m_angularVelocity.all;
--           end;
--        end loop;
--
--        contactSolverDef.contacts   := Self.m_contacts;
--        contactSolverDef.count      := Self.m_contactCount;
--        --        contactSolverDef.allocator = m_allocator;
--        contactSolverDef.step       := subStep;
--        contactSolverDef.positions  := Self.m_positions;
--        contactSolverDef.velocities := Self.m_velocities;
--
--        contactSolver := to_b2ContactSolver (contactSolverDef'Access);
--
--        -- Solve position constraints.
--        for i in 0 .. subStep.positionIterations - 1
--        loop
--           declare
--               contactsOkay : Boolean := contactSolver.SolveTOIPositionConstraints (toiIndexA, toiIndexB);
--           begin
--              if contactsOkay then
--                 exit;
--              end if;
--           end;
--        end loop;
--
--        --  #if 0
--        --        -- Is the new position really safe?
--        --        for (int32 i = 0; i < Self.m_contactCount; ++i)
--        --        {
--        --                b2Contact* c = Self.m_contacts (i);
--        --                b2Fixture* fA = c.GetFixtureA();
--        --                b2Fixture* fB = c.GetFixtureB();
--        --
--        --                b2Body* bA = fA.GetBody();
--        --                b2Body* bB = fB.GetBody();
--        --
--        --                int32 indexA = c.GetChildIndexA();
--        --                int32 indexB = c.GetChildIndexB();
--        --
--        --                b2DistanceInput input;
--        --                input.proxyA.Set(fA.GetShape(), indexA);
--        --                input.proxyB.Set(fB.GetShape(), indexB);
--        --                input.transformA = bA.GetTransform();
--        --                input.transformB = bB.GetTransform();
--        --                input.useRadii = false;
--        --
--        --                b2DistanceOutput output;
--        --                b2SimplexCache cache;
--        --                cache.count = 0;
--        --                b2Distance(&output, &cache, &input);
--        --
--        --                if (output.distance == 0 || cache.count == 3)
--        --                {
--        --                        cache.count += 0;
--        --                }
--        --        }
--        --  #endif
--
--        -- Leap of faith to new safe state.
--        Self.m_bodies (toiIndexA).m_sweep.c0 := Self.m_positions (toiIndexA).c;
--        Self.m_bodies (toiIndexA).m_sweep.a0 := Self.m_positions (toiIndexA).a;
--        Self.m_bodies (toiIndexB).m_sweep.c0 := Self.m_positions (toiIndexB).c;
--        Self.m_bodies (toiIndexB).m_sweep.a0 := Self.m_positions (toiIndexB).a;
--
--        -- No warm starting is needed for TOI events because warm
--        -- starting impulses were applied in the discrete solver.
--        --
--        contactSolver.InitializeVelocityConstraints;
--
--        -- Solve velocity constraints.
--        for i in 0 .. subStep.velocityIterations - 1
--        loop
--           contactSolver.SolveVelocityConstraints;
--        end loop
--
--        -- Don't store the TOI contact forces for warm starting
--        -- because they can be quite large.
--
--        h := subStep.dt;
--
--        -- Integrate positions
--        for i in 0 .. Self.m_bodyCount - 1
--        loop
--           declare
--              c : b2Vec2  := Self.m_positions  (i).c;
--              a : float32 := Self.m_positions  (i).a;
--              v : b2Vec2  := Self.m_velocities (i).v;
--              w : float32 := Self.m_velocities (i).w;
--
--              translation : b2Vec2 := h * v;
--              ratio,
--              rotation    : float32;
--
--              bod         : Solid_view;
--           begin
--              -- Check for large velocities
--              if b2Dot (translation, translation) > b2_maxTranslationSquared
--              then
--                 ratio := b2_maxTranslation / translation.Length;
--                 v     := v * ratio;
--              end if;
--
--              rotation := h * w;
--
--              if rotation * rotation > b2_maxRotationSquared
--              then
--                 ratio := b2_maxRotation / b2Abs (rotation);
--                 w     := w * ratio;
--              end if;
--
--              -- Integrate
--              c := c + h * v;
--              a := a + h * w;
--
--              Self.m_positions  (i).c := c;
--              Self.m_positions  (i).a := a;
--              Self.m_velocities (i).v := v;
--              Self.m_velocities (i).w := w;
--
--              -- Sync bodies
--              bod := m_bodies (i);
--
--              bod.m_sweep.c := c;
--              bod.m_sweep.a := a;
--
--              bod.m_linearVelocity  := v;
--              bod.m_angularVelocity := w;
--
--              bod.SynchronizeTransform;
--           end;
--        end loop;
--
--        Self.Report (contactSolver.m_velocityConstraints);
--     end;





end impact.d2.Island;
