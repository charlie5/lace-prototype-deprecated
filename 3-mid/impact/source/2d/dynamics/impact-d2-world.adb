with impact.d2.Fixture,
     impact.d2.Island,
     impact.d2.Time_of_impact,
     impact.d2.toi_Solver,
     impact.d2.Distance,
     impact.d2.Broadphase,

     ada.unchecked_Deallocation;



package body impact.d2.World
is

   use type int32, uint32;
   use type impact.d2.Joint.b2JointEdge_view;



   type Solid_view is access all Solid.b2Body'Class;
   procedure free is new ada.unchecked_Deallocation (Solid.b2Body'Class, Solid_view);


   type        Solid_views is array (int32 range <>) of Solid_view;
   type access_Solid_views is access all Solid_views;
   procedure free is new ada.Unchecked_Deallocation (Solid_views, access_Solid_views);


--     type Joint_view is access all Joint.b2Joint'Class;
   procedure free is new ada.unchecked_Deallocation (Joint.b2Joint'Class, Joint.view);


   type Contact_view is access all contact.b2Contact'Class;



   --------
   -- Forge
   --

   function to_b2World (gravity : in b2Vec2) return b2World
   is
      Self : b2World;
   begin
      Self.m_bodyCount         := 0;
      Self.m_jointCount        := 0;

      Self.m_warmStarting      := True;
      Self.m_continuousPhysics := True;
      Self.m_subStepping       := False;

      Self.m_stepComplete      := True;

      Self.m_allowSleep        := True;
      Self.m_gravity           := gravity;

      Self.m_flags             := e_clearForces;

      Self.m_inv_dt0           := 0.0;

      Self.m_contactManager := contact.Manager.to_b2ContactManager;

      return Self;
   end to_b2World;



   procedure destruct (Self : in out b2World)
   is
   begin
      null;

--      // Some shapes allocate using b2Alloc.
--      b2Body* b = m_bodyList;
--      while (b)
--      {
--              b2Body* bNext = b->m_next;
--
--              b2Fixture* f = b->m_fixtureList;
--              while (f)
--              {
--                      b2Fixture* fNext = f->m_next;
--                      f->m_proxyCount = 0;
--                      f->Destroy(&m_blockAllocator);
--                      f = fNext;
--              }
--
--              b = bNext;
--      }
   end destruct;





   procedure SetDestructionListener (Self : in out b2World;   listener : access world_callbacks.b2DestructionListener'Class)
   is
   begin
      Self.m_destructionListener := listener;
   end SetDestructionListener;



   procedure SetContactFilter (Self : in out b2World;   filter : access world_callbacks.b2ContactFilter'Class)
   is
   begin
      Self.m_contactManager.m_contactFilter := filter;
   end SetContactFilter;




   procedure SetContactListener (Self : in out b2World;   listener : access world_callbacks.b2ContactListener'Class)
   is
   begin
      Self.m_contactManager.m_contactListener := listener;
   end SetContactListener;




   function CreateBody (Self : access b2World;   def : in solid.b2BodyDef) return access Solid.b2Body'Class
   is
      b : Solid_view;
   begin
      pragma Assert (not Self.IsLocked);

      if Self.IsLocked then
         return null;
      end if;

      b := new solid.b2Body'(Solid.Forge.to_b2Body (def, Self));

      Self.add (b);

      return b;
   end CreateBody;



   procedure add (Self : in out b2World;   the_Object : access impact.d2.Solid.b2Body'Class)
   is
      b : constant Solid_view := Solid_view (the_Object);
   begin
--        b.m_world_is (Self'Unchecked_Access);

      --  Add to world doubly linked list.
      --
      b.m_prev_is (null);
      b.m_next_is (Self.m_bodyList);

      if Self.m_bodyList /= null then
         Self.m_bodyList.m_prev_is (b);
      end if;

      Self.m_bodyList  := b;
      Self.m_bodyCount := Self.m_bodyCount + 1;
   end add;




   procedure DestroyBody (Self : in out b2World;   solid : access impact.d2.Solid.b2Body'Class)
   is
      pragma assert (Self.m_bodyCount > 0);
      pragma assert (not Self.IsLocked);

      use type Fixture.view;

      b       :        Solid_view := solid.all'Access;

      je, je0 : access joint.b2JointEdge;
      ce, ce0 : access contact.b2ContactEdge;
      f,  f0  :        Fixture.view;

   begin
      if Self.IsLocked then
         return;
      end if;

      --  Delete the attached joints.
      je := b.getJointList;

      while je /= null
      loop
         je0 := je;
         je  := je.next;

         if Self.m_destructionListener /= null then
            Self.m_destructionListener.SayGoodbye (je0.joint);
         end if;

         Self.DestroyJoint (je0.joint);
      end loop;

      b.m_jointList_is (null);

      --  Delete the attached contacts.
      ce := b.m_contactList;

      while ce /= null
      loop
         ce0 := ce;
         ce  := ce.next;
         Self.m_contactManager.Destroy (ce0.contact);
      end loop;

      b.m_contactList_is (null);

      --  Delete the attached fixtures. This destroys broad-phase proxies.
      f := b.getFixtureList;

      while f /= null
      loop
         f0 := f;
         f  := f.getNext;

         if Self.m_destructionListener /= null then
            Self.m_destructionListener.SayGoodbye (f0);
         end if;

         f0.DestroyProxy (Self.m_contactManager.m_broadPhase'Access);
         f0.Destroy; -- (&m_blockAllocator);
         f0.destruct; -- ~b2Fixture();
         fixture.free (f0);    -- m_blockAllocator.Free(f0, sizeof(b2Fixture));
      end loop;

      b.m_fixtureList_is (null);
      b.m_fixtureCount_is (0);

      --  Remove world body list.
      if b.m_prev /= null then
         b.m_prev.m_next_is (b.getNext);
      end if;

      if b.getNext /= null then
         b.getNext.m_prev_is (b.m_prev);
      end if;

      if b = Self.m_bodyList then
         Self.m_bodyList := b.getNext;
      end if;

      Self.m_bodyCount := Self.m_bodyCount - 1;
      b.destruct;  -- ~b2Body();
      free (b); -- m_blockAllocator.Free(b, sizeof(b2Body));
   end DestroyBody;




   function CreateJoint (Self : access b2World;   def : in joint.b2JointDef) return access joint.b2Joint'Class
   is
      pragma Assert (not Self.IsLocked);

      use type Joint.view;

      j     :        Joint.view;
      edge  : access contact.b2ContactEdge;
      bodyA,
      bodyB :        Solid_view;

   begin
      if Self.IsLocked then
         return null;
      end if;

      j := Joint.view (Joint.Create (def)); --, &m_blockAllocator);

      --  Connect to the world list.
      j.m_prev_is (null);
      j.m_next_is (Self.m_jointList);

      if Self.m_jointList /= null then
         Self.m_jointList.m_prev_is (j);
      end if;

      Self.m_jointList  := j;
      Self.m_jointCount := Self.m_jointCount + 1;

      --  Connect to the bodies' doubly linked lists.
      j.m_edgeA.joint := j;
      j.m_edgeA.other := j.getBodyB;
      j.m_edgeA.prev  := null;
      j.m_edgeA.next  := j.getBodyA.getJointList;

      if j.getBodyA.getJointList /= null then
         j.getBodyA.getJointList.prev := j.m_edgeA;
      end if;

      j.getBodyA.m_jointList_is (j.m_edgeA);


      j.m_edgeB.joint := j;
      j.m_edgeB.other := j.getBodyA;
      j.m_edgeB.prev  := null;
      j.m_edgeB.next  := j.getBodyB.getJointList;

      if j.getBodyB.getJointList /= null then
         j.getBodyB.getJointList.prev := j.m_edgeB;
      end if;

      j.getBodyB.m_jointList_is (j.m_edgeB);


      bodyA := Solid_view (def.bodyA);
      bodyB := Solid_view (def.bodyB);

      --  If the joint prevents collisions, then flag any contacts for filtering.
      if not def.collideConnected
      then
         edge := bodyB.GetContactList;

         while edge /= null
         loop
            if edge.other = bodyA then
               edge.contact.FlagForFiltering;   -- Flag the contact for filtering at the next time step (where either body is awake).
            end if;

            edge := edge.next;
         end loop;
      end if;

      return j;
   end CreateJoint;






   procedure DestroyJoint (Self : in out b2World;   joint : access impact.d2.Joint.b2Joint'Class)
   is
      pragma assert (not Self.IsLocked);

      use type impact.d2.Joint.view;
      j                : impact.d2.Joint.view := joint.all'Access;

      collideConnected : Boolean;
      bodyA, bodyB     : Solid_view;
      edge             : access contact.b2ContactEdge;

   begin
      if Self.IsLocked then
         return;
      end if;

      collideConnected := j.m_collideConnected;

      --  Remove from the doubly linked list.
      if j.m_prev /= null then
         j.m_prev.m_next_is (j.getNext);
      end if;

      if j.getNext /= null then
         j.getNext.m_prev_is (j.m_prev);
      end if;

      if j = Self.m_jointList then
         Self.m_jointList := j.getNext;
      end if;

      --  Disconnect from island graph.
      bodyA := Solid_view (j.getBodyA);
      bodyB := Solid_view (j.getBodyB);

      --  Wake up connected bodies.
      bodyA.SetAwake (True);
      bodyB.SetAwake (True);

      --  Remove from body 1.
      if j.m_edgeA.prev /= null then
         j.m_edgeA.prev.next := j.m_edgeA.next;
      end if;

      if j.m_edgeA.next /= null then
         j.m_edgeA.next.prev := j.m_edgeA.prev;
      end if;

      if j.m_edgeA = bodyA.getJointList then
         bodyA.m_jointList_is (j.m_edgeA.next);
      end if;

      j.m_edgeA.prev := null;
      j.m_edgeA.next := null;

      --  Remove from body 2
      if j.m_edgeB.prev /= null then
         j.m_edgeB.prev.next := j.m_edgeB.next;
      end if;

      if j.m_edgeB.next /= null then
         j.m_edgeB.next.prev := j.m_edgeB.prev;
      end if;

      if j.m_edgeB = bodyB.getJointList then
         bodyB.m_jointList_is (j.m_edgeB.next);
      end if;

      j.m_edgeB.prev := null;
      j.m_edgeB.next := null;

      impact.d2.Joint.Destroy (j); --, &m_blockAllocator);

      pragma assert (Self.m_jointCount > 0);
      Self.m_jointCount := Self.m_jointCount - 1;

      --  If the joint prevents collisions, then flag any contacts for filtering.
      if not collideConnected
      then
         edge := bodyB.GetContactList;
         while edge /= null
         loop
            if edge.other = bodyA then
               edge.contact.FlagForFiltering;  -- Flag the contact for filtering at the next time step (where either body is awake).
            end if;

            edge := edge.next;
         end loop;
      end if;

   end DestroyJoint;









   function GetBodyList (Self : in b2World) return access Solid.b2Body'Class
   is
   begin
      return Self.m_bodyList;
   end GetBodyList;



   function GetJointList (Self : in b2World) return access Joint.b2Joint'Class
   is
   begin
      return Self.m_jointList;
   end GetJointList;



   function GetContactList (Self : in b2World) return access Contact.b2Contact'Class
   is
   begin
      return Self.m_contactManager.m_contactList;
   end GetContactList;



   function GetProxyCount (Self : in b2World) return int32
   is
   begin
      return Self.m_contactManager.m_broadPhase.GetProxyCount;
   end GetProxyCount;



   function GetBodyCount (Self : in b2World) return int32
   is
   begin
      return Self.m_bodyCount;
   end GetBodyCount;



   function GetJointCount (Self : in b2World) return int32
   is
   begin
      return Self.m_jointCount;
   end GetJointCount;



   function GetContactCount (Self : in b2World) return int32
   is
   begin
      return Self.m_contactManager.m_contactCount;
   end GetContactCount;



   function GetTreeHeight (Self : in b2World) return int32
   is
   begin
      return Self.m_contactManager.m_broadPhase.GetTreeHeight;
   end GetTreeHeight;



   function GetTreeBalance (Self : in b2World) return int32
   is
   begin
      return Self.m_contactManager.m_broadPhase.GetTreeBalance;
   end GetTreeBalance;



   function GetTreeQuality (Self : in b2World) return float32
   is
   begin
      return Self.m_contactManager.m_broadPhase.GetTreeQuality;
   end GetTreeQuality;









   procedure SetGravity (Self : in out b2World;   gravity : in b2Vec2)
   is
   begin
      Self.m_gravity := gravity;
   end SetGravity;


   function GetGravity (Self : in b2World) return b2Vec2
   is
   begin
      return Self.m_gravity;
   end GetGravity;



   function IsLocked (Self : in    b2World) return Boolean
   is
   begin
      return (Self.m_flags and e_locked) = e_locked;
   end IsLocked;







   procedure SetAutoClearForces (Self : in out b2World;   flag : in Boolean)
   is
   begin
      if flag then
         Self.m_flags := Self.m_flags or e_clearForces;
      else
         Self.m_flags := Self.m_flags and not e_clearForces;
      end if;
   end SetAutoClearForces;



   function GetAutoClearForces (Self : in b2World) return Boolean
   is
   begin
      return (Self.m_flags and e_clearForces) = e_clearForces;
   end GetAutoClearForces;





   procedure SetAllowSleeping (Self : in out b2World;   flag : in Boolean)
   is
      use Impact.d2.Solid;
      b : access b2Body;
   begin
      if flag = Self.m_allowSleep then
         return;
      end if;

      Self.m_allowSleep := flag;

      if not Self.m_allowSleep
      then
         b := Self.m_bodyList;

         while b /= null
         loop
            b.SetAwake (True);
            b := b.GetNext;
         end loop;
      end if;
   end SetAllowSleeping;





   procedure SetWarmStarting (Self : in out b2World;   flag : in Boolean)
   is
   begin
      Self.m_warmStarting := flag;
   end SetWarmStarting;



   procedure SetContinuousPhysics (Self : in out b2World;   flag : in Boolean)
   is
   begin
      Self.m_continuousPhysics := flag;
   end SetContinuousPhysics;



   procedure SetSubStepping (Self : in out b2World;   flag : in Boolean)
   is
   begin
      Self.m_subStepping := flag;
   end SetSubStepping;





   --  Find islands, integrate and solve constraints, solve position constraints
   --
   procedure Solve (Self : in out b2World;   step : in b2TimeStep)
   is
      use type uint16, solid.b2BodyType;
      use type Joint.view;

      --  Size the island for the worst case.
      island : impact.d2.Island.b2Island := impact.d2.island.to_b2Island (Self.m_bodyCount,
                                                                  Self.m_contactManager.m_contactCount,
                                                                  Self.m_jointCount,
                                                                  --&m_stackAllocator,
                                                                  Self.m_contactManager.m_contactListener);
      b          : Solid_view;
      c          : Contact_view;
      j          : Joint.view;

      stackSize,
      stackCount : int32;

      stack      : access_Solid_views;
      seed       : Solid_view;

      ce         : access contact.b2ContactEdge;
      je         : access joint.b2JointEdge;
   begin
      --  Clear all the island flags.
      --
      b := Solid_view (Self.m_bodyList);
      while b /= null
      loop
         b.m_flags_are (b.m_flags and not solid.e_islandFlag);
         b := Solid_view (b.getNext);
      end loop;

      c := Contact_view (Self.m_contactManager.m_contactList);
      while c /= null
      loop
         c.m_flags_are (c.m_flags and not contact.e_islandFlag);
         c := Contact_view (c.m_next);
      end loop;

      j := Joint.view (Self.m_jointList);
      while j /= null
      loop
         j.m_islandFlag_is (False);
         j := Joint.view (j.getNext);
      end loop;

      --  Build and simulate all awake islands.
      --
      stackSize := Self.m_bodyCount;
      stack     := new Solid_views (0 .. stackSize - 1);
      seed      := Solid_view (Self.m_bodyList);

      while seed /= null
      loop
         if        (seed.m_flags and solid.e_islandFlag) = 0
           and then not (not seed.IsAwake or else not seed.IsActive)
           and then seed.GetType /= solid.b2_staticBody                    -- The seed can be dynamic or kinematic.
         then
            --  Reset island and stack.
            island.Clear;
            stackCount         := 0;
            stack (stackCount) := seed;
            stackCount         := stackCount + 1;
            seed.m_flags_are (seed.m_flags or solid.e_islandFlag);

            --  Perform a depth first search (DFS) on the constraint graph.
            while stackCount > 0
            loop
               --  Grab the next body off the stack and add it to the island.
               stackCount := stackCount - 1;
               b          := stack (stackCount);   pragma assert (b.IsActive);
               island.Add (b);

               --  Make sure the body is awake.
               b.SetAwake (True);

               --  To keep islands as small as possible, we don't
               --  propagate islands across static bodies.
               if b.GetType /= solid.b2_staticBody
               then
                  --  Search all contacts connected to this body.
                  ce := b.m_contactList;

                  while ce /= null
                  loop
                     declare
                        contact : constant Contact_view := Contact_view (ce.contact);

                        procedure check
                        is
                           sensorA, sensorB : Boolean;
                           other            : Solid_view;
                        begin
                           --  Has this contact already been added to an island?
                           if (contact.m_flags and impact.d2.contact.e_islandFlag) /= 0 then
                              return;
                           end if;

                           --  Is this contact solid and touching?
                           if        not contact.IsEnabled
                             or else not contact.IsTouching
                           then
                              return;
                           end if;

                           --  Skip sensors.
                           sensorA := contact.getFixtureA.isSensor;
                           sensorB := contact.getFixtureB.isSensor;

                           if sensorA or else sensorB then
                              return;
                           end if;

                           island.Add (contact);
                           contact.m_flags_are (contact.m_flags or impact.d2.contact.e_islandFlag);

                           other := Solid_view (ce.other);

                           --  Was the other body already added to this island?
                           if (other.m_flags and solid.e_islandFlag) /= 0 then
                              return;
                           end if;

                           pragma assert (stackCount < stackSize);
                           stack (stackCount) := other;
                           stackCount         := stackCount + 1;
                           other.m_flags_are (other.m_flags or solid.e_islandFlag);
                        end check;

                     begin
                        check;
                     end;

                     ce := ce.next;
                  end loop;


                  --  Search all joints connect to this body.
                  je := b.getJointList;
                  while je /= null
                  loop
                     declare
                        procedure check
                        is
                           other : Solid_view;
                        begin
                           if je.joint.m_islandFlag then
                              return;
                           end if;

                           other := Solid_view (je.other);

                           --  Don't simulate joints connected to inactive bodies.
                           if not other.IsActive then
                              return;
                           end if;

                           island.Add (je.joint);
                           je.joint.m_islandFlag_is (True);

                           if (other.m_flags and solid.e_islandFlag) /= 0 then
                              return;
                           end if;

                           pragma assert (stackCount < stackSize);
                           stack (stackCount) := other;
                           stackCount := stackCount + 1;
                           other.m_flags_are (other.m_flags or solid.e_islandFlag);
                        end check;
                     begin
                        check;
                     end;

                     je := je.next;
                  end loop;

               end if;
            end loop;

--              island.Solve (step, Self.m_gravity, Self.m_allowSleep);

            --  Post solve cleanup.

            for i in 0 .. island.m_bodyCount - 1
            loop
               --  Allow static bodies to participate in other islands.
               b := Solid_view (island.m_bodies (i));

               if b.GetType = solid.b2_staticBody then
                  b.m_flags_are (b.m_flags and not solid.e_islandFlag);
               end if;
            end loop;

         end if;

         seed := Solid_view (seed.getNext);
      end loop;


      free (stack);

      --  Synchronize fixtures, check for out of range bodies.
      --
      b := Solid_view (Self.m_bodyList);
      while b /= null
      loop
         --  If a body was not in an island then it did not move.
         if (b.m_flags and solid.e_islandFlag) /= 0
         then
            if b.GetType /= solid.b2_staticBody then
               b.SynchronizeFixtures;                -- Update fixtures (for broad-phase).
            end if;
         end if;

         b := Solid_view (b.GetNext);
      end loop;

      --  Look for new contacts.
      Self.m_contactManager.FindNewContacts;
   end Solve;





   -- Find TOI contacts and solve them.
   --
   procedure SolveTOI (Self : in out b2World;   step : in b2TimeStep)
   is
      use impact.d2.Island,
          impact.d2.Contact,
          impact.d2.Solid;

      island : b2Island := to_b2Island (2 * b2_maxTOIContacts,
                                        b2_maxTOIContacts,
                                        0,
                                        Self.m_contactManager.m_contactListener);
   begin
      if Self.m_stepComplete
      then
         declare
            use type solid.Flag;
            b :        Solid_view := Self.m_bodyList;
            c : access b2Contact  := Self.m_contactManager.m_contactList;
         begin
            while b /= null
            loop
               b.m_flags_are (b.m_flags and not d2.Solid.e_islandFlag);
               b.m_sweep.alpha0 := 0.0;

               b := b.getNext;
            end loop;

            while c /= null
            loop
               -- Invalidate TOI
               c.m_flags_are (c.m_flags and not (Contact.e_toiFlag or Contact.e_islandFlag));
               c.m_toiCount_is (0);
               c.m_toi_is      (1.0);

               c := c.m_next;
            end loop;
         end;
      end if;


      -- Find TOI events and solve them.
      --
      loop
         declare
            minContact : Contact.view;
            minAlpha   : float32       := 1.0;

            c          : Contact.view := Self.m_contactManager.m_contactList;
         begin
            -- Find the first TOI.
            --
            while c /= null
            loop
               if   c.IsEnabled  = False            -- Is this contact disabled ?
                 or c.m_toiCount > b2_maxSubSteps   -- Prevent excessive sub-stepping.
               then
                  null;
               else
                  declare
                     alpha : float32 := 1.0;

                     procedure check
                     is
                     begin
                        if (c.m_flags and Contact.e_toiFlag) /= 0
                        then
                           -- This contact has a valid cached TOI.
                           alpha := c.m_toi;
                        else
                           declare
                              use Time_of_impact;

                              fA : constant Fixture.view := c.GetFixtureA;
                              fB : constant Fixture.view := c.GetFixtureB;

                              bA : Solid_view;
                              bB : Solid_view;

                              typeA    : b2BodyType;
                              typeB    : b2BodyType;

                              activeA,
                              activeB  : Boolean;

                              collideA,
                              collideB : Boolean;

                              alpha0   : float32;

                              indexA,
                              indexB   : int32;

                              input    : aliased Time_of_impact.b2TOIInput;
                              output   : aliased Time_of_impact.b2TOIOutput;

                              beta     : float32;

                           begin
                              -- Is there a sensor ?
                              if fA.IsSensor or fB.IsSensor then
                                 return;
                              end if;

                              bA := fA.GetBody;
                              bB := fB.GetBody;

                              typeA := bA.getType;
                              typeB := bB.getType;
                              pragma assert (   typeA = b2_dynamicBody
                                             or typeB = b2_dynamicBody);

                              activeA := bA.IsAwake and typeA /= b2_staticBody;
                              activeB := bB.IsAwake and typeB /= b2_staticBody;

                              -- Is at least one body active (awake and dynamic or kinematic)?
                              if    activeA = False
                                and activeB = False
                              then
                                 return;
                              end if;

                              collideA := bA.IsBullet or typeA /= b2_dynamicBody;
                              collideB := bB.IsBullet or typeB /= b2_dynamicBody;

                              -- Are these two non-bullet dynamic bodies?
                              if    collideA = False
                                and collideB = False
                              then
                                 return;
                              end if;

                              -- Compute the TOI for this contact.
                              -- Put the sweeps onto the same time interval.
                              alpha0 := bA.m_sweep.alpha0;

                              if bA.m_sweep.alpha0 < bB.m_sweep.alpha0
                              then
                                 alpha0 := bB.m_sweep.alpha0;
                                 bA.m_sweep.Advance (alpha0);

                              elsif bB.m_sweep.alpha0 < bA.m_sweep.alpha0
                              then
                                 alpha0 := bA.m_sweep.alpha0;
                                 bB.m_sweep.Advance (alpha0);
                              end if;

                              pragma assert (alpha0 < 1.0);

                              indexA := c.GetChildIndexA;
                              indexB := c.GetChildIndexB;

                              -- Compute the time of impact in interval [0, minTOI]

                              input.proxyA.Set (fA.GetShape, indexA);
                              input.proxyB.Set (fB.GetShape, indexB);

                              input.sweepA := bA.m_sweep.all;
                              input.sweepB := bB.m_sweep.all;
                              input.tMax   := 1.0;

                              b2TimeOfImpact (output'Access, input);

                              -- Beta is the fraction of the remaining portion of the .
                              beta := output.t;

                              if output.state = e_touching
                              then
                                 alpha := float32'Min (alpha0 + (1.0 - alpha0) * beta,
                                                       1.0);
                              else
                                 alpha := 1.0;
                              end if;

                              c.m_toi_is (alpha);
                              c.m_flags_are (c.m_flags or Contact.e_toiFlag);
                           end;
                        end if;

                        if alpha < minAlpha
                        then
                           -- This is the minimum TOI found so far.
                           minContact := c;
                           minAlpha   := alpha;
                        end if;
                     end check;

                  begin
                     check;
                  end;
               end if;

               c := c.m_next;
            end loop;


            if   minContact               = null
              or 1.0 - 10.0 * b2_epsilon  < minAlpha
            then
               -- No more TOI events. Done!
               Self.m_stepComplete := True;
               exit;
            end if;


            declare
               use type Solid.Flag;

               fA : constant Fixture.view := minContact.GetFixtureA;
               fB : constant Fixture.view := minContact.GetFixtureB;
               bA : constant Solid_view    := fA.GetBody;
               bB : constant Solid_view    := fB.GetBody;

               backup1 : constant b2Sweep   := bA.m_sweep.all;
               backup2 : constant b2Sweep   := bB.m_sweep.all;

               bodies  : array (0 .. 1) of Solid_view;

               subStep : b2TimeStep;
            begin
               -- Advance the bodies to the TOI.
               bA.Advance (minAlpha);
               bB.Advance (minAlpha);

               -- The TOI contact likely has some new contact points.
               minContact.Update (Self.m_contactManager.m_contactListener);

               minContact.m_flags_are (minContact.m_flags and not Contact.e_toiFlag);
               minContact.m_toiCount_is (minContact.m_toiCount + 1);

               -- Is the contact solid?
               if   minContact.IsEnabled  = False
                 or minContact.IsTouching = False
               then
                  -- Restore the sweeps.
                  minContact.SetEnabled (False);

                  bA.m_sweep.all := backup1;
                  bB.m_sweep.all := backup2;

                  bA.SynchronizeTransform;
                  bB.SynchronizeTransform;
               else
                  bA.SetAwake (True);
                  bB.SetAwake (True);

                  -- Build the island
                  island.Clear;
                  island.Add (bA);
                  island.Add (bB);
                  island.Add (minContact);

                  bA.m_flags_are (bA.m_flags or solid.e_islandFlag);
                  bB.m_flags_are (bB.m_flags or solid.e_islandFlag);

                  minContact.m_flags_are (minContact.m_flags or Contact.e_islandFlag);

                  -- Get contacts on bodyA and bodyB.
                  --
                  bodies := (bA, bB);

                  for i in 0 .. 1
                  loop
                     declare
                        bod     : constant Solid_view := bodies (i);
                        other   : Solid_view;

                        ce      : access b2ContactEdge;
                        contact :        d2.Contact.view;

                        sensorA,
                        sensorB : Boolean;

                        backup : b2Sweep;
                     begin
                        if bod.getType = b2_dynamicBody
                        then
                           ce := bod.m_contactList;

                           while ce /= null
                           loop
                              if   island.m_bodyCount    = island.m_bodyCapacity
                                or island.m_contactCount = island.m_contactCapacity
                              then
                                 exit;
                              end if;

                              contact := ce.contact;

                              declare
                                 procedure check
                                 is
                                 begin
                                    -- Has this contact already been added to the island?
                                    if (contact.m_flags and d2.Contact.e_islandFlag) /= 0 then
                                       return;
                                    end if;

                                    -- Only add static, kinematic, or bullet bodies.
                                    other := Solid_view (ce.other);

                                    if    other.getType  = b2_dynamicBody
                                      and bod  .IsBullet = False
                                      and other.IsBullet = False
                                    then
                                       return;
                                    end if;

                                    -- Skip sensors.
                                    sensorA := contact.getFixtureA.isSensor;
                                    sensorB := contact.getFixtureB.isSensor;

                                    if sensorA or sensorB then
                                       return;
                                    end if;

                                    -- Tentatively advance the body to the TOI.
                                    backup := other.m_sweep.all;

                                    if (other.m_flags and solid.e_islandFlag) = 0 then
                                       other.Advance (minAlpha);
                                    end if;

                                    -- Update the contact points
                                    contact.Update (Self.m_contactManager.m_contactListener);

                                    -- Was the contact disabled by the user?
                                    if contact.IsEnabled = False
                                    then
                                       other.m_sweep.all := backup;
                                       other.SynchronizeTransform;
                                       return;
                                    end if;

                                    -- Are there contact points?
                                    if contact.IsTouching = False
                                    then
                                       other.m_sweep.all := backup;
                                       other.SynchronizeTransform;
                                       return;
                                    end if;

                                    -- Add the contact to the island
                                    contact.m_flags_are (contact.m_flags / d2.Contact.e_islandFlag);   -- tbd: correct ??? *******************
                                    island.Add (contact);

                                    -- Has the other body already been added to the island?
                                    if (other.m_flags and solid.e_islandFlag) /= 0 then
                                       return;
                                    end if;

                                    -- Add the other body to the island.
                                    other.m_flags_are (other.m_flags or solid.e_islandFlag);

                                    if other.getType /= b2_staticBody then
                                       other.SetAwake (True);
                                    end if;

                                    island.Add (other);
                                 end check;

                              begin
                                 check;
                              end;


                              ce := ce.next;
                           end loop;
                        end if;
                     end;
                  end loop;

                  subStep.dt                 := (1.0 - minAlpha) * step.dt;
                  subStep.inv_dt             := 1.0 / subStep.dt;
                  subStep.dtRatio            := 1.0;
                  subStep.positionIterations := 20;
                  subStep.velocityIterations := step.velocityIterations;
                  subStep.warmStarting       := False;

--                    island.SolveTOI (subStep,
--                                     bA.m_islandIndex,
--                                     bB.m_islandIndex);

                  -- Reset island flags and synchronize broad-phase proxies.
                  for i in 0 .. island.m_bodyCount - 1
                  loop
                     declare
                        bod : constant Solid_view       := Solid_view (island.m_bodies (i));
                        ce  : access b2ContactEdge;
                     begin
                        bod.m_flags_are (bod.m_flags and not solid.e_islandFlag);

                        if bod.getType = b2_dynamicBody
                        then
                           bod.SynchronizeFixtures;

                           -- Invalidate all contact TOIs on this displaced body.
                           --
                           ce := bod.m_contactList;

                           while ce /= null
                           loop
                              ce.contact.m_flags_are (ce.contact.m_flags and not (   d2.Contact.e_toiFlag
                                                                                  or d2.Contact.e_islandFlag));
                              ce := ce.next;
                           end loop;
                        end if;
                     end;
                  end loop;


                  -- Commit fixture proxy movements to the broad-phase so that new contacts are created.
                  -- Also, some contacts can be destroyed.
                  --
                  Self.m_contactManager.FindNewContacts;

                  if Self.m_subStepping
                  then
                     Self.m_stepComplete := False;
                     exit;
                  end if;

               end if;
            end;

         end;
      end loop;

   end SolveTOI;






   --  Advance a dynamic body to its first time of contact
   --  and adjust the position to ensure clearance.
   --
--     procedure SolveTOI (Self : in out b2World;   solid : access impact.d2.Solid.b2Body'Class)
--     is
--        use type impact.d2.solid.b2BodyType;
--
--        toiContact : access contact.b2Contact;
--        toi        : float32 := 1.0;
--        toiOther   : Solid_view;
--        found      : Boolean;
--        count      : int32;
--        iter       : int32 := 0;
--
--        bullet     : constant Boolean := solid.IsBullet;
--
--        ce         : access contact.b2ContactEdge;
--
--        backup     : b2Sweep;
--
--        contacts   : Contact.views (1 .. b2_maxTOIContacts);
--        pragma Convention (C, contacts);                       -- Prevent default initialisation for performance.
--
--     begin
--        --  Find the minimum contact.
--
--        --  Iterate until all contacts agree on the minimum TOI. We have
--        --  to iterate because the TOI algorithm may skip some intermediate
--        --  collisions when objects rotate through each other.
--        --
--        loop
--           count := 0;
--           found := False;
--
--           ce := solid.m_contactList;
--           while ce /= null loop
--              declare
--                 procedure check
--                 is
--                    use impact.d2.Distance, impact.d2.Time_of_impact;
--                    use type uint16, impact.d2.Contact.view;
--                    other    : Solid_view;
--                    kind     : impact.d2.solid.b2BodyType;
--                    contact  : impact.d2.Contact.view;
--                    fixtureA,
--                    fixtureB : access Fixture.b2Fixture;
--                    bodyA,
--                    bodyB    : Solid_view;
--
--                    input    :         time_of_impact.b2TOIInput;
--                    output   : aliased time_of_impact.b2TOIOutput;
--                 begin
--                    if ce.contact = toiContact then
--                       return;
--                    end if;
--
--                    other := Solid_view (ce.other);
--                    kind  := other.GetType;
--
--                    --  Only bullets perform TOI with dynamic bodies.
--                    if bullet then
--                       --  Bullets only perform TOI with bodies that have their TOI resolved.
--                       if (other.m_flags and impact.d2.solid.e_toiFlag) = 0 then
--                          return;
--                       end if;
--
--                       --  No repeated hits on non-static bodies
--                       if          kind /= impact.d2.solid.b2_staticBody
--                         and then (ce.contact.m_flags and impact.d2.contact.e_bulletHitFlag) /= 0
--                       then
--                          return;
--                       end if;
--
--                    elsif kind = impact.d2.solid.b2_dynamicBody then
--                       return;
--                    end if;
--
--                    --  Check for a disabled contact.
--                    contact := ce.contact;
--                    if not contact.IsEnabled then
--                       return;
--                    end if;
--
--                    --  Prevent infinite looping.
--                    if contact.m_toiCount > 10 then
--                       return;
--                    end if;
--
--                    fixtureA := contact.getFixtureA;
--                    fixtureB := contact.getFixtureB;
--
--                    --  Cull sensors.
--                    if fixtureA.IsSensor or else fixtureB.IsSensor then
--                       return;
--                    end if;
--
--                    bodyA := Solid_view (fixtureA.getBody);
--                    bodyB := Solid_view (fixtureB.getBody);
--
--                    --  Compute the time of impact in interval [0, minTOI]
--                    Set (input.proxyA,  fixtureA.GetShape.all'Access);
--                    Set (input.proxyB,  fixtureB.GetShape.all'Access);
--
--                    input.sweepA := bodyA.m_sweep.all;
--                    input.sweepB := bodyB.m_sweep.all;
--                    input.tMax   := toi;
--
--                    b2TimeOfImpact (output'Access, input);
--
--                    if         output.state = e_touching
--                      and then output.t     < toi
--                    then
--                       toiContact := contact;
--                       toi        := output.t;
--                       toiOther   := other;
--                       found      := True;
--                    end if;
--
--                    count := count + 1;
--                 end check;
--              begin
--                 check;
--                 ce := ce.next;
--              end;
--           end loop;
--
--           iter := iter + 1;
--           exit when not (found and then count > 1 and then iter < 50);
--        end loop;
--
--        if toiContact = null then
--           solid.Advance (1.0);
--           return;
--        end if;
--
--        backup := solid.m_sweep.all;
--        solid.Advance (toi);
--        toiContact.Update (Self.m_contactManager.m_contactListener);
--
--        if not toiContact.IsEnabled then
--           --  Contact disabled. Backup and recurse.
--           solid.m_sweep.all := backup;
--           Self.SolveTOI (solid);
--        end if;
--
--        toiContact.m_toiCount_is (toiContact.m_toiCount + 1);
--
--        --  Update all the valid contacts on this body and build a contact island.
--        count := 0;
--
--        ce := solid.m_contactList;
--        while ce /= null and then count < b2_maxTOIContacts loop
--           declare
--              procedure check
--              is
--                 other    : constant Solid_view             := Solid_view (ce.other);
--                 kind     : constant impact.d2.solid.b2BodyType := other.GetType;
--                 contact  : Contact_view;
--                 fixtureA,
--                 fixtureB : access Fixture.b2Fixture;
--              begin
--                 --  Only perform correction with static bodies, so the
--                 --  body won't get pushed out of the world.
--                 if kind = impact.d2.solid.b2_dynamicBody then
--                    return;
--                 end if;
--
--                 --  Check for a disabled contact.
--                 contact := Contact_view (ce.contact);
--                 if not contact.IsEnabled then
--                    return;
--                 end if;
--
--                 fixtureA := contact.getFixtureA;
--                 fixtureB := contact.getFixtureB;
--
--                 --  Cull sensors.
--                 if fixtureA.IsSensor or else fixtureB.IsSensor then
--                    return;
--                 end if;
--
--                 --  The contact likely has some new contact points. The listener
--                 --  gives the user a chance to disable the contact.
--                 if contact /= toiContact then
--                    contact.Update (Self.m_contactManager.m_contactListener);
--                 end if;
--
--                 --  Did the user disable the contact?
--                 if not contact.IsEnabled then
--
--                    return;   -- Skip this contact.
--                 end if;
--
--                 if not contact.IsTouching then
--                    return;
--                 end if;
--
--                 count            := count + 1;
--                 contacts (count) := contact.all'Access;
--              end check;
--           begin
--              check;
--              ce := ce.next;
--           end;
--        end loop;
--
--        --  Reduce the TOI body's overlap with the contact island.
--
--        declare
--           solver         : toi_solver.b2TOISolver;      -- (&m_stackAllocator);
--           k_toiBaumgarte : constant := 0.75;
--           solved         : Boolean  := False;
--           pragma Unreferenced (solved);
--           contactsOkay   : Boolean;
--        begin
--           solver.Initialize (contacts (1 .. count),  solid);
--
--           for i in 1 .. 20 loop
--              contactsOkay := solver.Solve (k_toiBaumgarte);
--              if contactsOkay then
--                 solved := True;
--                 exit;
--              end if;
--           end loop;
--        end;
--
--        if toiOther.GetType /= impact.d2.Solid.b2_staticBody then
--           toiContact.m_flags_are (toiContact.m_flags or contact.e_bulletHitFlag);
--        end if;
--
--     end SolveTOI;






   --  Sequentially solve TOIs for each body. We bring each
   --  body to the time of contact and perform some position correction.
   --  Time is not conserved.
   --
--     procedure SolveTOI (Self : in out b2World;   step : in b2TimeStep)
--     is
--        use type uint16;
--        use type impact.d2.solid.b2BodyType;
--
--        c : Contact_view;
--        b : Solid_view;
--     begin
--        --  Prepare all contacts.
--        c := Contact_view (Self.m_contactManager.m_contactList);
--
--        while c /= null loop
--           c.m_flags_are (c.m_flags or contact.e_enabledFlag);    -- Enable the contact
--           c.m_toiCount_is (0);                                   -- Set the number of TOI events for this contact to zero.
--
--           c := Contact_view (c.m_next);
--        end loop;
--
--        --  Initialize the TOI flag.
--        b := Solid_view (Self.m_bodyList);
--
--        while b /= null loop
--           --  Kinematic, and static bodies will not be affected by the TOI event.
--           --  If a body was not in an island then it did not move.
--           if        (b.m_flags and solid.e_islandFlag) = 0
--             or else b.GetType = solid.b2_kinematicBody
--             or else b.GetType = solid.b2_staticBody
--           then
--              b.m_flags_are (b.m_flags or solid.e_toiFlag);
--           else
--              b.m_flags_are (b.m_flags and not solid.e_toiFlag);
--           end if;
--
--           b := Solid_view (b.getNext);
--        end loop;
--
--        --  Collide non-bullets.
--        b := Solid_view (Self.m_bodyList);
--
--        while b /= null loop
--           if         (b.m_flags and solid.e_toiFlag) = 0
--             and then not b.IsBullet
--           then
--              Self.SolveTOI (b);
--              b.m_flags_are (b.m_flags or solid.e_toiFlag);
--           end if;
--
--           b := Solid_view (b.getNext);
--        end loop;
--
--        --  Collide bullets.
--        b := Solid_view (Self.m_bodyList);
--
--        while b /= null loop
--           if (b.m_flags and solid.e_toiFlag) = 0
--             and then b.IsBullet
--           then
--              Self.SolveTOI (b);
--              b.m_flags_are (b.m_flags  or solid.e_toiFlag);
--           end if;
--
--           b := Solid_view (b.getNext);
--        end loop;
--
--     end SolveTOI;





   procedure Step (Self : in out b2World;   timeStep           : in float32;
                                            velocityIterations : in int32;
                                            positionIterations : in int32)
   is
      step : b2TimeStep;
   begin
      --  If new fixtures were added, we need to find the new contacts.
      if (Self.m_flags and e_newFixture) /= 0 then
         Self.m_contactManager.FindNewContacts;
         Self.m_flags := Self.m_flags and not e_newFixture;
      end if;

      Self.m_flags := Self.m_flags or e_locked;

      step.dt                 := timeStep;
      step.velocityIterations := velocityIterations;
      step.positionIterations := positionIterations;

      if timeStep > 0.0 then
         step.inv_dt := 1.0 / timeStep;
      else
         step.inv_dt := 0.0;
      end if;

      step.dtRatio      := Self.m_inv_dt0 * timeStep;
      step.warmStarting := Self.m_warmStarting;

      --  Update contacts. This is where some contacts are destroyed.
      Self.m_contactManager.Collide;

      --  Integrate velocities, solve velocity constraints, and integrate positions.
      if Self.m_stepComplete and step.dt > 0.0 then
         Self.Solve (step);
      end if;

      --  Handle TOI events.
      if Self.m_continuousPhysics and then step.dt > 0.0 then
         Self.SolveTOI (step);
      end if;

      if step.dt > 0.0 then
         Self.m_inv_dt0 := step.inv_dt;
      end if;

      if (Self.m_flags and e_clearForces) /= 0 then
         Self.ClearForces;
      end if;

      Self.m_flags := Self.m_flags and not e_locked;
   end Step;







   procedure ClearForces (Self : in out b2World)
   is
      b : Solid_view := Solid_view (Self.m_bodyList);
   begin
      while b /= null
      loop
         b.m_force.all  := (0.0, 0.0);
         b.m_torque.all := 0.0;

         b := Solid_view (b.getNext);
      end loop;
   end ClearForces;








   type broadPhase_view is access constant broadphase.b2BroadPhase'Class;


   type b2WorldQueryWrapper is
      record
        broadPhase : Broadphase_view;
        callback   : access world_callbacks.b2QueryCallback'Class;
      end record;


   function QueryCallback (Self : access b2WorldQueryWrapper;   proxyId : in int32) return Boolean
   is
      use Fixture;
      proxy   : constant b2FixtureProxy_view := b2FixtureProxy_view (Self.broadPhase.GetUserData (proxyId));
--        fixture : constant impact.d2.Fixture.view := impact.d2.Fixture.view (Self.broadPhase.GetUserData (proxyId));
   begin
      return Self.callback.ReportFixture (proxy.fixture);
   end QueryCallback;




   procedure QueryAABB (Self : access b2World;   callback : access world_callbacks.b2QueryCallback;
                                                 aabb     : in collision.b2AABB)
   is
      wrapper : aliased b2WorldQueryWrapper;
      procedure Query is new impact.d2.Broadphase.Query (b2WorldQueryWrapper,  QueryCallback);
   begin
      wrapper.broadPhase := Self.m_contactManager.m_broadPhase'Access;
      wrapper.callback   := callback;

      Query (Self.m_contactManager.m_broadPhase,  wrapper'Access, aabb);
   end QueryAABB;




   type b2WorldRayCastWrapper is
      record
        broadPhase : Broadphase_view;
        callback   : access world_callbacks.b2RayCastCallback'Class;
      end record;


   function RayCastCallback (Self : access b2WorldRayCastWrapper;   input   : in collision.b2RayCastInput;
                                                                    proxyId : in int32                 ) return float32
   is
      use Fixture;
      userData : access Any'Class    := Self.broadPhase.GetUserData (proxyId);
      proxy    : b2FixtureProxy_view := b2FixtureProxy_view (userData);

      fixture  : constant impact.d2.Fixture.view := proxy.fixture;
      index    :          int32                  := proxy.childIndex;
      output   : aliased  collision.b2RayCastOutput;
      hit      : constant Boolean                := fixture.RayCast (output'Access, input, index);

      fraction : float32;
      point    : b2Vec2;
   begin
      if hit then
         fraction := output.fraction;
         point    := (1.0 - fraction) * input.p1  +  fraction * input.p2;

         return Self.callback.ReportFixture (fixture, point, output.normal, fraction);
      end if;

      return input.maxFraction;
   end RayCastCallback;



   procedure RayCast (Self : access b2World;   callback       : access world_callbacks.b2RayCastCallback;
                                               point1, point2 : in     b2Vec2)
   is
      wrapper : aliased b2WorldRayCastWrapper;
      input   :         collision.b2RayCastInput;
      procedure rayCast is new impact.d2.Broadphase.rayCast (b2WorldRayCastWrapper,  RayCastCallback);
   begin
      wrapper.broadPhase := Self.m_contactManager.m_broadPhase'Access;
      wrapper.callback   := callback;

      input.maxFraction := 1.0;
      input.p1          := point1;
      input.p2          := point2;

      RayCast (Self.m_contactManager.m_broadPhase,  wrapper'Access, input);
   end RayCast;





   function m_contactManager (Self : access b2World) return access contact.manager.b2ContactManager
   is
   begin
      return Self.m_contactManager'Access;
   end m_contactManager;




   function m_flags          (Self : access b2World) return access uint32
   is
   begin
      return Self.m_flags'Access;
   end m_flags;




end impact.d2.World;
