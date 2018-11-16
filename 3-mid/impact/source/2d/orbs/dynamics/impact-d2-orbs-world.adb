with impact.d2.orbs.Fixture,
     impact.d2.orbs.Island,
     impact.d2.orbs.Time_of_impact,
     impact.d2.orbs.toi_Solver,
     impact.d2.orbs.Distance,
     impact.d2.orbs.Broadphase,

     ada.unchecked_Deallocation;




package body impact.d2.orbs.World
is

   use type int32, uint32;
   use type impact.d2.orbs.Joint.b2JointEdge_view;



   type Solid_view is access all Solid.item'Class;

   procedure free is new ada.unchecked_Deallocation (Solid.item'Class, Solid_view);



--     type Joint_view is access all Joint.b2Joint'Class;

   procedure free is new ada.unchecked_Deallocation (Joint.b2Joint'Class, Joint.view);


   type Contact_view is access all contact.b2Contact'Class;



   type Solid_views is array (int32 range <>) of Solid_view;

   type access_Solid_views is access all Solid_views;

   procedure free is new ada.Unchecked_Deallocation (Solid_views, access_Solid_views);




   function to_b2World (gravity : in b2Vec2;   doSleep : in Boolean) return b2World
   is
      Self : b2World;
   begin
      --  m_debugDraw = NULL;

      Self.m_bodyCount         := 0;
      Self.m_jointCount        := 0;

      Self.m_warmStarting      := True;
      Self.m_continuousPhysics := True;

      Self.m_allowSleep        := doSleep;
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
   end destruct;





   function IsLocked (Self : in    b2World) return Boolean
   is
   begin
      return (Self.m_flags and e_locked) = e_locked;
   end IsLocked;



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




   function CreateBody (Self : access b2World;   def : in solid.Definition) return Solid.view
   is
      b : Solid.view;
   begin
      pragma Assert (not Self.IsLocked);

      if Self.IsLocked then
         return null;
      end if;

      b := new solid.item'(Solid.Forge.to_b2Body (def, Self.all'Unchecked_Access));

      Self.add (b);

      return b;
   end CreateBody;



   procedure add (Self : in out b2World;   the_Object : access impact.d2.orbs.Solid.item'Class)
   is
      b : constant Solid_view := Solid_view (the_Object);
   begin
      b.m_world_is (Self'Unchecked_Access);

      --  Add to world doubly linked list.
      --
      b.m_next_is (Self.m_bodyList);

      if Self.m_bodyList /= null then
         Self.m_bodyList.m_prev_is (b);
      end if;

      Self.m_bodyList  := b;
      Self.m_bodyCount := Self.m_bodyCount + 1;
   end add;




   procedure DestroyBody (Self : in out b2World;   solid : access impact.d2.orbs.Solid.item'Class)
   is
      use type Fixture.item;

      b       : Solid_view := solid.all'Access;

      je, je0 : access joint.b2JointEdge;
      ce, ce0 : access contact.b2ContactEdge;
      f,  f0  :        Fixture.view;
   begin
      pragma Assert (Self.m_bodyCount > 0);
      pragma Assert (not Self.IsLocked);

      if Self.IsLocked then
         return;
      end if;

      --  Delete the attached joints.
      je := b.getJointList;
      while je /= null loop
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
      while ce /= null loop
         ce0 := ce;
         ce  := ce.next;
         Self.m_contactManager.Destroy (ce0.contact);
      end loop;
      b.m_contactList_is (null);

      --  Delete the attached fixtures. This destroys broad-phase proxies.
      f := b.get_Fixture.all'Access;

      if Fixture.item (f.all) /= fixture.null_Fixture then
         if Self.m_destructionListener /= null then
            Self.m_destructionListener.SayGoodbye (f);
         end if;

         f.DestroyProxy (Self.m_contactManager.m_broadPhase'Access);
         f.Destroy; -- (&m_blockAllocator);
         f.destruct; -- ~b2Fixture();
         fixture.free (f);    -- m_blockAllocator.Free(f0, sizeof(b2Fixture));
      end if;

      b.m_fixtureList_is (impact.d2.orbs.Fixture.null_Fixture);

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
      use type Joint.view;
      j     : Joint.view;
      edge  : access contact.b2ContactEdge;
      bodyA,
      bodyB : Solid_view;
   begin
      pragma Assert (not Self.IsLocked);

      if Self.IsLocked then
         return null;
      end if;

      j := Joint.view (Joint.Create (def)); --, &m_blockAllocator);

      --  Connect to the world list.
      j.m_prev_is (null);
      j.m_next_is (Self.m_jointList);
      if Self.m_jointList /= null then   Self.m_jointList.m_prev_is (j);   end if;
      Self.m_jointList  := j;
      Self.m_jointCount := Self.m_jointCount + 1;

      --  Connect to the bodies' doubly linked lists.
      j.m_edgeA.joint := j;
      j.m_edgeA.other := j.getBodyB;
      j.m_edgeA.prev  := null;
      j.m_edgeA.next  := j.getBodyA.getJointList;
      if j.getBodyA.getJointList /= null then   j.getBodyA.getJointList.prev := j.m_edgeA;   end if;
      j.getBodyA.m_jointList_is (j.m_edgeA);

      j.m_edgeB.joint := j;
      j.m_edgeB.other := j.getBodyA;
      j.m_edgeB.prev  := null;
      j.m_edgeB.next  := j.getBodyB.getJointList;
      if j.getBodyB.getJointList /= null then   j.getBodyB.getJointList.prev := j.m_edgeB;   end if;
      j.getBodyB.m_jointList_is (j.m_edgeB);

      bodyA := Solid_view (def.bodyA);
      bodyB := Solid_view (def.bodyB);

      --  If the joint prevents collisions, then flag any contacts for filtering.
      if not def.collideConnected then
         edge := bodyB.GetContactList;
         while edge /= null loop
            if edge.other = bodyA then
               edge.contact.FlagForFiltering;   -- Flag the contact for filtering at the next time step (where either body is awake).
            end if;

            edge := edge.next;
         end loop;
      end if;

      --  Note: creating a joint doesn't wake the bodies.
      return j;
   end CreateJoint;






   procedure DestroyJoint (Self : in out b2World;   joint : access impact.d2.orbs.Joint.b2Joint'Class)
   is
      use type impact.d2.orbs.Joint.view;
      j                : impact.d2.orbs.Joint.view := joint.all'Access;

      collideConnected : Boolean;
      bodyA, bodyB     : Solid_view;
      edge             : access contact.b2ContactEdge;
   begin
      pragma Assert (not Self.IsLocked);

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

      impact.d2.orbs.Joint.Destroy (j); --, &m_blockAllocator);

      pragma Assert (Self.m_jointCount > 0);
      Self.m_jointCount := Self.m_jointCount - 1;

      --  If the joint prevents collisions, then flag any contacts for filtering.
      if not collideConnected then
         edge := bodyB.GetContactList;
         while edge /= null loop
            if edge.other = bodyA then
               edge.contact.FlagForFiltering;  -- Flag the contact for filtering at the next time step (where either body is awake).
            end if;

            edge := edge.next;
         end loop;
      end if;

   end DestroyJoint;









   function GetBodyList (Self : in b2World) return access Solid.item'Class
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






   --  Find islands, integrate and solve constraints, solve position constraints
   --
   procedure Solve (Self : in out b2World;   step : in b2TimeStep)
   is
      use type uint16, solid.Kind;
      use type Joint.view;

      --  Size the island for the worst case.
      island : impact.d2.orbs.Island.b2Island := impact.d2.orbs.island.to_b2Island (Self.m_bodyCount,
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
      while b /= null loop
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
      stack     := new Solid_views (1 .. stackSize);
      seed      := Solid_view (Self.m_bodyList);

      while seed /= null loop
         if        (seed.m_flags and solid.e_islandFlag) = 0
           and then not (not seed.IsAwake or else not seed.IsActive)
           and then seed.GetType /= solid.b2_staticBody                    -- The seed can be dynamic or kinematic.
         then
            --  Reset island and stack.
            island.Clear;
            stackCount         := 0;
            stackCount         := stackCount + 1;
            stack (stackCount) := seed;
            seed.m_flags_are (seed.m_flags or solid.e_islandFlag);

            --  Perform a depth first search (DFS) on the constraint graph.
            while stackCount > 0 loop
               --  Grab the next body off the stack and add it to the island.
               b          := stack (stackCount);
               stackCount := stackCount - 1;
               pragma Assert (b.IsActive);
               island.Add (b);

               --  Make sure the body is awake.
               b.SetAwake (True);

               --  To keep islands as small as possible, we don't
               --  propagate islands across static bodies.
               if b.GetType /= solid.b2_staticBody then

                  --  Search all contacts connected to this body.
                  ce := b.m_contactList;

                  while ce /= null loop
                     declare
                        contact : constant Contact_view := Contact_view (ce.contact);

                        procedure check
                        is
                           sensorA, sensorB : Boolean;
                           other            : Solid_view;
                        begin
                           --  Has this contact already been added to an island?
                           if (contact.m_flags and impact.d2.orbs.contact.e_islandFlag) /= 0 then
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
                           contact.m_flags_are (contact.m_flags or impact.d2.orbs.contact.e_islandFlag);

                           other := Solid_view (ce.other);

                           --  Was the other body already added to this island?
                           if (other.m_flags and solid.e_islandFlag) /= 0 then
                              return;
                           end if;

                           pragma Assert (stackCount < stackSize);
                           stackCount         := stackCount + 1;
                           stack (stackCount) := other;
                           other.m_flags_are (other.m_flags or solid.e_islandFlag);
                        end check;

                     begin
                        check;
                        ce := ce.next;
                     end;
                  end loop;


                  --  Search all joints connect to this body.
                  --
                  je := b.getJointList;
                  while je /= null loop
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

                           pragma Assert (stackCount < stackSize);
                           stackCount := stackCount + 1;
                           stack (stackCount) := other;
                           other.m_flags_are (other.m_flags or solid.e_islandFlag);
                        end check;
                     begin
                        check;
                        je := je.next;
                     end;
                  end loop;

               end if;
            end loop;

            island.Solve (step, Self.m_gravity, Self.m_allowSleep);

            --  Post solve cleanup.
            --
            for i in 1 .. island.m_bodyCount loop
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
      while b /= null loop
         --  If a body was not in an island then it did not move.
         if (b.m_flags and solid.e_islandFlag) /= 0 then
            if b.GetType /= solid.b2_staticBody then
               b.SynchronizeFixtures;                -- Update fixtures (for broad-phase).
            end if;
         end if;

         b := Solid_view (b.GetNext);
      end loop;

      --  Look for new contacts.
      Self.m_contactManager.FindNewContacts;
   end Solve;












   --  Advance a dynamic body to its first time of contact
   --  and adjust the position to ensure clearance.
   --
   procedure SolveTOI (Self : in out b2World;   solid : access impact.d2.orbs.Solid.item'Class)
   is
      use type impact.d2.orbs.solid.Kind;

      toiContact : access contact.b2Contact;
      toi        : float32 := 1.0;
      toiOther   : Solid_view;
      found      : Boolean;
      count      : int32;
      iter       : int32 := 0;

      bullet     : constant Boolean := solid.IsBullet;

      ce         : access contact.b2ContactEdge;

      backup     : b2Sweep;

      contacts   : Contact.views (1 .. b2_maxTOIContacts);
      pragma Convention (C, contacts);                       -- Prevent default initialisation for performance.

   begin
      --  Find the minimum contact.

      --  Iterate until all contacts agree on the minimum TOI. We have
      --  to iterate because the TOI algorithm may skip some intermediate
      --  collisions when objects rotate through each other.
      --
      loop
         count := 0;
         found := False;

         ce := solid.m_contactList;
         while ce /= null loop
            declare
               procedure check
               is
                  use impact.d2.orbs.Distance, impact.d2.orbs.Time_of_impact;
                  use type uint16,
                      impact.d2.orbs.Contact.view;
                  other    : Solid_view;
                  kind     : impact.d2.orbs.solid.Kind;
                  contact  : impact.d2.orbs.Contact.view;
                  fixtureA,
                  fixtureB : access Fixture.item;
                  bodyA,
                  bodyB    : Solid_view;

                  input    :         time_of_impact.b2TOIInput;
                  output   : aliased time_of_impact.b2TOIOutput;
               begin
                  if ce.contact = toiContact then
                     return;
                  end if;

                  other := Solid_view (ce.other);
                  kind  := other.GetType;

                  --  Only bullets perform TOI with dynamic bodies.
                  if bullet then
                     --  Bullets only perform TOI with bodies that have their TOI resolved.
                     if (other.m_flags and impact.d2.orbs.solid.e_toiFlag) = 0 then
                        return;
                     end if;

                     --  No repeated hits on non-static bodies
                     if          kind /= impact.d2.orbs.solid.b2_staticBody
                       and then (ce.contact.m_flags and impact.d2.orbs.contact.e_bulletHitFlag) /= 0
                     then
                        return;
                     end if;

                  elsif kind = impact.d2.orbs.solid.b2_dynamicBody then
                     return;
                  end if;

                  --  Check for a disabled contact.
                  contact := ce.contact;
                  if not contact.IsEnabled then
                     return;
                  end if;

                  --  Prevent infinite looping.
                  if contact.m_toiCount > 10 then
                     return;
                  end if;

                  fixtureA := contact.getFixtureA;
                  fixtureB := contact.getFixtureB;

                  --  Cull sensors.
                  if fixtureA.IsSensor or else fixtureB.IsSensor then
                     return;
                  end if;

                  bodyA := Solid_view (fixtureA.getBody);
                  bodyB := Solid_view (fixtureB.getBody);

                  --  Compute the time of impact in interval [0, minTOI]
                  Set (input.proxyA,  fixtureA.GetShape);
                  Set (input.proxyB,  fixtureB.GetShape);

                  input.sweepA := bodyA.m_sweep.all;
                  input.sweepB := bodyB.m_sweep.all;
                  input.tMax   := toi;

                  b2TimeOfImpact (output'Access, input);

                  if         output.state = e_touching
                    and then output.t     < toi
                  then
                     toiContact := contact;
                     toi        := output.t;
                     toiOther   := other;
                     found      := True;
                  end if;

                  count := count + 1;
               end check;
            begin
               check;
               ce := ce.next;
            end;
         end loop;

         iter := iter + 1;
         exit when not (found and then count > 1 and then iter < 50);
      end loop;

      if toiContact = null then
         solid.Advance (1.0);
         return;
      end if;

      backup := solid.m_sweep.all;
      solid.Advance (toi);
      toiContact.Update (Self.m_contactManager.m_contactListener);

      if not toiContact.IsEnabled then
         --  Contact disabled. Backup and recurse.
         solid.m_sweep.all := backup;
         Self.SolveTOI (solid);
      end if;

      toiContact.m_toiCount_is (toiContact.m_toiCount + 1);

      --  Update all the valid contacts on this body and build a contact island.
      count := 0;

      ce := solid.m_contactList;
      while ce /= null and then count < b2_maxTOIContacts loop
         declare
            procedure check
            is
               other    : constant Solid_view             := Solid_view (ce.other);
               kind     : constant impact.d2.orbs.solid.Kind := other.GetType;
               contact  : Contact_view;
               fixtureA,
               fixtureB : access Fixture.item;
            begin
               --  Only perform correction with static bodies, so the
               --  body won't get pushed out of the world.
               if kind = impact.d2.orbs.solid.b2_dynamicBody then
                  return;
               end if;

               --  Check for a disabled contact.
               contact := Contact_view (ce.contact);
               if not contact.IsEnabled then
                  return;
               end if;

               fixtureA := contact.getFixtureA;
               fixtureB := contact.getFixtureB;

               --  Cull sensors.
               if fixtureA.IsSensor or else fixtureB.IsSensor then
                  return;
               end if;

               --  The contact likely has some new contact points. The listener
               --  gives the user a chance to disable the contact.
               if contact /= toiContact then
                  contact.Update (Self.m_contactManager.m_contactListener);
               end if;

               --  Did the user disable the contact?
               if not contact.IsEnabled then

                  return;   -- Skip this contact.
               end if;

               if not contact.IsTouching then
                  return;
               end if;

               count            := count + 1;
               contacts (count) := contact.all'Access;
            end check;
         begin
            check;
            ce := ce.next;
         end;
      end loop;

      --  Reduce the TOI body's overlap with the contact island.

      declare
         solver         : toi_solver.b2TOISolver;      -- (&m_stackAllocator);
         k_toiBaumgarte : constant := 0.75;
         solved         : Boolean  := False;
         pragma Unreferenced (solved);
         contactsOkay   : Boolean;
      begin
         solver.Initialize (contacts (1 .. count),  solid);

         for i in 1 .. 20 loop
            contactsOkay := solver.Solve (k_toiBaumgarte);
            if contactsOkay then
               solved := True;
               exit;
            end if;
         end loop;
      end;

      if toiOther.GetType /= impact.d2.orbs.Solid.b2_staticBody then
         toiContact.m_flags_are (toiContact.m_flags or contact.e_bulletHitFlag);
      end if;

   end SolveTOI;






   --  Sequentially solve TOIs for each body. We bring each
   --  body to the time of contact and perform some position correction.
   --  Time is not conserved.
   --
   procedure SolveTOI (Self : in out b2World)
   is
      use type uint16;
      use type impact.d2.orbs.solid.Kind;

      c : Contact_view;
      b : Solid_view;
   begin
      --  Prepare all contacts.
      c := Contact_view (Self.m_contactManager.m_contactList);

      while c /= null loop
         c.m_flags_are (c.m_flags or contact.e_enabledFlag);    -- Enable the contact
         c.m_toiCount_is (0);                                   -- Set the number of TOI events for this contact to zero.

         c := Contact_view (c.m_next);
      end loop;

      --  Initialize the TOI flag.
      b := Solid_view (Self.m_bodyList);

      while b /= null loop
         --  Kinematic, and static bodies will not be affected by the TOI event.
         --  If a body was not in an island then it did not move.
         if        (b.m_flags and solid.e_islandFlag) = 0
           or else b.GetType = solid.b2_kinematicBody
           or else b.GetType = solid.b2_staticBody
         then
            b.m_flags_are (b.m_flags or solid.e_toiFlag);
         else
            b.m_flags_are (b.m_flags and not solid.e_toiFlag);
         end if;

         b := Solid_view (b.getNext);
      end loop;

      --  Collide non-bullets.
      b := Solid_view (Self.m_bodyList);

      while b /= null loop
         if         (b.m_flags and solid.e_toiFlag) = 0
           and then not b.IsBullet
         then
            Self.SolveTOI (b);
            b.m_flags_are (b.m_flags or solid.e_toiFlag);
         end if;

         b := Solid_view (b.getNext);
      end loop;

      --  Collide bullets.
      b := Solid_view (Self.m_bodyList);

      while b /= null loop
         if (b.m_flags and solid.e_toiFlag) = 0
           and then b.IsBullet
         then
            Self.SolveTOI (b);
            b.m_flags_are (b.m_flags  or solid.e_toiFlag);
         end if;

         b := Solid_view (b.getNext);
      end loop;

   end SolveTOI;





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
      if step.dt > 0.0 then
         Self.Solve (step);
      end if;

      --  Handle TOI events.
      if Self.m_continuousPhysics and then step.dt > 0.0 then
         Self.SolveTOI;
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
      while b /= null loop
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
      fixture : constant impact.d2.orbs.Fixture.view := impact.d2.orbs.Fixture.view (Self.broadPhase.GetUserData (proxyId));
   begin
      return Self.callback.ReportFixture (fixture);
   end QueryCallback;




   procedure QueryAABB (Self : access b2World;   callback : access world_callbacks.b2QueryCallback;
                                                 aabb     : in collision.b2AABB)
   is
      wrapper : aliased b2WorldQueryWrapper;
      procedure Query is new impact.d2.orbs.Broadphase.Query (b2WorldQueryWrapper,  QueryCallback);
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
      fixture  : constant impact.d2.orbs.Fixture.view := impact.d2.orbs.Fixture.view (Self.broadPhase.getUserData (proxyId));
      output   : aliased collision.b2RayCastOutput;
      hit      : constant Boolean            := fixture.RayCast (output'Access, input);

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
      procedure rayCast is new impact.d2.orbs.Broadphase.rayCast (b2WorldRayCastWrapper,  RayCastCallback);
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




end impact.d2.orbs.World;
