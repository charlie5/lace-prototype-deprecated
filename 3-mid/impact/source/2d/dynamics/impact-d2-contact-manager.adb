with impact.d2.Fixture,
     impact.d2.Solid;


package body impact.d2.contact.Manager
is
   use type int32;



   type default_Listener is new world_Callbacks.b2ContactListener with null record;

   b2_defaultListener : aliased default_Listener;
   b2_defaultFilter   : aliased world_Callbacks.b2ContactFilter;




   function to_b2ContactManager return b2ContactManager
   is
      Self : b2ContactManager;
   begin
      Self.m_contactCount    := 0;
      Self.m_contactFilter   := b2_defaultFilter'Access;
      Self.m_contactListener := b2_defaultListener'Access;

      return Self;
   end to_b2ContactManager;




   procedure Destroy (Self : in out b2ContactManager;   c : in out impact.d2.Contact.view)
   is
      use type Contact.view;

      fixtureA : constant access Fixture.b2Fixture  := c.GetFixtureA;
      fixtureB : constant access Fixture.b2Fixture  := c.GetFixtureB;
      bodyA    : constant access Solid.b2Body'Class := fixtureA.GetBody;
      bodyB    : constant access Solid.b2Body'Class := fixtureB.GetBody;
   begin
      if         Self.m_contactListener /= null
        and then c.IsTouching
      then
         Self.m_contactListener.EndContact (c);
      end if;

      --  Remove from the world.
      if c.m_prev /= null then
         c.m_prev.m_next_is (c.m_next);
      end if;

      if c.m_next /= null then
         c.m_next.m_prev_is (c.m_prev);
      end if;

      if c = Self.m_contactList then
         Self.m_contactList := c.m_next;
      end if;

      --  Remove from body 1
      if c.m_nodeA.prev /= null then
         c.m_nodeA.prev.next := c.m_nodeA.next;
      end if;

      if c.m_nodeA.next /= null then
         c.m_nodeA.next.prev := c.m_nodeA.prev;
      end if;

      if c.m_nodeA'Access = bodyA.getContactList then
         bodyA.m_contactList_is (c.m_nodeA.next);
      end if;

      --  Remove from body 2
      if c.m_nodeB.prev /= null then
         c.m_nodeB.prev.next := c.m_nodeB.next;
      end if;

      if c.m_nodeB.next /= null then
         c.m_nodeB.next.prev := c.m_nodeB.prev;
      end if;

      if c.m_nodeB'Access = bodyB.getContactList then
         bodyB.m_contactList_is (c.m_nodeB.next);
      end if;

      --  Call the factory.
      Contact.Destroy (c); --, m_allocator);
      Self.m_contactCount := Self.m_contactCount - 1;
   end Destroy;





   --  This is the top level collision call for the time step. Here
   --  all the narrow phase collision is processed for the world
   --  contact list.
   --
   procedure Collide (Self : in out b2ContactManager)
   is
      use type Contact.view;

      c : Contact.view := Contact.view (Self.m_contactList);
   begin
      --  Update awake contacts.
      while c /= null loop
         declare
            fixtureA : constant access Fixture.b2Fixture'Class  := c.GetFixtureA;
            fixtureB : constant access Fixture.b2Fixture'Class  := c.GetFixtureB;

            bodyA    : constant access Solid.b2Body'Class := fixtureA.GetBody;
            bodyB    : constant access Solid.b2Body'Class := fixtureB.GetBody;

            procedure check
            is
               use type uint32;
               cNuke    : Contact.view;
               proxyIdA,
               proxyIdB : int32;
               overlap  : Boolean;
            begin
               --  Is this contact flagged for filtering?
               if (c.m_flags and Contact.e_filterFlag) /= 0 then
                  --  Should these bodies collide?
                  if not bodyB.ShouldCollide (bodyA) then
                     cNuke := c;
                     c     := cNuke.GetNext;
                     Destroy (cNuke);
                     return;
                  end if;

                  --  Check user filtering.
                  if         Self.m_contactFilter /= null
                    and then not Self.m_contactFilter.ShouldCollide (fixtureA, fixtureB)
                  then
                     cNuke := c;
                     c     := cNuke.GetNext;
                     Destroy (cNuke);
                     return;
                  end if;

                  --  Clear the filtering flag.
                  c.m_flags := c.m_flags and not Contact.e_filterFlag;
               end if;

               proxyIdA := fixtureA.m_proxyId;
               proxyIdB := fixtureB.m_proxyId;

               overlap  := Self.m_broadPhase.TestOverlap (proxyIdA, proxyIdB);

               --  Here we destroy contacts that cease to overlap in the broad-phase.
               if not overlap then
                  cNuke := c;
                  c     := cNuke.GetNext;
                  Destroy (cNuke);
                  return;
               end if;

               --  The contact persists.
               c.Update (Self.m_contactListener);
               c := c.GetNext;
            end check;

         begin
            if not bodyA.IsAwake  and then  not bodyB.IsAwake then
               c := c.GetNext;
            else
               check;
            end if;
         end;
      end loop;

   end Collide;




   procedure FindNewContacts (Self : in out b2ContactManager)
   is
      procedure update_Pairs is new broadphase.UpdatePairs (callback_t => b2ContactManager,
                                                            addPair    => manager.addPair);
   begin
      update_Pairs (self.m_broadPhase, Self'Access);
   end FindNewContacts;



   type Solid_view is access all Solid.b2Body'Class;



   --  Broad-phase callback.
   --
   procedure addPair (Self : access b2ContactManager;   userDataA, userDataB : access Any'Class)
   is

      fixtureA : access Fixture.b2Fixture := Fixture.view (userDataA);
      fixtureB : access Fixture.b2Fixture := Fixture.view (userDataB);
      --        b2Fixture* fixtureB = (b2Fixture*)proxyUserDataB;

      bodyA : Solid_view := fixtureA.GetBody.all'Access;
      bodyB : Solid_view := fixtureB.GetBody.all'Access;
      --        b2Body* bodyB = fixtureB.GetBody();

      edge : access b2ContactEdge;
      c    :        Contact.view;
   begin
      --  Are the fixtures on the same body?
      if bodyA = bodyB then
         return;
      end if;

      --  Does a contact already exist?
      edge := bodyB.GetContactList;
      while edge /= null loop
         if edge.other = bodyA then
            declare
               fA : constant access Fixture.b2Fixture := edge.contact.GetFixtureA;
               fB : constant access Fixture.b2Fixture := edge.contact.GetFixtureB;
               --                          b2Fixture* fB := edge.contact.GetFixtureB();
            begin
               if fA = fixtureA and then fB = fixtureB then
                  --  A contact already exists.
                  return;
               end if;

               if fA = fixtureB and then fB = fixtureA then
                  --  A contact already exists.
                  return;
               end if;
            end;
         end if;

         edge := edge.next;
      end loop;

      --  Does a joint override collision? Is at least one body dynamic?
      if bodyB.ShouldCollide (bodyA) = False then
         return;
      end if;

      --  Check user filtering.
      if         Self.m_contactFilter /= null
        and then not Self.m_contactFilter.ShouldCollide (fixtureA, fixtureB)
      then
         return;
      end if;


      --  Call the factory.
      c := Contact.create (fixtureA, fixtureB).all'Access; --, m_allocator);

      --  Contact creation may swap fixtures.
      fixtureA := c.GetFixtureA;
      fixtureB := c.GetFixtureB;
      bodyA := fixtureA.GetBody.all'Access;
      bodyB := fixtureB.GetBody.all'Access;

      --  Insert into the world.
      c.m_prev := null;
      c.m_next := Contact.view (Self.m_contactList); --.all'unchecked_access;

      if Self.m_contactList /= null then
         Self.m_contactList.m_prev := Contact.view (c);
      end if;

      Self.m_contactList := c;

      --  Connect to island graph.

      --  Connect to body A
      c.m_nodeA.contact := c;
      c.m_nodeA.other   := bodyB;

      c.m_nodeA.prev := null;
      c.m_nodeA.next := bodyA.m_contactList;

      if bodyA.m_contactList /= null then
         bodyA.m_contactList.prev := c.m_nodeA'Access;
      end if;

      bodyA.m_contactList_is (c.m_nodeA'Access);

      --  Connect to body B
      c.m_nodeB.contact := c;
      c.m_nodeB.other := bodyA;

      c.m_nodeB.prev := null;
      c.m_nodeB.next := bodyB.m_contactList;

      if bodyB.m_contactList /= null then
         bodyB.m_contactList.prev := c.m_nodeB'Access;
      end if;

      bodyB.m_contactList_is (c.m_nodeB'Access);

      Self.m_contactCount := Self.m_contactCount + 1;
   end addPair;



end impact.d2.contact.Manager;
