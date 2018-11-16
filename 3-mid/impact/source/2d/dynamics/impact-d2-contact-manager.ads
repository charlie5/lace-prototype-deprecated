with impact.d2.Broadphase,
     impact.d2.Contact,
     impact.d2.world_Callbacks;



package impact.d2.contact.Manager
--
--
--
is

   --  Delegate of b2World.
   --
   type b2ContactManager is tagged
      record
         m_broadPhase      : aliased Broadphase.b2BroadPhase := broadphase.to_b2BroadPhase;
         m_contactList     : access  Contact.b2Contact;
         m_contactCount    :         int32;
         m_contactFilter   : access  world_callbacks.b2ContactFilter'Class;
         m_contactListener : access  world_callbacks.b2ContactListener'Class;
         --          b2BlockAllocator* m_allocator;
      end record;


   function to_b2ContactManager return b2ContactManager;


   --  Broad-phase callback.
   --
   procedure addPair (Self : access b2ContactManager;   userDataA, userDataB : access Any'Class);

   procedure FindNewContacts (Self : in out b2ContactManager);
   procedure Destroy         (Self : in out b2ContactManager;   c : in out impact.d2.Contact.view);
   procedure Collide         (Self : in out b2ContactManager);




end impact.d2.contact.Manager;

