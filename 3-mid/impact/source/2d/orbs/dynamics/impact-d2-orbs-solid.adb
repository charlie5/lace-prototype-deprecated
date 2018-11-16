with impact.d2.orbs.World,
     impact.d2.orbs.Broadphase,

     ada.unchecked_Deallocation;



package body impact.d2.orbs.Solid
is
   use type int32, uint16;




   procedure SetType (Self : in out item;   Kind : in solid.Kind)
   is
      ce : access contact.b2ContactEdge;
   begin
      if Self.m_type = Kind then
         return;
      end if;

      Self.m_type := Kind;

      Self.ResetMassData;

      if Self.m_type = b2_staticBody then
         Self.m_linearVelocity  := (0.0, 0.0);
         Self.m_angularVelocity := 0.0;
      end if;

      Self.SetAwake (True);

      Self.m_force  := (0.0, 0.0);
      Self.m_torque := 0.0;

      --  Since the body type changed, we need to flag contacts for filtering.
      --
      ce := Self.m_contactList;

      while ce /= null loop
         ce.contact.FlagForFiltering;
         ce := ce.next;
      end loop;
   end SetType;





   function GetType (Self : in item) return Kind
   is
   begin
      return Self.m_type;
   end GetType;





   function GetContactList (Self : in item) return access contact.b2ContactEdge
   is
   begin
      return Self.m_contactList;
   end GetContactList;



   function GetTransform (Self : in item) return b2Transform
   is
   begin
      return Self.m_xf;
   end GetTransform;




   function GetWorldPoint (Self : in item;   localPoint : in b2Vec2) return b2Vec2
   is
   begin
      return b2Mul (Self.m_xf, localPoint);
   end GetWorldPoint;



   function GetWorldVector (Self : in item;   localVector : in b2Vec2) return b2Vec2
   is
   begin
      return b2Mul (Self.m_xf.R, localVector);
   end GetWorldVector;



   function GetMass (Self : in item) return float32
   is
   begin
      return self.m_mass;
   end GetMass;



   procedure ResetMassData (Self : in out item)
   is
      center,
      oldCenter :         b2Vec2;
      massData  : aliased shape.mass_Data;
      f         :         Fixture.item renames Self.m_Fixture;
   begin
      --  Compute mass data from shapes. Each shape has its own density.
      Self.m_mass    := 0.0;
      Self.m_invMass := 0.0;
      Self.m_I       := 0.0;
      Self.m_invI    := 0.0;
      Self.m_sweep.localCenter := (0.0, 0.0);

      --  Static and kinematic bodies have zero mass.
      if        Self.m_type = b2_staticBody
        or else Self.m_type = b2_kinematicBody
      then
         Self.m_sweep.c0 := Self.m_xf.position;
         Self.m_sweep.c  := Self.m_xf.position;
         return;
      end if;

      pragma Assert (Self.m_type = b2_dynamicBody);

      --  Accumulate mass over all fixtures.
      center := b2Vec2_zero;
      f      := Self.m_Fixture;

      if f.getDensity /= 0.0 then
         f.GetMassData (massData'Access);
         Self.m_mass := Self.m_mass + massData.mass;
         center      := center      + massData.mass * massData.center;
         Self.m_I    := Self.m_I    + massData.I;
      end if;


      --  Compute center of mass.
      if Self.m_mass > 0.0 then
         Self.m_invMass := 1.0 / Self.m_mass;
         center         := center * Self.m_invMass;
      else
         --  Force all dynamic bodies to have a positive mass.
         Self.m_mass    := 1.0;
         Self.m_invMass := 1.0;
      end if;

      if Self.m_I > 0.0 and then (Self.m_flags and e_fixedRotationFlag) = 0 then
         --  Center the inertia about the center of mass.
         Self.m_I    := Self.m_I - Self.m_mass * b2Dot (center, center);      pragma Assert (Self.m_I > 0.0);
         Self.m_invI := 1.0 / Self.m_I;
      else
         Self.m_I    := 0.0;
         Self.m_invI := 0.0;
      end if;

      --  Move center of mass.
      oldCenter                := Self.m_sweep.c;
      Self.m_sweep.localCenter := center;
      Self.m_sweep.c0          := b2Mul (Self.m_xf, Self.m_sweep.localCenter);
      Self.m_sweep.c           := Self.m_sweep.c0;

      --  Update center of mass velocity.
      Self.m_linearVelocity := Self.m_linearVelocity + b2Cross (Self.m_angularVelocity, Self.m_sweep.c - oldCenter);
   end ResetMassData;




   function GetLinearVelocity (Self : in item) return b2Vec2
   is
   begin
      return self.m_linearVelocity;
   end GetLinearVelocity;


   function GetAngularVelocity (Self : in item) return float32
   is
   begin
      return self.m_angularVelocity;
   end GetAngularVelocity;






   procedure SetSleepingAllowed (Self : in out item;   flag : in Boolean)
   is
   begin
      if flag then
         Self.m_flags := Self.m_flags or e_autoSleepFlag;
      else
         Self.m_flags := Self.m_flags and not e_autoSleepFlag;
         Self.SetAwake (True);
      end if;

   end SetSleepingAllowed;



   function IsSleepingAllowed (Self : in  item) return Boolean
   is
   begin
      return (Self.m_flags and e_autoSleepFlag) = e_autoSleepFlag;
   end IsSleepingAllowed;






   procedure SetAwake (Self : in out item;   flag : in Boolean)
   is
   begin
      if flag then
         if (Self.m_flags and e_awakeFlag) = 0 then
            Self.m_flags     := Self.m_flags or e_awakeFlag;
            Self.m_sleepTime := 0.0;
         end if;
      else
         Self.m_flags           := Self.m_flags and not e_awakeFlag;
         Self.m_sleepTime       := 0.0;
         Self.m_linearVelocity  := (0.0, 0.0); --.SetZero();
         Self.m_angularVelocity := 0.0;
         Self.m_force           := (0.0, 0.0); --.SetZero();
         Self.m_torque          := 0.0;
      end if;
   end SetAwake;



   function IsAwake (Self : in  item) return Boolean
   is
   begin
      return (Self.m_flags and e_awakeFlag) = e_awakeFlag;
   end IsAwake;










   function GetPosition (Self : in   item) return b2Vec2
   is
   begin
      return Self.m_xf.position;
   end GetPosition;



   function GetAngle (Self : in   item) return float32
   is
   begin
      return Self.m_sweep.a;
   end GetAngle;




   function GetWorldCenter (Self : in   item) return b2Vec2
   is
   begin
      return Self.m_sweep.c;
   end GetWorldCenter;


   function GetLocalCenter (Self : in   item) return b2Vec2
   is
   begin
      return Self.m_sweep.localCenter;
   end GetLocalCenter;




   procedure SetLinearVelocity (Self : in out item;   v : in b2Vec2)
   is
   begin
      if Self.m_type = b2_staticBody then
         return;
      end if;

      if b2Dot (v, v) > 0.0 then
         Self.SetAwake (True);
      end if;

      Self.m_linearVelocity := v;
   end SetLinearVelocity;



   procedure SetAngularVelocity (Self : in out item;   omega : in float32)
   is
   begin
      if Self.m_type = b2_staticBody then
         return;
      end if;

      if omega * omega > 0.0 then
         Self.SetAwake (True);
      end if;

      Self.m_angularVelocity := omega;
   end SetAngularVelocity;





   procedure ApplyForce (Self : in out item;   force : in b2Vec2;
                                                 point : in b2Vec2)
   is
   begin
      if Self.m_type /= b2_dynamicBody then
         return;
      end if;

      if not Self.IsAwake then
         Self.SetAwake (True);
      end if;

      Self.m_force  := Self.m_force  + force;
      Self.m_torque := Self.m_torque + b2Cross (point - Self.m_sweep.c, force);
   end ApplyForce;




   procedure ApplyTorque (Self : in out item;   torque : in float32)
   is
   begin
      if Self.m_type /= b2_dynamicBody then
         return;
      end if;

      if not Self.IsAwake then
         Self.SetAwake (True);
      end if;

      Self.m_torque := Self.m_torque + torque;
   end ApplyTorque;




   procedure ApplyLinearImpulse (Self : in out item;   impulse : in b2Vec2;
                                                         point   : in b2Vec2)
   is
   begin
      if Self.m_type /= b2_dynamicBody then
         return;
      end if;

      if not Self.IsAwake then
         Self.SetAwake (True);
      end if;

      Self.m_linearVelocity  := Self.m_linearVelocity  + Self.m_invMass * impulse;
      Self.m_angularVelocity := Self.m_angularVelocity + Self.m_invI * b2Cross (point - Self.m_sweep.c,  impulse);
   end ApplyLinearImpulse;





   procedure ApplyAngularImpulse (Self : in out item;   impulse : in float32)
   is
   begin
      if Self.m_type /= b2_dynamicBody then
         return;
      end if;

      if not Self.IsAwake then
         Self.SetAwake (True);
      end if;

      Self.m_angularVelocity := Self.m_angularVelocity + Self.m_invI * impulse;
   end ApplyAngularImpulse;






   function GetInertia (Self : in   item) return float32
   is
   begin
      return   Self.m_I
             + Self.m_mass * b2Dot (Self.m_sweep.localCenter,  Self.m_sweep.localCenter);
   end GetInertia;





   procedure GetMassData (Self : in    item;   data : access shape.mass_Data)
   is
   begin
      data.mass   := Self.m_mass;
      data.I      :=   Self.m_I
                     + Self.m_mass * b2Dot (Self.m_sweep.localCenter, Self.m_sweep.localCenter);
      data.center := Self.m_sweep.localCenter;
   end GetMassData;





   function GetLocalPoint (Self : in   item;   worldPoint : in b2Vec2) return b2Vec2
   is
   begin
      return b2MulT (Self.m_xf,  worldPoint);
   end GetLocalPoint;



   function GetLocalVector (Self : in   item;   worldVector : in b2Vec2) return b2Vec2
   is
   begin
      return b2MulT (Self.m_xf.R,  worldVector);
   end GetLocalVector;





   function GetLinearVelocityFromWorldPoint (Self : in   item;   worldPoint : in b2Vec2) return b2Vec2
   is
   begin
      return Self.m_linearVelocity + b2Cross (Self.m_angularVelocity,
                                              worldPoint - Self.m_sweep.c);
   end GetLinearVelocityFromWorldPoint;



   function GetLinearVelocityFromLocalPoint (Self : in   item;   localPoint : in b2Vec2) return b2Vec2
   is
   begin
      return Self.GetLinearVelocityFromWorldPoint (Self.GetWorldPoint (localPoint));
   end GetLinearVelocityFromLocalPoint;





   function GetLinearDamping (Self : in   item) return float32
   is
   begin
      return Self.m_linearDamping;
   end GetLinearDamping;



   procedure SetLinearDamping (Self : in out item;   linearDamping : in float32)
   is
   begin
      Self.m_linearDamping := linearDamping;
   end SetLinearDamping;




   function GetAngularDamping (Self : in   item) return float32
   is
   begin
      return Self.m_angularDamping;
   end GetAngularDamping;



   procedure SetAngularDamping (Self : in out item;   angularDamping : in float32)
   is
   begin
      Self.m_angularDamping := angularDamping;
   end SetAngularDamping;





   procedure SetBullet (Self : in out item;   flag : in Boolean)
   is
   begin
      if flag then
         Self.m_flags := Self.m_flags or e_bulletFlag;
      else
         Self.m_flags := Self.m_flags and not e_bulletFlag;
      end if;
   end SetBullet;



   function IsBullet (Self : in   item) return Boolean
   is
   begin
      return (Self.m_flags and e_bulletFlag) = e_bulletFlag;
   end IsBullet;






   function IsActive (Self : in   item) return Boolean
   is
   begin
      return (Self.m_flags and e_activeFlag) = e_activeFlag;
   end IsActive;



   procedure SetFixedRotation (Self : in out item;   flag : in Boolean)
   is
   begin
      if flag then
         Self.m_flags := Self.m_flags or e_fixedRotationFlag;
      else
         Self.m_flags := Self.m_flags and not e_fixedRotationFlag;
      end if;

      Self.ResetMassData;
   end SetFixedRotation;



   function IsFixedRotation (Self : in   item) return Boolean
   is
   begin
      return (Self.m_flags and e_fixedRotationFlag) = e_fixedRotationFlag;
   end IsFixedRotation;






   function get_Fixture (Self : access   item) return access Fixture.item
   is
   begin
      return Self.m_Fixture'Access;
   end get_Fixture;




   function GetJointList (Self : in   item) return access joint.b2JointEdge
   is
   begin
      return Self.m_jointList;
   end GetJointList;





   function GetNext (Self : in   item) return access item'Class
   is
   begin
      return Self.m_next;
   end GetNext;


   function GetUserData (Self : in   item) return access Any'Class
   is
   begin
      return Self.m_userData;
   end GetUserData;



   procedure SetUserData (Self : in out item;   data : access Any'Class)
   is
   begin
      Self.m_userData := data;
   end SetUserData;





   function GetWorld (Self : in   item) return access constant world.b2World'Class
   is
   begin
      return Self.m_world;
   end GetWorld;








   procedure Advance             (Self : in out item;   t : in float32)
   is
   begin
      --  Advance to the new safe time.
      Advance (Self.m_sweep, t);
      Self.m_sweep.c := Self.m_sweep.c0;
      Self.m_sweep.a := Self.m_sweep.a0;

      Self.SynchronizeTransform;
   end Advance;




   package body Forge
   is

      function to_b2Body (bd : in Definition;   world : access impact.d2.orbs.world.b2World'Class) return item
      is
         Self : item;
      begin
         pragma Assert (IsValid (bd.position));
         pragma Assert (IsValid (bd.linearVelocity));
         pragma Assert (b2IsValid (bd.angle));
         pragma Assert (b2IsValid (bd.angularVelocity));
         pragma Assert (b2IsValid (bd.inertiaScale)   and then bd.inertiaScale >= 0.0);
         pragma Assert (b2IsValid (bd.angularDamping) and then bd.angularDamping >= 0.0);
         pragma Assert (b2IsValid (bd.linearDamping)  and then bd.linearDamping >= 0.0);

         Self.m_flags := 0;

         if bd.bullet        then   Self.m_flags := Self.m_flags or e_bulletFlag;          end if;
         if bd.fixedRotation then   Self.m_flags := Self.m_flags or e_fixedRotationFlag;   end if;
         if bd.allowSleep    then   Self.m_flags := Self.m_flags or e_autoSleepFlag;       end if;
         if bd.awake         then   Self.m_flags := Self.m_flags or e_awakeFlag;           end if;
         if bd.active        then   Self.m_flags := Self.m_flags or e_activeFlag;          end if;

         Self.m_world := world;

         Self.m_xf.position := bd.position;
         Set (Self.m_xf.R,  bd.angle);

         Self.m_sweep.localCenter := (0.0, 0.0);
         Self.m_sweep.a0          := bd.angle;
         Self.m_sweep.a           := bd.angle;
         Self.m_sweep.c0          := b2Mul (Self.m_xf,  Self.m_sweep.localCenter);
         Self.m_sweep.c           := Self.m_sweep.c0;

         Self.m_jointList   := null;
         Self.m_contactList := null;
         Self.m_prev        := null;
         Self.m_next        := null;

         Self.m_linearVelocity  := bd.linearVelocity;
         Self.m_angularVelocity := bd.angularVelocity;

         Self.m_linearDamping  := bd.linearDamping;
         Self.m_angularDamping := bd.angularDamping;

         Self.m_force  := (0.0, 0.0);
         Self.m_torque := 0.0;

         Self.m_sleepTime := 0.0;

         Self.m_type := bd.kind;

         if Self.m_type = b2_dynamicBody then
            Self.m_mass := 1.0;
            Self.m_invMass := 1.0;
         else
            Self.m_mass := 0.0;
            Self.m_invMass := 0.0;
         end if;

         Self.m_I    := 0.0;
         Self.m_invI := 0.0;

         Self.m_userData := bd.userData;

         return Self;
      end to_b2Body;

   end Forge;




   function CreateFixture (Self : access   item;   def : in fixture.Definition) return Fixture.view
   is
      use type uint32;
      fixture : impact.d2.orbs.Fixture.item renames Self.m_Fixture;

   begin
      pragma Assert (not Self.m_world.IsLocked);

      if Self.m_world.IsLocked then
         return null;
      end if;

      --          b2BlockAllocator* allocator = &m_world.m_blockAllocator;

      --          void* memory = allocator.Allocate(sizeof(b2Fixture));
      fixture.Create (Self, def);

      if (Self.m_flags and e_activeFlag) /= 0 then
         --  b2BroadPhase* broadPhase = &m_world.m_contactManager.m_broadPhase;
         fixture.CreateProxy (Self.m_world.m_contactManager.all.m_broadPhase'Access,  Self.m_xf);
      end if;

      fixture.m_body_is (Self.all'Unchecked_Access);

      --  Adjust mass properties if needed.
      if fixture.getDensity > 0.0 then
         Self.ResetMassData;
      end if;

      --  Let the world know we have a new fixture. This will cause new contacts
      --  to be created at the beginning of the next time step.
      Self.m_world.m_flags.all := Self.m_world.m_flags.all or World.e_newFixture;

      return fixture'Access;
   end CreateFixture;





   function CreateFixture (Self : access   item;   shape   : in impact.d2.orbs.Shape.item;
                                                     density : in float32                ) return Fixture.view
   is
      def : aliased fixture.Definition;
   begin
      def.shape   := shape;
      def.density := density;

      return Self.CreateFixture (def);
   end CreateFixture;





   procedure DestroyFixture (Self : access item;   fixture : in out impact.d2.orbs.Fixture.view)
   is
      procedure free is new ada.Unchecked_Deallocation (impact.d2.orbs.Fixture.item'Class, impact.d2.orbs.Fixture.view);
   begin
      pragma Assert (not Self.m_world.IsLocked);

      if Self.m_world.IsLocked then
         return;
      end if;

      pragma Assert (fixture.getBody = Self);


      --  Destroy any contacts associated with the fixture.
      declare
         use type impact.d2.orbs.Fixture.view;
         edge : access contact.b2ContactEdge := Self.m_contactList;
         c    :        Contact.view;
      begin
         while edge /= null loop
            c    := edge.contact.all'Access;
            edge := edge.next;

            if fixture = c.GetFixtureA or else fixture = c.GetFixtureB then
               --  This destroys the contact and removes it from
               --  this body's contact list.
               Self.m_world.m_contactManager.Destroy (c);
            end if;
         end loop;
      end;

      --          b2BlockAllocator* allocator = &m_world.m_blockAllocator;

      if (Self.m_flags and e_activeFlag) /= 0 then
         pragma Assert (fixture.m_proxyId /= BroadPhase.e_nullProxy);
         --                  b2BroadPhase* broadPhase = &Self.m_world.m_contactManager.m_broadPhase;
         fixture.DestroyProxy (Self.m_world.m_contactManager.m_broadPhase'Access);
      else
         null;   -- pragma assert (fixture.m_proxyId = BroadPhase.e_nullProxy);
      end if;

      fixture.Destroy; --(allocator);
      fixture.m_body_is (null);
      fixture.destruct;
      --  allocator.Free(fixture, sizeof(b2Fixture));
      free (fixture);


      --  Reset the mass data.
      Self.ResetMassData;
   end DestroyFixture;




   procedure SetMassData (Self : in out item;   data : in shape.mass_Data)
   is
      oldCenter : b2Vec2;
   begin
      pragma Assert (Self.m_world.IsLocked = False);

      if Self.m_world.IsLocked then
         return;
      end if;

      if Self.m_type /= b2_dynamicBody then
         return;
      end if;

      Self.m_invMass := 0.0;
      Self.m_I       := 0.0;
      Self.m_invI    := 0.0;

      Self.m_mass    := data.mass;

      if Self.m_mass <= 0.0 then
         Self.m_mass := 1.0;
      end if;

      Self.m_invMass := 1.0 / Self.m_mass;

      if data.I > 0.0 and then (Self.m_flags and Solid.e_fixedRotationFlag) = 0 then
         Self.m_I    := Data.I - Self.m_mass * b2Dot (Data.center, Data.center);   pragma Assert (Self.m_I > 0.0);
         Self.m_invI := 1.0 / Self.m_I;
      end if;

      --  Move center of mass.
      oldCenter                := Self.m_sweep.c;
      Self.m_sweep.localCenter := Data.center;
      Self.m_sweep.c0          := b2Mul (Self.m_xf, Self.m_sweep.localCenter);
      Self.m_sweep.c           := Self.m_sweep.c0;

      --  Update center of mass velocity.
      Self.m_linearVelocity := Self.m_linearVelocity + b2Cross (Self.m_angularVelocity,  Self.m_sweep.c - oldCenter);
   end SetMassData;





   procedure SetTransform (Self : in out item;   position : in b2Vec2;
                                                   angle    : in float32)
   is
      use type Fixture.view;
      broadPhase : access impact.d2.orbs.Broadphase.b2BroadPhase;
   begin
      pragma Assert (not Self.m_world.IsLocked);

      if Self.m_world.IsLocked then
         return;
      end if;

      Set (Self.m_xf.R,  angle);
      Self.m_xf.position := position;

      Self.m_sweep.c0 := b2Mul (Self.m_xf, Self.m_sweep.localCenter);
      Self.m_sweep.c  := Self.m_sweep.c0;

      Self.m_sweep.a0 := angle;
      Self.m_sweep.a  := Self.m_sweep.a0;

      broadPhase := Self.m_world.m_contactManager.m_broadPhase'Access;

      Self.m_Fixture.Synchronize (broadPhase,  Self.m_xf, Self.m_xf);
      Self.m_world.m_contactManager.FindNewContacts;
   end SetTransform;



   procedure SynchronizeFixtures (Self : in out item)
   is
      use type Fixture.view;

      broadPhase : access impact.d2.orbs.Broadphase.b2BroadPhase;
      xf1        :        b2Transform;
   begin
      Set (xf1.R,  Self.m_sweep.a0);
      xf1.position := Self.m_sweep.c0 - b2Mul (xf1.R,  Self.m_sweep.localCenter);

      broadPhase := Self.m_world.m_contactManager.m_broadPhase'Access;

      Self.m_Fixture.Synchronize (broadPhase, xf1, Self.m_xf);
   end SynchronizeFixtures;







   procedure SetActive (Self : access item;   flag : in Boolean)
   is
      use type Fixture.view;
      broadPhase : access impact.d2.orbs.Broadphase.b2BroadPhase;
      f          :        Fixture.view;
      ce, ce0    : access contact.b2ContactEdge;
   begin
      if flag = Self.IsActive then
         return;
      end if;

      if flag then
         Self.m_flags := Self.m_flags or e_activeFlag;

         --  Create all proxies.
         broadPhase := Self.m_world.m_contactManager.m_broadPhase'Access;

         f := Self.m_Fixture'Access;
         f.CreateProxy (broadPhase, Self.m_xf);

         --  Contacts are created the next time step.
      else
         Self.m_flags := Self.m_flags and not e_activeFlag;

         --  Destroy all proxies.
         broadPhase := Self.m_world.m_contactManager.m_broadPhase'Access;

         f := Self.m_Fixture'Access;
         f.DestroyProxy (broadPhase);

         --  Destroy the attached contacts.
         ce := Self.m_contactList;
         while ce /= null loop
            ce0 := ce;
            ce  := ce.next;
            Self.m_world.m_contactManager.Destroy (Contact.view (ce0.contact));
         end loop;

         Self.m_contactList := null;
      end if;

   end SetActive;






   procedure SynchronizeTransform (Self : in out item)
   is
   begin
      Set (Self.m_xf.R,  Self.m_sweep.a);
      Self.m_xf.position := Self.m_sweep.c - b2Mul (Self.m_xf.R,
                                                    Self.m_sweep.localCenter);
   end SynchronizeTransform;







   --- 'protected' subprograms used by C 'friends'.
   --
   function m_angularVelocity (Self : access item'Class) return access float32
   is
   begin
      return  Self.m_angularVelocity'Unchecked_Access;
   end m_angularVelocity;



   function m_linearVelocity   (Self : access item'Class) return access b2Vec2
   is
   begin
      return  Self.m_linearVelocity'Unchecked_Access;
   end m_linearVelocity;





   function m_force   (Self : access item'Class) return access b2Vec2
   is
   begin
      return  Self.m_force'Access;
   end m_force;



   function m_torque           (Self : access item'Class) return access float32
   is
   begin
      return Self.m_torque'Access;
   end m_torque;



   procedure m_flags_are       (Self : in out item'Class;   Now : in uint16)
   is
   begin
      Self.m_flags := Now;
   end m_flags_are;




   function  m_flags           (Self : in     item'Class) return uint16
   is
   begin
      return Self.m_flags;
   end m_flags;


   function  m_sleepTime       (Self : access     item'Class) return access float32
   is
   begin
      return Self.m_sleepTime'Access;
   end m_sleepTime;




   function m_linearDamping    (Self : access item'Class) return access float32
   is
   begin
      return Self.m_linearDamping'Access;
   end m_linearDamping;



   function m_angularDamping   (Self : access item'Class) return access float32
   is
   begin
      return Self.m_angularDamping'Access;
   end m_angularDamping;



   function m_invMass (Self : access item'Class) return access float32
   is
   begin
      return  Self.m_invMass'Unchecked_Access;
   end m_invMass;



   function m_invI    (Self : access item'Class) return access float32
   is
   begin
      return  Self.m_invI'Unchecked_Access;
   end m_invI;



   function m_sweep   (Self : access item'Class) return access b2Sweep
   is
   begin
      return  Self.m_sweep'Unchecked_Access;
   end m_sweep;



   procedure m_contactList_is  (Self : in out  item'Class;   Now : access contact.b2ContactEdge)
   is
   begin
      Self.m_contactList := Now;
   end m_contactList_is;



   function  m_contactList     (Self : access item'Class)  return access contact.b2ContactEdge
   is
   begin
      return Self.m_contactList;
   end m_contactList;



   procedure m_islandIndex_is  (Self : in out item'Class;   Now : in int32)
   is
   begin
      Self.m_islandIndex := Now;
   end m_islandIndex_is;


   procedure m_world_is        (Self : in out item'Class;   Now : access world.b2World'Class)
   is
   begin
      Self.m_world := Now;
   end m_world_is;



   procedure m_next_is         (Self : in out item'Class;   Now : access item'Class)
   is
   begin
      Self.m_next := Now;
   end m_next_is;


   procedure m_prev_is         (Self : in out item'Class;   Now : access item'Class)
   is
   begin
      Self.m_prev := Now;
   end m_prev_is;



   function  m_prev            (Self : access item'Class) return access item'Class
   is
   begin
      return Self.m_prev;
   end m_prev;



   procedure m_jointList_is    (Self : in out item'Class;   Now : access joint.b2JointEdge)
   is
   begin
      Self.m_jointList := Now;
   end m_jointList_is;




   procedure m_fixtureList_is  (Self : in out item'Class;   Now : in Fixture.item)
   is
   begin
      Self.m_Fixture := Now;
   end m_fixtureList_is;






   function ShouldCollide   (Self : in item;   other : access constant item'Class) return Boolean
   is
      use type impact.d2.orbs.Joint.Solid_view;
      jn : access joint.b2JointEdge;
   begin
      --  At least one body should be dynamic.
      if         Self.m_type  /= b2_dynamicBody
        and then other.m_type /= b2_dynamicBody
      then
         return False;
      end if;

      --  Does a joint prevent collision?
      jn := Self.m_jointList;

      while jn /= null loop
         if impact.d2.orbs.Joint.Solid_view (jn.other) = other then
            if not jn.joint.m_collideConnected then
               return False;
            end if;
         end if;

         jn := jn.next;
      end loop;

      return True;
   end ShouldCollide;



end impact.d2.orbs.Solid;
