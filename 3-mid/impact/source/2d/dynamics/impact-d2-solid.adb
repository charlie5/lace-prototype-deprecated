with impact.d2.World,
     impact.d2.Broadphase,

     ada.unchecked_Deallocation;



package body impact.d2.Solid
is
   use type int32, uint16;




   procedure SetType (Self : in out b2Body;   Kind : in b2BodyType)
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





   function GetType (Self : in b2Body) return b2BodyType
   is
   begin
      return Self.m_type;
   end GetType;





   function GetContactList (Self : in b2Body) return access contact.b2ContactEdge
   is
   begin
      return Self.m_contactList;
   end GetContactList;



   function GetTransform (Self : in b2Body) return b2Transform
   is
   begin
      return Self.m_xf;
   end GetTransform;




   function GetWorldPoint (Self : in b2Body;   localPoint : in b2Vec2) return b2Vec2
   is
   begin
      return b2Mul (Self.m_xf, localPoint);
   end GetWorldPoint;



   function GetWorldVector (Self : in b2Body;   localVector : in b2Vec2) return b2Vec2
   is
   begin
      return b2Mul (Self.m_xf.R, localVector);
   end GetWorldVector;



   function GetMass (Self : in b2Body) return float32
   is
   begin
      return self.m_mass;
   end GetMass;



   procedure ResetMassData (Self : in out b2Body)
   is
      center,
      oldCenter :         b2Vec2;
      massData  : aliased shape.b2MassData;
      f         : access  Fixture.b2Fixture;
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
      f      := Self.m_fixtureList;

      while f /= null loop
         if f.getDensity /= 0.0 then
            f.GetMassData (massData'Access);
            Self.m_mass := Self.m_mass + massData.mass;
            center      := center      + massData.mass * massData.center;
            Self.m_I    := Self.m_I    + massData.I;
         end if;

         f := f.getNext;
      end loop;

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




   function GetLinearVelocity (Self : in b2Body) return b2Vec2
   is
   begin
      return self.m_linearVelocity;
   end GetLinearVelocity;


   function GetAngularVelocity (Self : in b2Body) return float32
   is
   begin
      return self.m_angularVelocity;
   end GetAngularVelocity;






   procedure SetSleepingAllowed (Self : in out b2Body;   flag : in Boolean)
   is
   begin
      if flag then
         Self.m_flags := Self.m_flags or e_autoSleepFlag;
      else
         Self.m_flags := Self.m_flags and not e_autoSleepFlag;
         Self.SetAwake (True);
      end if;

   end SetSleepingAllowed;



   function IsSleepingAllowed (Self : in  b2Body) return Boolean
   is
   begin
      return (Self.m_flags and e_autoSleepFlag) = e_autoSleepFlag;
   end IsSleepingAllowed;






   procedure SetAwake (Self : in out b2Body;   flag : in Boolean)
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



   function IsAwake (Self : in  b2Body) return Boolean
   is
   begin
      return (Self.m_flags and e_awakeFlag) = e_awakeFlag;
   end IsAwake;










   function GetPosition (Self : in   b2Body) return b2Vec2
   is
   begin
      return Self.m_xf.position;
   end GetPosition;



   function GetAngle (Self : in   b2Body) return float32
   is
   begin
      return Self.m_sweep.a;
   end GetAngle;




   function GetWorldCenter (Self : in   b2Body) return b2Vec2
   is
   begin
      return Self.m_sweep.c;
   end GetWorldCenter;


   function GetLocalCenter (Self : in   b2Body) return b2Vec2
   is
   begin
      return Self.m_sweep.localCenter;
   end GetLocalCenter;




   procedure SetLinearVelocity (Self : in out b2Body;   v : in b2Vec2)
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



   procedure SetAngularVelocity (Self : in out b2Body;   omega : in float32)
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





   procedure ApplyForce (Self : in out b2Body;   force : in b2Vec2;
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




   procedure ApplyTorque (Self : in out b2Body;   torque : in float32)
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




   procedure ApplyLinearImpulse (Self : in out b2Body;   impulse : in b2Vec2;
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





   procedure ApplyAngularImpulse (Self : in out b2Body;   impulse : in float32)
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






   function GetInertia (Self : in   b2Body) return float32
   is
   begin
      return   Self.m_I
             + Self.m_mass * b2Dot (Self.m_sweep.localCenter,  Self.m_sweep.localCenter);
   end GetInertia;





   procedure GetMassData (Self : in    b2Body;   data : access shape.b2MassData)
   is
   begin
      data.mass   := Self.m_mass;
      data.I      :=   Self.m_I
                     + Self.m_mass * b2Dot (Self.m_sweep.localCenter, Self.m_sweep.localCenter);
      data.center := Self.m_sweep.localCenter;
   end GetMassData;





   function GetLocalPoint (Self : in   b2Body;   worldPoint : in b2Vec2) return b2Vec2
   is
   begin
      return b2MulT (Self.m_xf,  worldPoint);
   end GetLocalPoint;



   function GetLocalVector (Self : in   b2Body;   worldVector : in b2Vec2) return b2Vec2
   is
   begin
      return b2MulT (Self.m_xf.R,  worldVector);
   end GetLocalVector;





   function GetLinearVelocityFromWorldPoint (Self : in   b2Body;   worldPoint : in b2Vec2) return b2Vec2
   is
   begin
      return Self.m_linearVelocity + b2Cross (Self.m_angularVelocity,
                                              worldPoint - Self.m_sweep.c);
   end GetLinearVelocityFromWorldPoint;



   function GetLinearVelocityFromLocalPoint (Self : in   b2Body;   localPoint : in b2Vec2) return b2Vec2
   is
   begin
      return Self.GetLinearVelocityFromWorldPoint (Self.GetWorldPoint (localPoint));
   end GetLinearVelocityFromLocalPoint;





   function GetLinearDamping (Self : in   b2Body) return float32
   is
   begin
      return Self.m_linearDamping;
   end GetLinearDamping;



   procedure SetLinearDamping (Self : in out b2Body;   linearDamping : in float32)
   is
   begin
      Self.m_linearDamping := linearDamping;
   end SetLinearDamping;




   function GetAngularDamping (Self : in   b2Body) return float32
   is
   begin
      return Self.m_angularDamping;
   end GetAngularDamping;



   procedure SetAngularDamping (Self : in out b2Body;   angularDamping : in float32)
   is
   begin
      Self.m_angularDamping := angularDamping;
   end SetAngularDamping;





   procedure SetBullet (Self : in out b2Body;   flag : in Boolean)
   is
   begin
      if flag then
         Self.m_flags := Self.m_flags or e_bulletFlag;
      else
         Self.m_flags := Self.m_flags and not e_bulletFlag;
      end if;
   end SetBullet;



   function IsBullet (Self : in   b2Body) return Boolean
   is
   begin
      return (Self.m_flags and e_bulletFlag) = e_bulletFlag;
   end IsBullet;






   function IsActive (Self : in   b2Body) return Boolean
   is
   begin
      return (Self.m_flags and e_activeFlag) = e_activeFlag;
   end IsActive;



   procedure SetFixedRotation (Self : in out b2Body;   flag : in Boolean)
   is
   begin
      if flag then
         Self.m_flags := Self.m_flags or e_fixedRotationFlag;
      else
         Self.m_flags := Self.m_flags and not e_fixedRotationFlag;
      end if;

      Self.ResetMassData;
   end SetFixedRotation;



   function IsFixedRotation (Self : in   b2Body) return Boolean
   is
   begin
      return (Self.m_flags and e_fixedRotationFlag) = e_fixedRotationFlag;
   end IsFixedRotation;






   function GetFixtureList (Self : in   b2Body) return access Fixture.b2Fixture
   is
   begin
      return Self.m_fixtureList;
   end GetFixtureList;




   function GetJointList (Self : in   b2Body) return access joint.b2JointEdge
   is
   begin
      return Self.m_jointList;
   end GetJointList;





   function GetNext (Self : in   b2Body) return access b2Body'Class
   is
   begin
      return Self.m_next;
   end GetNext;


   function GetUserData (Self : in   b2Body) return access Any'Class
   is
   begin
      return Self.m_userData;
   end GetUserData;



   procedure SetUserData (Self : in out b2Body;   data : access Any'Class)
   is
   begin
      Self.m_userData := data;
   end SetUserData;





   function GetWorld (Self : in   b2Body) return access constant world.b2World'Class
   is
   begin
      return Self.m_world;
   end GetWorld;








   procedure Advance             (Self : in out b2Body;   t : in float32)
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

      function to_b2Body (bd : in b2BodyDef;   world : access impact.d2.world.b2World'Class) return b2Body
      is
         Self : b2Body;
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

         Self.m_fixtureList  := null;
         Self.m_fixtureCount := 0;

         return Self;
      end to_b2Body;

   end Forge;




   function CreateFixture (Self : access   b2Body;   def : in fixture.b2FixtureDef) return Fixture.view
   is
      use type uint32;
      fixture : impact.d2.Fixture.view;
   begin
      pragma Assert (not Self.m_world.IsLocked);

      if Self.m_world.IsLocked then
         return null;
      end if;

      --          b2BlockAllocator* allocator = &m_world.m_blockAllocator;

      --          void* memory = allocator.Allocate(sizeof(b2Fixture));
      fixture := new impact.d2.Fixture.b2Fixture;
      fixture.Create (Self, def);

      if (Self.m_flags and e_activeFlag) /= 0 then
         --  b2BroadPhase* broadPhase = &m_world.m_contactManager.m_broadPhase;
         fixture.CreateProxy (Self.m_world.m_contactManager.all.m_broadPhase'Access,  Self.m_xf);
      end if;

      fixture.m_next_is (Self.m_fixtureList);
      Self.m_fixtureList  := fixture;
      Self.m_fixtureCount := Self.m_fixtureCount + 1;

      fixture.m_body_is (Self.all'Unchecked_Access);

      --  Adjust mass properties if needed.
      if fixture.getDensity > 0.0 then
         Self.ResetMassData;
      end if;

      --  Let the world know we have a new fixture. This will cause new contacts
      --  to be created at the beginning of the next time step.
      Self.m_world.m_flags.all := Self.m_world.m_flags.all or World.e_newFixture;

      return fixture;
   end CreateFixture;





   function CreateFixture (Self : access   b2Body;   shape   : in impact.d2.Shape.View;
                                                     density : in float32       ) return Fixture.view
   is
      def : aliased fixture.b2FixtureDef;
   begin
      def.shape   := shape;
      def.density := density;

      return Self.CreateFixture (def);
   end CreateFixture;





   procedure DestroyFixture (Self : access b2Body;   fixture : in out impact.d2.Fixture.view)
   is
      procedure free is new ada.Unchecked_Deallocation (impact.d2.Fixture.b2Fixture'Class, impact.d2.Fixture.view);
   begin
      pragma Assert (not Self.m_world.IsLocked);

      if Self.m_world.IsLocked then
         return;
      end if;

      pragma Assert (fixture.getBody = Self);

      --  Remove the fixture from this body's singly linked list.
      pragma Assert (Self.m_fixtureCount > 0);

      declare
         use type impact.d2.Fixture.view;
         node  : impact.d2.Fixture.View := Self.m_fixtureList;   -- b2Fixture** node = &Self.m_fixtureList;
         found : Boolean := False;
      begin

         while node /= null loop
            if node = fixture then
               node  := fixture.getNext.all'Access;
               found := True;
               exit;
            end if;

            node := node.getNext.all'Access;
         end loop;

         --  You tried to remove a shape that is not attached to this body.
         pragma Assert (found);
      end;

      --  Destroy any contacts associated with the fixture.
      declare
         use type impact.d2.Fixture.view;
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
      fixture.m_next_is (null);
      fixture.destruct;
      --  allocator.Free(fixture, sizeof(b2Fixture));
      free (fixture);

      Self.m_fixtureCount := Self.m_fixtureCount - 1;

      --  Reset the mass data.
      Self.ResetMassData;
   end DestroyFixture;




   procedure SetMassData (Self : in out b2Body;   data : in shape.b2MassData)
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





   procedure SetTransform (Self : in out b2Body;   position : in b2Vec2;
                                                   angle    : in float32)
   is
      use type Fixture.view;
      broadPhase : access impact.d2.Broadphase.b2BroadPhase;
      f          :        Fixture.view;
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

      f := Self.m_fixtureList;
      while f /= null loop
         f.Synchronize (broadPhase,  Self.m_xf, Self.m_xf);
         f := Fixture.view (f.getNext);
      end loop;

      Self.m_world.m_contactManager.FindNewContacts;
   end SetTransform;



   procedure SynchronizeFixtures (Self : in out b2Body)
   is
      use type Fixture.view;
      broadPhase : access impact.d2.Broadphase.b2BroadPhase;
      f          :        Fixture.view;
      xf1        :        b2Transform;
   begin
      Set (xf1.R,  Self.m_sweep.a0);
      xf1.position := Self.m_sweep.c0 - b2Mul (xf1.R,  Self.m_sweep.localCenter);

      broadPhase := Self.m_world.m_contactManager.m_broadPhase'Access;

      f := Self.m_fixtureList;

      while f /= null loop
         f.Synchronize (broadPhase, xf1, Self.m_xf);
         f := Fixture.view (f.getNext);
      end loop;
   end SynchronizeFixtures;







   procedure SetActive (Self : in out b2Body;   flag : in Boolean)
   is
      use type Fixture.view;
      broadPhase : access impact.d2.Broadphase.b2BroadPhase;
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

         f := Self.m_fixtureList;

         while f /= null loop
            f.CreateProxy (broadPhase, Self.m_xf);
            f := Fixture.view (f.getNext);
         end loop;

         --  Contacts are created the next time step.
      else
         Self.m_flags := Self.m_flags and not e_activeFlag;

         --  Destroy all proxies.
         broadPhase := Self.m_world.m_contactManager.m_broadPhase'Access;

         f := Self.m_fixtureList;

         while f /= null loop
            f.DestroyProxy (broadPhase);
            f := Fixture.view (f.getNext);
         end loop;

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






   procedure SynchronizeTransform (Self : in out b2Body)
   is
   begin
      Set (Self.m_xf.R,  Self.m_sweep.a);
      Self.m_xf.position := Self.m_sweep.c - b2Mul (Self.m_xf.R,
                                                    Self.m_sweep.localCenter);
   end SynchronizeTransform;







   --- 'protected' subprograms used by C 'friends'.
   --
   function m_angularVelocity (Self : access b2Body'Class) return access float32
   is
   begin
      return  Self.m_angularVelocity'Unchecked_Access;
   end m_angularVelocity;



   function m_linearVelocity   (Self : access b2Body'Class) return access b2Vec2
   is
   begin
      return  Self.m_linearVelocity'Unchecked_Access;
   end m_linearVelocity;





   function m_force   (Self : access b2Body'Class) return access b2Vec2
   is
   begin
      return  Self.m_force'Access;
   end m_force;



   function m_torque           (Self : access b2Body'Class) return access float32
   is
   begin
      return Self.m_torque'Access;
   end m_torque;



   procedure m_flags_are       (Self : in out b2Body'Class;   Now : in uint16)
   is
   begin
      Self.m_flags := Now;
   end m_flags_are;




   function  m_flags           (Self : in     b2Body'Class) return uint16
   is
   begin
      return Self.m_flags;
   end m_flags;


   function  m_sleepTime       (Self : access     b2Body'Class) return access float32
   is
   begin
      return Self.m_sleepTime'Access;
   end m_sleepTime;




   function m_linearDamping    (Self : access b2Body'Class) return access float32
   is
   begin
      return Self.m_linearDamping'Access;
   end m_linearDamping;



   function m_angularDamping   (Self : access b2Body'Class) return access float32
   is
   begin
      return Self.m_angularDamping'Access;
   end m_angularDamping;



   function m_invMass (Self : access b2Body'Class) return access float32
   is
   begin
      return  Self.m_invMass'Unchecked_Access;
   end m_invMass;



   function m_invI    (Self : access b2Body'Class) return access float32
   is
   begin
      return  Self.m_invI'Unchecked_Access;
   end m_invI;



   function m_sweep   (Self : access b2Body'Class) return access b2Sweep
   is
   begin
      return  Self.m_sweep'Unchecked_Access;
   end m_sweep;



   procedure m_contactList_is  (Self : in out  b2Body'Class;   Now : access contact.b2ContactEdge)
   is
   begin
      Self.m_contactList := Now;
   end m_contactList_is;



   function  m_contactList     (Self : access b2Body'Class)  return access contact.b2ContactEdge
   is
   begin
      return Self.m_contactList;
   end m_contactList;


   function  m_islandIndex     (Self : in     b2Body'Class)         return int32
   is
   begin
      return Self.m_islandIndex;
   end m_islandIndex;


   procedure m_islandIndex_is  (Self : in out b2Body'Class;   Now : in int32)
   is
   begin
      Self.m_islandIndex := Now;
   end m_islandIndex_is;


   procedure m_world_is        (Self : in out b2Body'Class;   Now : access world.b2World'Class)
   is
   begin
      Self.m_world := Now;
   end m_world_is;



   procedure m_next_is         (Self : in out b2Body'Class;   Now : access b2Body'Class)
   is
   begin
      Self.m_next := Now;
   end m_next_is;


   procedure m_prev_is         (Self : in out b2Body'Class;   Now : access b2Body'Class)
   is
   begin
      Self.m_prev := Now;
   end m_prev_is;



   function  m_prev            (Self : access b2Body'Class) return access b2Body'Class
   is
   begin
      return Self.m_prev;
   end m_prev;



   procedure m_jointList_is    (Self : in out b2Body'Class;   Now : access joint.b2JointEdge)
   is
   begin
      Self.m_jointList := Now;
   end m_jointList_is;




   procedure m_fixtureList_is  (Self : in out b2Body'Class;   Now : in Fixture.view)
   is
   begin
      Self.m_fixtureList := Now;
   end m_fixtureList_is;



   procedure m_fixtureCount_is (Self : in out b2Body'Class;   Now : in int32)
   is
   begin
      Self.m_fixtureCount := Now;
   end m_fixtureCount_is;




   function ShouldCollide   (Self : in b2Body;   other : access constant b2Body'Class) return Boolean
   is
      use type impact.d2.Joint.Solid_view;
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
         if impact.d2.Joint.Solid_view (jn.other) = other then
            if not jn.joint.m_collideConnected then
               return False;
            end if;
         end if;

         jn := jn.next;
      end loop;

      return True;
   end ShouldCollide;



end impact.d2.Solid;
