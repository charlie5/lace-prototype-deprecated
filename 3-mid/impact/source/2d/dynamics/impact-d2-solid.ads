with impact.d2.Contact,
     impact.d2.Fixture,
     impact.d2.Joint,
     impact.d2.Shape,
     impact.d2.Math;

limited
with impact.d2.World;



package impact.d2.Solid
--
--
--
is
   use impact.d2.Math;


   --  The body type.
   --  static: zero mass, zero velocity, may be manually moved
   --  kinematic: zero mass, non-zero velocity set by user, moved by solver
   --  dynamic: positive mass, non-zero velocity determined by forces, moved by solver
   --
   type b2BodyType is (b2_staticBody, b2_kinematicBody, b2_dynamicBody);



   --  A body definition holds all the data needed to construct a rigid body.
   --  You can safely re-use body definitions. Shapes are added to a body after construction.
   --
   type b2BodyDef is
      record
         --  The body type: static, kinematic, or dynamic.
         --  Note: if a dynamic body would have zero mass, the mass is set to one.
         kind            : b2BodyType := b2_staticBody;

         --  The world position of the body. Avoid creating bodies at the origin
         --  since this can lead to many overlapping shapes.
         position        : b2Vec2 := (0.0, 0.0);

         --  The world angle of the body in radians.
         angle           : float32 := 0.0;

         --  The linear velocity of the body's origin in world co-ordinates.
         linearVelocity  : b2Vec2 := (0.0, 0.0);

         --  The angular velocity of the body.
         angularVelocity : float32 := 0.0;

         --  Linear damping is use to reduce the linear velocity. The damping parameter
         --  can be larger than 1.0f but the damping effect becomes sensitive to the
         --  time step when the damping parameter is large.
         linearDamping   : float32 := 0.0;

         --  Angular damping is use to reduce the angular velocity. The damping parameter
         --  can be larger than 1.0f but the damping effect becomes sensitive to the
         --  time step when the damping parameter is large.
         angularDamping  : float32 := 0.0;

         --  Set this flag to false if this body should never fall asleep. Note that
         --  this increases CPU usage.
         allowSleep      : Boolean := True;

         --  Is this body initially awake or sleeping?
         awake           : Boolean := True;

         --  Should this body be prevented from rotating? Useful for characters.
         fixedRotation   : Boolean := False;

         --  Is this a fast moving body that should be prevented from tunneling through
         --  other moving bodies? Note that all bodies are prevented from tunneling through
         --  kinematic and static bodies. This setting is only considered on dynamic bodies.
         --  Warning: You should use this flag sparingly since it increases processing time.
         bullet          : Boolean := False;

         --  Does this body start out active?
         active          : Boolean := True;

         --  Use this to store application specific body data.
         userData        : access Any'Class;

         --  Experimental: scales the inertia tensor.
         inertiaScale    : float32 := 1.0;
      end record;






   --  A rigid body. These are created via 'World.CreateBody'.
   --
   type b2Body is tagged private;
   type View is access all b2Body'Class;




   --  Set the type of this body. This may alter the mass and velocity.
   --
   procedure SetType (Self : in out b2Body;   Kind : in b2BodyType);


   --  Get the type of this body.
   --
   function GetType (Self : in b2Body) return b2BodyType;




   --  Get the list of all contacts attached to this body.
   --  warning: this list changes during the time step and you may
   --          miss some collisions if you don't use b2ContactListener.
   --
   function GetContactList (Self : in b2Body) return access contact.b2ContactEdge;

--          b2ContactEdge* GetContactList();
--          const b2ContactEdge* GetContactList() const;



   --  Get the body transform for the body's origin.
   --  Returns the world transform of the body's origin.
   --
   function GetTransform (Self : in b2Body) return b2Transform;



--  Get the world coordinates of a point given the local coordinates.
--  'localPoint' is a point on the body measured relative the the body's origin.
--  Returns the same point expressed in world coordinates.
--
   function GetWorldPoint (Self : in b2Body;   localPoint : in b2Vec2) return b2Vec2;




   --  Get the world coordinates of a vector given the local coordinates.
   --  'localVector' is a vector fixed in the body.
   --  Return the same vector expressed in world coordinates.
   --
   function GetWorldVector (Self : in b2Body;   localVector : in b2Vec2) return b2Vec2;


   --  Get the linear velocity of the center of mass.
   --  Returns the linear velocity of the center of mass.
   --
   function GetLinearVelocity (Self : in b2Body) return b2Vec2;


   --  Get the angular velocity.
   --  Returns the angular velocity in radians/second.
   --
   function GetAngularVelocity (Self : in b2Body) return float32;



   --  Get the total mass of the body.
   --  Returns the mass, usually in kilograms (kg).
   --
   function GetMass (Self : in b2Body) return float32;





   --          You can disable sleeping on this body. If you disable sleeping, the
   --          body will be woken.
   --
   procedure SetSleepingAllowed (Self : in out b2Body;   flag : in Boolean);


   --  Is this body allowed to sleep
   --
   function IsSleepingAllowed (Self : in  b2Body) return Boolean;


   --  Set the sleep state of the body. A sleeping body has very
   --  low CPU cost.
   --  'flag' set to true to put body to sleep, false to wake it.
   --
   procedure SetAwake (Self : in out b2Body;   flag : in Boolean);


   --  Get the sleeping state of this body.
   --  Returns True if the body is sleeping.
   --
   function IsAwake (Self : in  b2Body) return Boolean;



   --  Set the mass properties to override the mass properties of the fixtures.
   --  Note that this changes the center of mass position.
   --  Note that creating or destroying fixtures can also alter the mass.
   --  This function has no effect if the body isn't dynamic.
   --  @param massData the mass properties.
   --
   procedure SetMassData (Self : in out b2Body;   data : in shape.b2MassData);


   --  This resets the mass properties to the sum of the mass properties of the fixtures.
   --  This normally does not need to be called unless you called SetMassData to override
   --  the mass and you later want to reset the mass.
   --
   procedure ResetMassData (Self : in out b2Body);


   --  Creates a fixture and attach it to this body. Use this function if you need
   --  to set some fixture parameters, like friction. Otherwise you can create the
   --  fixture directly from a shape.
   --  If the density is non-zero, this function automatically updates the mass of the body.
   --  Contacts are not created until the next time step.
   --  'def' the fixture definition.
   --  Warning: This function is locked during callbacks.
   --
   function CreateFixture (Self : access   b2Body;   def : in fixture.b2FixtureDef) return Fixture.view;


   --  Creates a fixture from a shape and attach it to this body.
   --  This is a convenience function. Use b2FixtureDef if you need to set parameters
   --  like friction, restitution, user data, or filtering.
   --  If the density is non-zero, this function automatically updates the mass of the body.
   --  @param shape the shape to be cloned.
   --  @param density the shape density (set to zero for static bodies).
   --  @warning This function is locked during callbacks.
   --
   function CreateFixture (Self : access   b2Body;   shape : in impact.d2.Shape.View;
                                                     density : in float32     ) return Fixture.view;



   --  Destroy a fixture. This removes the fixture from the broad-phase and
   --  destroys all contacts associated with this fixture. This will
   --  automatically adjust the mass of the body if the body is dynamic and the
   --  fixture has positive density.
   --  All fixtures attached to a body are implicitly destroyed when the body is destroyed.
   --  @param fixture the fixture to be removed.
   --  @warning This function is locked during callbacks.
   --
   procedure DestroyFixture (Self : access b2Body;   fixture : in out impact.d2.Fixture.view);




   --  Set the position of the body's origin and rotation.
   --  This breaks any contacts and wakes the other bodies.
   --  Manipulating a body's transform may cause non-physical behavior.
   --  @param position the world position of the body's local origin.
   --  @param angle the world rotation in radians.
   --
   procedure SetTransform (Self : in out b2Body;   position : in b2Vec2;
                                                   angle    : in float32);



   --  Get the world body origin position.
   --  @return the world position of the body's origin.
   --
   function GetPosition (Self : in   b2Body) return b2Vec2;


   --  Get the angle in radians.
   --  @return the current world rotation angle in radians.
   --
   function GetAngle (Self : in   b2Body) return float32;



   --  Get the world position of the center of mass.
   --
   function GetWorldCenter (Self : in   b2Body) return b2Vec2;


   --  Get the local position of the center of mass.
   --
   function GetLocalCenter (Self : in   b2Body) return b2Vec2;


   --  Set the linear velocity of the center of mass.
   --  @param v the new linear velocity of the center of mass.
   --
   procedure SetLinearVelocity (Self : in out b2Body;   v : in b2Vec2);


   --  Set the angular velocity.
   --  @param omega the new angular velocity in radians/second.
   --
   procedure SetAngularVelocity (Self : in out b2Body;   omega : in float32);



   --  Apply a force at a world point. If the force is not
   --  applied at the center of mass, it will generate a torque and
   --  affect the angular velocity. This wakes up the body.
   --  @param force the world force vector, usually in Newtons (N).
   --  @param point the world position of the point of application.
   --
   procedure ApplyForce (Self : in out b2Body;   force : in b2Vec2;
                                                 point : in b2Vec2);


   --  Apply a torque. This affects the angular velocity
   --  without affecting the linear velocity of the center of mass.
   --  This wakes up the body.
   --  @param torque about the z-axis (out of the screen), usually in N-m.
   --
   procedure ApplyTorque (Self : in out b2Body;   torque : in float32);



   --  Apply an impulse at a point. This immediately modifies the velocity.
   --  It also modifies the angular velocity if the point of application
   --  is not at the center of mass. This wakes up the body.
   --  @param impulse the world impulse vector, usually in N-seconds or kg-m/s.
   --  @param point the world position of the point of application.
   --
   procedure ApplyLinearImpulse (Self : in out b2Body;   impulse : in b2Vec2;
                                                         point   : in b2Vec2);


   --  Apply an angular impulse.
   --  @param impulse the angular impulse in units of kg*m*m/s
   --
   procedure ApplyAngularImpulse (Self : in out b2Body;   impulse : in float32);


   --  Get the rotational inertia of the body about the local origin.
   --  @return the rotational inertia, usually in kg-m^2.
   --
   function GetInertia (Self : in   b2Body) return float32;


   --  Get the mass data of the body.
   --  @return a struct containing the mass, inertia and center of the body.
   --
   procedure GetMassData (Self : in    b2Body;   data : access shape.b2MassData);




   --  Gets a local point relative to the body's origin given a world point.
   --  @param a point in world coordinates.
   --  @return the corresponding local point relative to the body's origin.
   --
   function GetLocalPoint (Self : in   b2Body;   worldPoint : in b2Vec2) return b2Vec2;


   --  Gets a local vector given a world vector.
   --  @param a vector in world coordinates.
   --  @return the corresponding local vector.
   --
   function GetLocalVector (Self : in   b2Body;   worldVector : in b2Vec2) return b2Vec2;


   --  Get the world linear velocity of a world point attached to this body.
   --  @param a point in world coordinates.
   --  @return the world velocity of a point.
   --
   function GetLinearVelocityFromWorldPoint (Self : in   b2Body;   worldPoint : in b2Vec2) return b2Vec2;


   --  Get the world velocity of a local point.
   --  @param a point in local coordinates.
   --  @return the world velocity of a point.
   --
   function GetLinearVelocityFromLocalPoint (Self : in   b2Body;   localPoint : in b2Vec2) return b2Vec2;


   --  Get the linear damping of the body.
   --
   function GetLinearDamping (Self : in   b2Body) return float32;


   --  Set the linear damping of the body.
   --
   procedure SetLinearDamping (Self : in out b2Body;   linearDamping : in float32);


   --  Get the angular damping of the body.
   --
   function GetAngularDamping (Self : in   b2Body) return float32;


   --  Set the angular damping of the body.
   --
   procedure SetAngularDamping (Self : in out b2Body;   angularDamping : in float32);


   --  Should this body be treated like a bullet for continuous collision detection?
   --
   procedure SetBullet (Self : in out b2Body;   flag : in Boolean);


   --  Is this body treated like a bullet for continuous collision detection?
   --
   function IsBullet (Self : in   b2Body) return Boolean;


   --  Set the active state of the body. An inactive body is not
   --  simulated and cannot be collided with or woken up.
   --  If you pass a flag of true, all fixtures will be added to the
   --  broad-phase.
   --  If you pass a flag of false, all fixtures will be removed from
   --  the broad-phase and all contacts will be destroyed.
   --  Fixtures and joints are otherwise unaffected. You may continue
   --  to create/destroy fixtures and joints on inactive bodies.
   --  Fixtures on an inactive body are implicitly inactive and will
   --  not participate in collisions, ray-casts, or queries.
   --  Joints connected to an inactive body are implicitly inactive.
   --  An inactive body is still owned by a b2World object and remains
   --  in the body list.
   --
   procedure SetActive (Self : in out b2Body;   flag : in Boolean);


   --  Get the active state of the body.
   --
   function IsActive (Self : in   b2Body) return Boolean;


   --  Set this body to have fixed rotation. This causes the mass to be reset.
   --
   procedure SetFixedRotation (Self : in out b2Body;   flag : in Boolean);


   --  Does this body have fixed rotation?
   --
   function IsFixedRotation (Self : in   b2Body) return Boolean;


   --  Get the list of all fixtures attached to this body.
   --
   function GetFixtureList (Self : in   b2Body) return access Fixture.b2Fixture;



   --  Get the list of all joints attached to this body.
   --
   function GetJointList (Self : in   b2Body) return access joint.b2JointEdge;


   --  Get the next body in the world's body list.
   --
   function GetNext (Self : in   b2Body) return access b2Body'Class;


   --  Get the user data pointer that was provided in the body definition.
   --
   function GetUserData (Self : in   b2Body) return access Any'Class;



   --  Set the user data. Use this to store your application specific data.
   --
   procedure SetUserData (Self : in out b2Body;   data : access Any'Class);


   --  Get the parent world of this body.
   --
   function GetWorld (Self : in   b2Body) return access constant world.b2World'Class;









   --- 'protected' subprograms used by C 'friends'.

   function m_invMass          (Self : access b2Body'Class) return access float32;
   function m_invI             (Self : access b2Body'Class) return access float32;
   function m_sweep            (Self : access b2Body'Class) return access b2Sweep;
   function m_angularVelocity  (Self : access b2Body'Class) return access float32;
   function m_linearVelocity   (Self : access b2Body'Class) return access b2Vec2;

   function m_force            (Self : access b2Body'Class) return access b2Vec2;
   function m_torque           (Self : access b2Body'Class) return access float32;

   function m_linearDamping    (Self : access b2Body'Class) return access float32;
   function m_angularDamping   (Self : access b2Body'Class) return access float32;

   function  m_contactList     (Self : access b2Body'Class)  return access contact.b2ContactEdge;
   procedure m_contactList_is  (Self : in out b2Body'Class;   Now : access contact.b2ContactEdge);

   function  m_islandIndex     (Self : in     b2Body'Class)         return int32;
   procedure m_islandIndex_is  (Self : in out b2Body'Class;   Now : in     int32);

   procedure m_next_is         (Self : in out b2Body'Class;   Now : access b2Body'Class);

   procedure m_prev_is         (Self : in out b2Body'Class;   Now : access b2Body'Class);
   function  m_prev            (Self : access b2Body'Class) return access b2Body'Class;

   procedure m_flags_are       (Self : in out b2Body'Class;   Now : in uint16);
   function  m_flags           (Self : in     b2Body'Class) return uint16;

   function  m_sleepTime       (Self : access b2Body'Class) return access float32;

   procedure m_jointList_is    (Self : in out b2Body'Class;   Now : access joint.b2JointEdge);

   procedure m_fixtureList_is  (Self : in out b2Body'Class;   Now : in Fixture.view);
   procedure m_fixtureCount_is (Self : in out b2Body'Class;   Now : in int32);

   procedure m_world_is        (Self : in out b2Body'Class;   Now : access world.b2World'Class);





   procedure SynchronizeTransform (Self : in out b2Body);
   procedure SynchronizeFixtures  (Self : in out b2Body);

   --  This is used to prevent connected bodies from colliding.
   --  It may lie, depending on the collideConnected flag.
   --
   function ShouldCollide   (Self : in b2Body;   other : access constant b2Body'Class) return Boolean;



   subtype Flag is uint16;

   e_islandFlag               : constant Flag := 16#0001#;
   e_awakeFlag               : constant Flag := 16#0002#;
   e_autoSleepFlag     : constant Flag := 16#0004#;
   e_bulletFlag               : constant Flag := 16#0008#;
   e_fixedRotationFlag : constant Flag := 16#0010#;
   e_activeFlag               : constant Flag := 16#0020#;
   e_toiFlag           : constant Flag := 16#0040#;


   package Forge
   is
      function to_b2Body (bd : in b2BodyDef;   world : access impact.d2.world.b2World'Class) return b2Body;
   end Forge;


   procedure destruct            (Self : in out b2Body) is null;   -- shapes and joints are destroyed in 'World.destroy'

   procedure Advance             (Self : in out b2Body;   t : in float32);




private


   type b2Body is tagged
      record
         m_type            : b2BodyType;
         m_flags           : uint16;
         m_islandIndex     : int32;

         m_xf              :         b2Transform;    -- the body origin transform
         m_sweep           : aliased b2Sweep;             -- the swept motion for CCD

         m_linearVelocity  : aliased b2Vec2;
         m_angularVelocity : aliased float32;

         m_force           : aliased b2Vec2;
         m_torque          : aliased float32;

         m_world           : access World.b2World'Class;
         m_prev            : access b2Body'Class;
         m_next            : access b2Body'Class;

         m_fixtureList     : aliased Fixture.view;
         m_fixtureCount    : int32;

         m_jointList       : access joint.b2JointEdge;
         m_contactList     : access contact.b2ContactEdge;

         m_mass,
         m_invMass         : aliased float32;

         --  Rotational inertia about the center of mass.
         m_I,
         m_invI            : aliased float32;

         m_linearDamping   : aliased float32;
         m_angularDamping  : aliased float32;

         m_sleepTime       : aliased float32;

         m_userData        : access Any'Class;
      end record;





end impact.d2.Solid;
