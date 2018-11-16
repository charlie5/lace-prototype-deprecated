with impact.d2.orbs.Collision,
     impact.d2.orbs.contact.Manager,
     impact.d2.orbs.world_Callbacks,
     impact.d2.orbs.Solid,
     impact.d2.orbs.Joint,
     impact.d2.Math;




package impact.d2.orbs.World
--
--
--
is
   use impact.d2.Math;



   --  The world class manages all physics entities, dynamic simulation,
   --  and asynchronous queries. The world also contains efficient memory
   --  management facilities.
   --
   type b2World is tagged private;


   --  Construct a world object.
   --  @param gravity the world gravity vector.
   --  @param doSleep improve performance by not simulating inactive bodies.
   --
   function to_b2World (gravity : in b2Vec2;   doSleep : in Boolean) return b2World;

   --  Destruct the world. All physics entities are destroyed and all heap memory is released.
   --
   procedure destruct (Self : in out b2World);




   --  Is the world locked (in the middle of a time step).
   --
   function IsLocked (Self : in    b2World) return Boolean;


   --  Register a destruction listener. The listener is owned by you and must
   --  remain in scope.
   --
   procedure SetDestructionListener (Self : in out b2World;   listener : access world_callbacks.b2DestructionListener'Class);


   --  Register a contact filter to provide specific control over collision.
   --  Otherwise the default filter is used (b2_defaultFilter). The listener is
   --  owned by you and must remain in scope.
   --
   procedure SetContactFilter (Self : in out b2World;   filter : access world_callbacks.b2ContactFilter'Class);


   --  Register a contact event listener. The listener is owned by you and must
   --  remain in scope.
   --
   procedure SetContactListener (Self : in out b2World;   listener : access world_callbacks.b2ContactListener'Class);



   --  Create a rigid body given a definition. No reference to the definition
   --  is retained.
   --  @warning This function is locked during callbacks.
   --
   function CreateBody (Self : access b2World;   def : in solid.Definition) return Solid.view;

   procedure add (Self : in out b2World;   the_Object : access impact.d2.orbs.Solid.item'Class);


   --  Destroy a rigid body given a definition. No reference to the definition
   --  is retained. This function is locked during callbacks.
   --  @warning This automatically deletes all associated shapes and joints.
   --  @warning This function is locked during callbacks.
   --
   procedure DestroyBody (Self : in out b2World;   solid : access impact.d2.orbs.Solid.item'Class);



   --  Create a joint to constrain bodies together. No reference to the definition
   --  is retained. This may cause the connected bodies to cease colliding.
   --  @warning This function is locked during callbacks.
   --
   function CreateJoint (Self : access b2World;   def : in joint.b2JointDef) return access joint.b2Joint'Class;


   --  Destroy a joint. This may cause the connected bodies to begin colliding.
   --  @warning This function is locked during callbacks.
   --
   procedure DestroyJoint (Self : in out b2World;   joint : access impact.d2.orbs.Joint.b2Joint'Class);



   --  Take a time step. This performs collision detection, integration,
   --  and constraint solution.
   --  @param timeStep the amount of time to simulate, this should not vary.
   --  @param velocityIterations for the velocity constraint solver.
   --  @param positionIterations for the position constraint solver.
   --
   procedure Step (Self : in out b2World;   timeStep           : in float32;
                                            velocityIterations : in int32;
                                            positionIterations : in int32);



   --  Call this after you are done with time steps to clear the forces. You normally
   --  call this after each call to Step, unless you are performing sub-steps. By default,
   --  forces will be automatically cleared, so you don't need to call this function.
   --  @see SetAutoClearForces
   --
   procedure ClearForces (Self : in out b2World);


   --  Query the world for all fixtures that potentially overlap the
   --  provided AABB.
   --  @param callback a user implemented callback class.
   --  @param aabb the query box.
   --
   procedure QueryAABB (Self : access b2World;   callback : access world_callbacks.b2QueryCallback;
                                                 aabb     : in     collision.b2AABB);


   --  Ray-cast the world for all fixtures in the path of the ray. Your callback
   --  controls whether you get the closest point, any point, or n-points.
   --  The ray-cast ignores shapes that contain the starting point.
   --  @param callback a user implemented callback class.
   --  @param point1 the ray starting point
   --  @param point2 the ray ending point
   --
   procedure RayCast (Self : access b2World;   callback       : access world_callbacks.b2RayCastCallback;
                                               point1, point2 : in     b2Vec2);


   --  Get the world body list. With the returned body, use b2Body::GetNext to get
   --  the next body in the world list. A NULL body indicates the end of the list.
   --  @return the head of the world body list.
   --
   function GetBodyList (Self : in b2World) return access Solid.item'Class;


   --  Get the world joint list. With the returned joint, use b2Joint::GetNext to get
   --  the next joint in the world list. A NULL joint indicates the end of the list.
   --  @return the head of the world joint list.
   --
   function GetJointList (Self : in b2World) return access Joint.b2Joint'Class;


   --  Get the world contact list. With the returned contact, use b2Contact::GetNext to get
   --  the next contact in the world list. A NULL contact indicates the end of the list.
   --  @return the head of the world contact list.
   --  @warning contacts are
   --
   function GetContactList (Self : in b2World) return access Contact.b2Contact'Class;


   --  Enable/disable warm starting. For testing.
   --
   procedure SetWarmStarting (Self : in out b2World;   flag : in Boolean);


   --  Enable/disable continuous physics. For testing.
   --
   procedure SetContinuousPhysics (Self : in out b2World;   flag : in Boolean);



   --  Get the number of broad-phase proxies.
   --
   function GetProxyCount (Self : in b2World) return int32;


   --  Get the number of bodies.
   --
   function GetBodyCount (Self : in b2World) return int32;


   --  Get the number of joints.
   --
   function GetJointCount (Self : in b2World) return int32;

   --  Get the number of contacts (each may have 0 or more contact points).
   --
   function GetContactCount (Self : in b2World) return int32;


   --  Change the global gravity vector.
   --
   procedure SetGravity (Self : in out b2World;   gravity : in b2Vec2);


   --  Get the global gravity vector.
   --
   function GetGravity (Self : in b2World) return b2Vec2;


   --  Set flag to control automatic clearing of forces after each time step.
   --
   procedure SetAutoClearForces (Self : in out b2World;   flag : in Boolean);


   --  Get the flag that controls automatic clearing of forces after each time step.
   --
   function GetAutoClearForces (Self : in b2World) return Boolean;








   --  'protected' subprograms for use by C 'friend's.
   --

   --  m_flags
   e_newFixture  : constant uint32 := 16#0001#;
   e_locked      : constant uint32 := 16#0002#;
   e_clearForces : constant uint32 := 16#0004#;



   function m_contactManager (Self : access b2World) return access contact.manager.b2ContactManager;
   function m_flags          (Self : access b2World) return access uint32;





private


   type b2World is tagged
      record
--          b2BlockAllocator m_blockAllocator;
--          b2StackAllocator m_stackAllocator;

        m_flags : aliased uint32;

        m_contactManager : aliased contact.manager.b2ContactManager;

        m_bodyList  : access Solid.item'Class;
        m_jointList : Joint.view;

        m_bodyCount : int32;
        m_jointCount : int32;

        m_gravity : b2Vec2;
        m_allowSleep : Boolean;

        m_groundBody : access Solid.item'Class;

        m_destructionListener : access world_callbacks.b2DestructionListener'Class;
--          b2DebugDraw* m_debugDraw;

        m_inv_dt0           : float32;    -- This is used to compute the time step ratio to support a variable time step.

        m_warmStarting      : Boolean;    -- This is for debugging the solver.
        m_continuousPhysics : Boolean;    -- This is for debugging the solver.
      end record;



   procedure Solve    (Self : in out b2World;   step : in b2TimeStep);
   procedure SolveTOI (Self : in out b2World);
   procedure SolveTOI (Self : in out b2World;   solid : access impact.d2.orbs.Solid.item'Class);



--          void DrawJoint(b2Joint* joint);
--          void DrawShape(b2Fixture* shape, const b2Transform& xf, const b2Color& color);






end impact.d2.orbs.World;
