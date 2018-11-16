with
     impact.d2.Collision,
     impact.d2.contact.Manager,
     impact.d2.world_Callbacks,
     impact.d2.Solid,
     impact.d2.Joint,
     impact.d2.Math;


package impact.d2.World
--
--  The world class manages all physics entities, dynamic simulation,
--  and asynchronous queries. The world also contains efficient memory
--  management facilities.
--
--
is
   use impact.d2.Math;


   type b2World is tagged private;


   ---------
   --  Forge

   function to_b2World (gravity : in b2Vec2) return b2World;
   --
   --  Construct a world object.
   --
   --  'gravity'    the world gravity vector.


   procedure destruct (Self : in out b2World);
   --
   --  Destruct the world. All physics entities are destroyed and all heap memory is released.



   --------------
   --  Attributes
   --

   procedure SetDestructionListener (Self : in out b2World;   listener : access world_callbacks.b2DestructionListener'Class);
   --
   --  Register a destruction listener. The listener is owned by you and must
   --  remain in scope.


   procedure SetContactFilter (Self : in out b2World;   filter : access world_callbacks.b2ContactFilter'Class);
   --
   --  Register a contact filter to provide specific control over collision.
   --  Otherwise the default filter is used (b2_defaultFilter). The listener is
   --  owned by you and must remain in scope.


   procedure SetContactListener (Self : in out b2World;   listener : access world_callbacks.b2ContactListener'Class);
   --
   --  Register a contact event listener. The listener is owned by you and must
   --  remain in scope.



   function CreateBody (Self : access b2World;   def : in solid.b2BodyDef) return access Solid.b2Body'Class;
   --
   --  Create a rigid body given a definition. No reference to the definition
   --  is retained.
   --  This function is locked during callbacks.


   procedure add (Self : in out b2World;   the_Object : access impact.d2.Solid.b2Body'Class);


   procedure DestroyBody (Self : in out b2World;   solid : access impact.d2.Solid.b2Body'Class);
   --
   --  Destroy a rigid body given a definition. No reference to the definition
   --  is retained. This function is locked during callbacks.
   --  This automatically deletes all associated shapes and joints.
   --  This function is locked during callbacks.



   function CreateJoint (Self : access b2World;   def : in joint.b2JointDef) return access joint.b2Joint'Class;
   --
   --  Create a joint to constrain bodies together. No reference to the definition
   --  is retained. This may cause the connected bodies to cease colliding.
   --  This function is locked during callbacks.
   --  Note: creating a joint doesn't wake the bodies.



   procedure DestroyJoint (Self : in out b2World;   joint : access impact.d2.Joint.b2Joint'Class);
   --
   --  Destroy a joint. This may cause the connected bodies to begin colliding.
   --  This function is locked during callbacks.



   procedure Step (Self : in out b2World;   timeStep           : in float32;
                                            velocityIterations : in int32;
                                            positionIterations : in int32);
   --
   --  Take a time step. This performs collision detection, integration,
   --  and constraint solution.
   --  @param timeStep the amount of time to simulate, this should not vary.
   --  @param velocityIterations for the velocity constraint solver.
   --  @param positionIterations for the position constraint solver.



   procedure ClearForces (Self : in out b2World);
   --
   --  Call this after you are done with time steps to clear the forces. You normally
   --  call this after each call to Step, unless you are performing sub-steps. By default,
   --  forces will be automatically cleared, so you don't need to call this function.
   --  @see SetAutoClearForces


   procedure QueryAABB (Self : access b2World;   callback : access world_callbacks.b2QueryCallback;
                                                 aabb     : in     collision.b2AABB);
   --
   --  Query the world for all fixtures that potentially overlap the
   --  provided AABB.
   --  @param callback a user implemented callback class.
   --  @param aabb the query box.


   procedure RayCast (Self : access b2World;   callback       : access world_callbacks.b2RayCastCallback;
                                               point1, point2 : in     b2Vec2);
   --
   --  Ray-cast the world for all fixtures in the path of the ray. Your callback
   --  controls whether you get the closest point, any point, or n-points.
   --  The ray-cast ignores shapes that contain the starting point.
   --  @param callback a user implemented callback class.
   --  @param point1 the ray starting point
   --  @param point2 the ray ending point


   function GetBodyList (Self : in b2World) return access Solid.b2Body'Class;
   --
   --  Get the world body list. With the returned body, use b2Body::GetNext to get
   --  the next body in the world list. A NULL body indicates the end of the list.
   --  @return the head of the world body list.


   function GetJointList (Self : in b2World) return access Joint.b2Joint'Class;
   --
   --  Get the world joint list. With the returned joint, use b2Joint::GetNext to get
   --  the next joint in the world list. A NULL joint indicates the end of the list.
   --  @return the head of the world joint list.


   function GetContactList (Self : in b2World) return access Contact.b2Contact'Class;
   --
   --  Get the world contact list. With the returned contact, use b2Contact::GetNext to get
   --  the next contact in the world list. A NULL contact indicates the end of the list.
   --  @return the head of the world contact list.
   --  @warning contacts are


   procedure SetAllowSleeping (Self : in out b2World;   flag : in Boolean);
   --
   --  Enable/disable sleep.


   procedure SetWarmStarting (Self : in out b2World;   flag : in Boolean);
   --
   --  Enable/disable warm starting. For testing.


   procedure SetContinuousPhysics (Self : in out b2World;   flag : in Boolean);
   --
   --  Enable/disable continuous physics. For testing.


   procedure SetSubStepping (Self : in out b2World;   flag : in Boolean);
   --
   --  Enable/disable single stepped continuous physics. For testing.




   function GetProxyCount (Self : in b2World) return int32;
   --
   --  Get the number of broad-phase proxies.


   function GetBodyCount (Self : in b2World) return int32;
   --
   --  Get the number of bodies.


   function GetJointCount (Self : in b2World) return int32;
   --
   --  Get the number of joints.

   function GetContactCount (Self : in b2World) return int32;
   --
   --  Get the number of contacts (each may have 0 or more contact points).


   function GetTreeHeight (Self : in b2World) return int32;
   --
   --  Get the height of the dynamic tree.


   function GetTreeBalance (Self : in b2World) return int32;
   --
   --  Get the balance of the dynamic tree.


   function GetTreeQuality (Self : in b2World) return float32;
   --
   -- Get the quality metric of the dynamic tree. The smaller the better.
   -- The minimum is 1.




   procedure SetGravity (Self : in out b2World;   gravity : in b2Vec2);
   --
   --  Change the global gravity vector.


   function GetGravity (Self : in b2World) return b2Vec2;
   --
   --  Get the global gravity vector.



   function IsLocked (Self : in    b2World) return Boolean;
   --
   --  Is the world locked (in the middle of a time step).




   procedure SetAutoClearForces (Self : in out b2World;   flag : in Boolean);
   --
   --  Set flag to control automatic clearing of forces after each time step.


   function GetAutoClearForces (Self : in b2World) return Boolean;
   --
   --  Get the flag that controls automatic clearing of forces after each time step.


   -- todo:
   --   /// Shift the world origin. Useful for large worlds.
   --   /// The body shift formula is: position -= newOrigin
   --   /// @param newOrigin the new origin with respect to the old origin
   --   void ShiftOrigin(const b2Vec2& newOrigin);





   ---------------------------------------------------
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

        m_bodyList  : access Solid.b2Body'Class;
        m_jointList : Joint.view;

        m_bodyCount : int32;
        m_jointCount : int32;

        m_gravity : b2Vec2;
        m_allowSleep : Boolean;

--          m_groundBody : access Solid.b2Body'Class;

        m_destructionListener : access world_callbacks.b2DestructionListener'Class;
--          b2DebugDraw* m_debugDraw;

        m_inv_dt0           : float32;    -- This is used to compute the time step ratio to support a variable time step.

        m_warmStarting      : Boolean;    -- This is for debugging the solver.
        m_continuousPhysics : Boolean;    -- This is for debugging the solver.
        m_subStepping       : Boolean;    -- This is for debugging the solver.

         m_stepComplete : Boolean;
      end record;



   procedure Solve    (Self : in out b2World;   step : in b2TimeStep);
   procedure SolveTOI (Self : in out b2World;   step : in b2TimeStep);
--     procedure SolveTOI (Self : in out b2World;   solid : access impact.d2.Solid.b2Body'Class);


end impact.d2.World;
