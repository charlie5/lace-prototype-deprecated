with impact.d2.Fixture,
     impact.d2.Collision,
     impact.d2.Shape,
     impact.d2.Math;

limited
with impact.d2.Solid,
     impact.d2.world_Callbacks;




package impact.d2.Contact
--
--
--
is
   use impact.d2.Math;


   --  The class manages contact between two shapes. A contact exists for each overlapping
   --  AABB in the broad-phase (except if filtered). Therefore a contact object may exist
   --  that has no contact points.
   --
   type b2Contact  is abstract tagged private;

   type View      is access all b2Contact'Class;
   type Views     is array (int32 range <>) of View;

   procedure free (Self : in out View);




   type b2ContactCreateFcn  is access function  (fixtureA, fixtureB : access Fixture.b2Fixture) return access b2Contact'Class;
   type b2ContactDestroyFcn is access procedure (contact            : in out impact.d2.Contact.view);

   type b2ContactRegister is
      record
         createFcn  : b2ContactCreateFcn;
         destroyFcn : b2ContactDestroyFcn;
         primary    : Boolean;
      end record;


   --  A contact edge is used to connect bodies and contacts together
   --  in a contact graph where each body is a node and each contact
   --  is an edge. A contact edge belongs to a doubly linked list
   --  maintained in each attached body. Each contact has two contact
   --  nodes, one for each attached body.
   --
   type b2ContactEdge is
      record
        other   : access Solid.b2Body'Class;   -- provides quick access to the other body attached.
        contact :        impact.d2.Contact.view;   -- the contact
        prev    : access b2ContactEdge;               -- the previous contact edge in the body's contact list
        next    : access b2ContactEdge;        -- the next contact edge in the body's contact list
      end record;




   --  Get the contact manifold. Do not modify the manifold unless you understand the internals of Box2D.
   --
   function GetManifold (Self : access b2Contact) return access collision.b2Manifold;


   --  Get the world manifold.
   --
   procedure GetWorldManifold (Self : in b2Contact;   worldManifold : access collision.b2WorldManifold);



   --  Is this contact touching?
   --
   function IsTouching (Self : in b2Contact) return Boolean;


   --  Enable/disable this contact. This can be used inside the pre-solve
   --  contact listener. The contact is only disabled for the current
   --  time step (or sub-step in continuous collisions).
   --
   procedure SetEnabled (Self : in out b2Contact;   flag : in Boolean);


   --  Has this contact been disabled?
   --
   function IsEnabled (Self : in b2Contact) return Boolean;



   --  Get the next contact in the world's contact list.
   --
   function GetNext (Self : in b2Contact) return View;


   --  Get the first fixture in this contact.
   --
   function GetFixtureA (Self : in b2Contact) return access Fixture.b2Fixture'Class;


   --  Get the second fixture in this contact.
   --
   function GetFixtureB (Self : in b2Contact) return access Fixture.b2Fixture'Class;


   function GetChildIndexA (Self : in b2Contact) return int32;
   --
   -- Get the child primitive index for fixture A.

   function GetChildIndexB (Self : in b2Contact) return int32;
   --
   -- Get the child primitive index for fixture B.



   --  Evaluate this contact with your own manifold and transforms.
   --
   procedure Evaluate (Self : in out b2Contact;   manifold : access collision.b2Manifold;
                                                  xfA, xfB : in     b2Transform)
   is abstract;





   --- 'protected' subprograms for use by C 'friend's.
   --

   --  Flags stored in m_flags
   --
   subtype Flag is uint32;

   e_islandFlag    : constant Flag := 16#0001#;   -- Used when crawling contact graph when forming islands.
   e_touchingFlag  : constant Flag := 16#0002#;   -- Set when the shapes are touching.
   e_enabledFlag   : constant Flag := 16#0004#;   -- This contact can be disabled (by user)
   e_filterFlag    : constant Flag := 16#0008#;   -- This contact needs filtering because a fixture filter was changed.
   e_bulletHitFlag : constant Flag := 16#0010#;   -- This bullet contact had a TOI event
   e_toiFlag       : constant Flag := 16#0020#;   -- This contact has a valid TOI in m_toi.



   --  Flag this contact for filtering. Filtering will occur the next time step.
   --
   procedure FlagForFiltering (Self : in out b2Contact);




   function m_prev (Self : access b2Contact) return View;
   function m_next (Self : access b2Contact) return  View;

--     function m_nodeA (Self : access b2Contact) return access b2ContactEdge;
--     function m_nodeB (Self : access b2Contact) return access b2ContactEdge;


   procedure m_prev_is (Self : in out b2Contact;   Now : in View);
   procedure m_next_is (Self : in out b2Contact;   Now : in View);

   function  m_flags     (Self : in     b2Contact'Class) return  uint32;
   procedure m_flags_are (Self : in out b2Contact'Class;   Now : in uint32);

   function  m_toiCount    (Self : in     b2Contact'Class) return  int32;
   procedure m_toiCount_is (Self : in out b2Contact'Class;   Now : in int32);

   function  m_toi    (Self : in     b2Contact'Class) return  float32;
   procedure m_toi_is (Self : in out b2Contact'Class;   Now : in float32);


   procedure Destroy (contact      : in out impact.d2.Contact.view);

   procedure Update (Self : in out b2Contact'Class;   listener : access world_callbacks.b2ContactListener'Class);



private




   type b2Contact is abstract tagged
      record
         m_flags        : uint32;

         -- World pool and list pointers.
         m_prev         : Contact.view;
         m_next         : Contact.view;

         -- Nodes for connecting bodies.
         m_nodeA        : aliased b2ContactEdge;
         m_nodeB        : aliased b2ContactEdge;

         m_fixtureA     : access Fixture.b2Fixture;
         m_fixtureB     : access Fixture.b2Fixture;

         m_indexA,
         m_indexB       : int32;

         m_manifold     : aliased collision.b2Manifold;

         m_toiCount     : int32;
         m_toi          : float32;

	 m_friction,
	 m_restitution  : float32;

	 m_tangentSpeed : float32;
      end record;



   procedure define   (Self : in out b2Contact;   fixtureA, fixtureB : access Fixture.b2Fixture'Class);

   procedure destruct (Self : in out b2Contact) is null;










   procedure AddType (createFcn    : in     b2ContactCreateFcn;
                      destroyFcn   : in     b2ContactDestroyFcn;
                      kindA, kindB : in     shape.Kind);

   procedure InitializeRegisters;

   function  Create  (fixtureA,
                      fixtureB     : access Fixture.b2Fixture) return access b2Contact'Class;

--     procedure Destroy (contact      : access b2Contact;
--                        kindA, kindB : in     shape.Kind);   -- tbd: There appears to be no 'body' for this in the original C code






   s_registers   : array (Shape.Kind'Range, Shape.Kind'Range) of b2ContactRegister;
   s_initialized : Boolean := False;

end impact.d2.Contact;

