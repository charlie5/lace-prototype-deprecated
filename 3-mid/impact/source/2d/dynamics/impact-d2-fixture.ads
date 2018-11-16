with impact.d2.Shape,
     impact.d2.Collision,
     impact.d2.Broadphase,
     impact.d2.Math;

limited
with impact.d2.Solid;


package impact.d2.Fixture
--
--
--
is
   use impact.d2.Math;


   --  This holds contact filtering data.
   --
   type b2Filter is
      record
         --  The collision category bits. Normally you would just set one bit.
         categoryBits : uint16;

         --  The collision mask bits. This states the categories that this
         --  shape would accept for collision.
         maskBits : uint16;

         --  Collision groups allow a certain group of objects to never collide (negative)
         --  or always collide (positive). Zero means no collision group. Non-zero group
         --  filtering always wins against the mask bits.
         groupIndex : int16;
      end record;






   --  A fixture definition is used to create a fixture. This class defines an
   --  abstract fixture definition. You can reuse fixture definitions safely.
   --
   type b2FixtureDef is
      record
         shape       : access constant impact.d2.Shape.b2Shape'Class;   -- The shape, this must be set. The shape will be cloned, so you can create the shape on the stack.
         userData    : access Any'Class;                            -- Use this to store application specific fixture data.

         friction    : float32  := 0.2;                             -- The friction coefficient, usually in the range [0,1].
         restitution : float32  := 0.0;                             -- The restitution (elasticity) usually in the range [0,1].
         density     : float32  := 0.0;                             -- The density, usually in kg/m^2.

         isSensor    : Boolean  := False;                           -- A sensor shape collects contact information but never generates a collision response.

         filter      : b2Filter := (categoryBits => 16#0001#,       -- Contact filtering data.
                                    maskBits     => 16#ffff#,
                                    groupIndex   => 0);             -- tbd: maybe should be '1' ?
      end record;




   --  A fixture is used to attach a shape to a body for collision detection. A fixture
   --  inherits its transform from its parent. Fixtures hold additional non-geometric data
   --  such as friction, collision filters, etc.
   --  Fixtures are created via 'Solid.CreateFixture'.
   --  Warning: you cannot reuse fixtures.
   --
   type b2Fixture is new Any with private;
   type View      is access all b2Fixture'Class;

   procedure free (Self : in out View);



   --  Get the type of the child shape. You can use this to down cast to the concrete shape.
   --  Returns the shape kind.
   --
   function getKind (Self : in b2Fixture) return shape.Kind;


   --  Get the child shape. You can modify the child shape, however you should not change the
   --  number of vertices because this will crash some collision caching mechanisms.
   --  Manipulating the shape may lead to non-physical behavior.
   --
   function getShape (Self : in b2Fixture) return access Shape.b2Shape'Class;


   --  Set if this fixture is a sensor.
   --
   procedure setSensor (Self : in out b2Fixture;   sensor : Boolean);


   --  Is this fixture a sensor (non-solid)?
   --  Returns True if the shape is a sensor.
   --
   function IsSensor (Self : in b2Fixture) return Boolean;


   --  Get the contact filtering data.
   --
   function GetFilterData (Self : in b2Fixture) return b2Filter;


   --  Set the contact filtering data. This will not update contacts until the next time
   --  step when either parent body is active and awake.
   --
   procedure SetFilterData (Self : in out b2Fixture;   filter : in b2Filter);



   --  Get the parent body of this fixture. This is NULL if the fixture is not attached.
   --  Returns the parent body.
   --
   function getBody (Self : in b2Fixture) return access Solid.b2Body'Class;


   --  Get the next fixture in the parent body's fixture list.
   --  Returns the next shape.
   --
   function GetNext (Self : in b2Fixture) return access b2Fixture;



   --  Get the user data that was assigned in the fixture definition. Use this to
   --  store your application specific data.
   --
   function GetUserData (Self : in b2Fixture) return access Any'Class;


   --  Set the user data. Use this to store your application specific data.
   --
   procedure SetUserData (Self : in out b2Fixture;   data : access Any'Class);



   --  Test a point for containment in this fixture.
   --  'xf' the shape world transform.
   --  'p'  a point in world coordinates.
   --
   function TestPoint (Self : in b2Fixture;   p : in b2Vec2) return Boolean;


   --  Cast a ray against this shape.
   --  'output' the ray-cast results.
   --  'input' the ray-cast input parameters.
   --
   function RayCast (Self : in b2Fixture;   output     : access collision.b2RayCastOutput;
                                            input      : in     collision.b2RayCastInput;
                                            childIndex : in     int32) return Boolean;



   --  Get the mass data for this fixture. The mass data is based on the density and
   --  the shape. The rotational inertia is about the shape's origin. This operation may be expensive.
   --
   procedure GetMassData (Self : in b2Fixture;   massData : access shape.b2MassData);



   --  Set the density of this fixture. This will _not_ automatically adjust the mass
   --  of the body. You must call b2Body::ResetMassData to update the body's mass.
   --
   procedure SetDensity (Self : in out b2Fixture;   density : in float32);


   --  Get the density of this fixture.
   --
   function GetDensity (Self : in b2Fixture) return float32;



   --  Get the coefficient of friction.
   --
   function GetFriction (Self : in b2Fixture) return float32;

   --  Set the coefficient of friction.
   --
   procedure SetFriction (Self : in out b2Fixture;   friction : in float32);



   --  Get the coefficient of restitution.
   --
   function GetRestitution (Self : in b2Fixture) return float32;


   --  Set the coefficient of restitution.
   --
   procedure SetRestitution (Self : in out b2Fixture;   restitution : in float32);



   --  Get the fixture's AABB. This AABB may be enlarge and/or stale.
   --  If you need a more accurate AABB, compute it using the shape and the body transform.
   --
   function GetAABB (Self : in b2Fixture) return collision.b2AABB;



   -- This proxy is used internally to connect fixtures to the broad-phase.
   --
   type b2FixtureProxy is new Any with
      record
         aabb       : collision.b2AABB;
         fixture    : d2.Fixture.view;
         childIndex,
         proxyId    : int32;
      end record;

   type b2FixtureProxy_view is access all b2FixtureProxy'Class;





   --- 'protected' subprograms for use by C 'friend's.
   --
   type Solid_view is access all Solid.b2Body'Class;


   function  m_proxyId (Self : in b2Fixture) return int32;

   procedure m_next_is (Self : in out b2Fixture;   Now : in Fixture.view);
   procedure m_body_is (Self : in out b2Fixture;   Now : in Solid_view);


   --  We need separation create/destroy functions from the constructor/destructor because
   --  the destructor cannot access the allocator (no destructor arguments allowed by C++).
   --
   procedure Create  (Self : in out b2Fixture;   Solid : access impact.d2.Solid.b2Body;
                                                 def   : in     b2FixtureDef);


   --  These support body activation/deactivation.
   --
   procedure CreateProxy  (Self : in out b2Fixture;   broadPhase : access impact.d2.broadphase.b2BroadPhase;
                                                      xf         : in     b2Transform);

   procedure DestroyProxy (Self : in out b2Fixture;   broadPhase : access impact.d2.broadphase.b2BroadPhase);

   procedure Destroy (Self : in out b2Fixture);

   procedure destruct (Self : in out b2Fixture);


   procedure Synchronize  (Self : in out b2Fixture;   broadPhase : access impact.d2.broadphase.b2BroadPhase;
                                                      xf1, xf2   : in     b2Transform);



private

   type b2Fixture is new Any with
      record
         m_aabb        : aliased collision.b2AABB;

         m_density     : float32 := 0.0;

         m_next        : access b2Fixture;
         m_body        : access Solid.b2Body;

         m_shape       : Shape.view;

         m_friction    : float32;
         m_restitution : float32;

         m_proxyId     : int32   := broadphase.e_nullProxy;
         m_filter      : b2Filter;

         m_isSensor    : Boolean;

         m_userData    : access Any'Class;
      end record;


end impact.d2.Fixture;
