with impact.d2.Contact,
     impact.d2.Fixture,
     impact.d2.Collision,
     impact.d2.Joint,
     impact.d2.Math;





package impact.d2.world_Callbacks
--
--
--
is
   use impact.d2.Math;

   procedure dummy;


--
--  struct b2Vec2;
--  struct b2Transform;
--  class b2Fixture;
--  class b2Body;
--  class b2Joint;
--  class b2Contact;
--  struct b2ContactPoint;
--  struct b2ContactResult;
--  struct b2Manifold;




   --  Joints and fixtures are destroyed when their associated
   --  body is destroyed. Implement this listener so that you
   --  may nullify references to these joints and shapes.
   --
   type b2DestructionListener is interface;

   procedure destruct (Self : in out b2DestructionListener) is null;

   --  Called when any joint is about to be destroyed due
   --  to the destruction of one of its attached bodies.
   --
   procedure SayGoodbye (Self : in out b2DestructionListener;   joint : access impact.d2.Joint.b2Joint'Class)
   is abstract;



   --  Called when any fixture is about to be destroyed due
   --  to the destruction of its parent body.
   procedure SayGoodbye (Self : in out b2DestructionListener;   fixture : in impact.d2.Fixture.view)
   is abstract;


--  {
--  public:
--          virtual ~b2DestructionListener() {}
--
--          /// Called when any joint is about to be destroyed due
--          /// to the destruction of one of its attached bodies.
--          virtual void SayGoodbye(b2Joint* joint) = 0;
--
--          /// Called when any fixture is about to be destroyed due
--          /// to the destruction of its parent body.
--          virtual void SayGoodbye(b2Fixture* fixture) = 0;
--  };
--

   --  Implement this class to provide collision filtering. In other words, you can implement
   --  this class if you want finer control over contact creation.
   --
   type b2ContactFilter is tagged null record;

   procedure destruct (Self : in out b2ContactFilter) is null;


   --  Return true if contact calculations should be performed between these two shapes.
   --  Warning: for performance reasons this is only called when the AABBs begin to overlap.
   --
   function  ShouldCollide (Self : access b2ContactFilter;  fixtureA, fixtureB : access Fixture.b2Fixture'Class) return Boolean;






   --  Contact impulses for reporting. Impulses are used instead of forces because
   --  sub-step forces may approach infinity for rigid body collisions. These
   --  match up one-to-one with the contact points in b2Manifold.
   --
   type b2ContactImpulse is
      record
         normalImpulses  : float32_array (1 .. b2_maxManifoldPoints);
         tangentImpulses : float32_array (1 .. b2_maxManifoldPoints);
      end record;






   --  Implement this class to get contact information. You can use these results for
   --  things like sounds and game logic. You can also get contact results by
   --  traversing the contact lists after the time step. However, you might miss
   --  some contacts because continuous physics leads to sub-stepping.
   --  Additionally you may receive multiple callbacks for the same contact in a
   --  single time step.
   --  You should strive to make your callbacks efficient because there may be
   --  many callbacks per time step.
   --  Warning: You cannot create/destroy Box2D entities inside these callbacks.
   --
   type b2ContactListener is interface;

   --  Called when two fixtures begin to touch.
   --
   procedure BeginContact (Self : in out b2ContactListener;   contact : access impact.d2.Contact.b2Contact'Class) is null;


   --  Called when two fixtures cease to touch.
   --
   procedure EndContact   (Self : in out b2ContactListener;   contact : access impact.d2.Contact.b2Contact'Class) is null;


   --  This is called after a contact is updated. This allows you to inspect a
   --  contact before it goes to the solver. If you are careful, you can modify the
   --  contact manifold (e.g. disable contact).
   --  A copy of the old manifold is provided so that you can detect changes.
   --  Note: this is called only for awake bodies.
   --  Note: this is called even when the number of contact points is zero.
   --  Note: this is not called for sensors.
   --  Note: if you set the number of contact points to zero, you will not
   --       get an EndContact callback. However, you may get a BeginContact callback
   --       the next step.
   --
   procedure PreSolve (Self : in out b2ContactListener;   contact     : access impact.d2.Contact.b2Contact'Class;
                                                          oldManifold : in     collision.b2Manifold                 ) is null;



   --  This lets you inspect a contact after the solver is finished. This is useful
   --  for inspecting impulses.
   --  Note: the contact manifold does not include time of impact impulses, which can be
   --  arbitrarily large if the sub-step is small. Hence the impulse is provided explicitly
   --  in a separate data structure.
   --  Note: this is only called for contacts that are touching, solid, and awake.
   --
   procedure PostSolve (Self : in out b2ContactListener;   contact : access impact.d2.Contact.b2Contact'Class;
                                                           impulse : in     b2ContactImpulse     ) is null;


--  class b2ContactListener
--  {
--  public:
--          virtual ~b2ContactListener() {}
--  };










   --  Callback class for AABB queries.
   --  See b2World::Query
   --
   type b2QueryCallback is interface;

   procedure destruct (Self : in out b2QueryCallback) is null;


   --  Called for each fixture found in the query AABB.
   --  @return false to terminate the query.
   --
   function ReportFixture (Self : access b2QueryCallback;   fixture : in impact.d2.Fixture.view) return Boolean
   is abstract;






   --  Callback class for ray casts.
   --  See b2World::RayCast
   --
   type b2RayCastCallback is interface;

   procedure destruct (Self : in out b2RayCastCallback) is null;

   --  Called for each fixture found in the query. You control how the ray cast
   --  proceeds by returning a float:
   --  return -1: ignore this fixture and continue
   --  return 0: terminate the ray cast
   --  return fraction: clip the ray to this point
   --  return 1: don't clip the ray and continue
   --  @param fixture the fixture hit by the ray
   --  @param point the point of initial intersection
   --  @param normal the normal vector at the point of intersection
   --  @return -1 to filter, 0 to terminate, fraction to clip the ray for
   --  closest hit, 1 to continue
   --
   function ReportFixture (Self : in b2RayCastCallback;   fixture  : in impact.d2.Fixture.View;
                                                          point    : in b2Vec2;
                                                          normal   : in b2Vec2;
                                                          fraction : in float32) return float32
   is abstract;

--          virtual float32 ReportFixture(        b2Fixture* fixture, const b2Vec2& point,
--                                                                          const b2Vec2& normal, float32 fraction) = 0;



--  {
--  public:
--          virtual ~b2RayCastCallback() {}
--
--          /// Called for each fixture found in the query. You control how the ray cast
--          /// proceeds by returning a float:
--          /// return -1: ignore this fixture and continue
--          /// return 0: terminate the ray cast
--          /// return fraction: clip the ray to this point
--          /// return 1: don't clip the ray and continue
--          /// @param fixture the fixture hit by the ray
--          /// @param point the point of initial intersection
--          /// @param normal the normal vector at the point of intersection
--          /// @return -1 to filter, 0 to terminate, fraction to clip the ray for
--          /// closest hit, 1 to continue
--          virtual float32 ReportFixture(        b2Fixture* fixture, const b2Vec2& point,
--                                                                          const b2Vec2& normal, float32 fraction) = 0;
--  };






--  /// Color for debug drawing. Each value has the range [0,1].
--  struct b2Color
--  {
--          b2Color() {}
--          b2Color(float32 r, float32 g, float32 b) : r(r), g(g), b(b) {}
--          void Set(float32 ri, float32 gi, float32 bi) { r = ri; g = gi; b = bi; }
--          float32 r, g, b;
--  };
--
--  /// Implement and register this class with a b2World to provide debug drawing of physics
--  /// entities in your game.
--  class b2DebugDraw
--  {
--  public:
--          b2DebugDraw();
--
--          virtual ~b2DebugDraw() {}
--
--          enum
--          {
--                  e_shapeBit                                = 0x0001, ///< draw shapes
--                  e_jointBit                                = 0x0002, ///< draw joint connections
--                  e_aabbBit                                = 0x0004, ///< draw axis aligned bounding boxes
--                  e_pairBit                                = 0x0008, ///< draw broad-phase pairs
--                  e_centerOfMassBit                = 0x0010, ///< draw center of mass frame
--          };
--
--          /// Set the drawing flags.
--          void SetFlags(uint32 flags);
--
--          /// Get the drawing flags.
--          uint32 GetFlags() const;
--
--          /// Append flags to the current flags.
--          void AppendFlags(uint32 flags);
--
--          /// Clear flags from the current flags.
--          void ClearFlags(uint32 flags);
--
--          /// Draw a closed polygon provided in CCW order.
--          virtual void DrawPolygon(const b2Vec2* vertices, int32 vertexCount, const b2Color& color) = 0;
--
--          /// Draw a solid closed polygon provided in CCW order.
--          virtual void DrawSolidPolygon(const b2Vec2* vertices, int32 vertexCount, const b2Color& color) = 0;
--
--          /// Draw a circle.
--          virtual void DrawCircle(const b2Vec2& center, float32 radius, const b2Color& color) = 0;
--
--          /// Draw a solid circle.
--          virtual void DrawSolidCircle(const b2Vec2& center, float32 radius, const b2Vec2& axis, const b2Color& color) = 0;
--
--          /// Draw a line segment.
--          virtual void DrawSegment(const b2Vec2& p1, const b2Vec2& p2, const b2Color& color) = 0;
--
--          /// Draw a transform. Choose your own length scale.
--          /// @param xf a transform.
--          virtual void DrawTransform(const b2Transform& xf) = 0;
--
--  protected:
--          uint32 m_drawFlags;
--  };
--
--  #endif

end impact.d2.world_Callbacks;
