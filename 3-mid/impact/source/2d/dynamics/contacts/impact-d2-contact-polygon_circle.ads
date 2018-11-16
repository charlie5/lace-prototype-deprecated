package impact.d2.Contact.polygon_circle
--
--
--
is

   type b2PolygonAndCircleContact is new b2Contact with null record;
   type View                      is access all b2PolygonAndCircleContact'Class;



   overriding procedure Evaluate (Self : in out b2PolygonAndCircleContact;   manifold : access collision.b2Manifold;
                                                                  xfA, xfB : in     b2Transform);

   function  Create  (fixtureA, fixtureB : access Fixture.b2Fixture) return access b2Contact'Class;
   procedure Destroy (contact            : in out impact.d2.Contact.view);

--
--  #include <Box2D/Dynamics/Contacts/b2Contact.h>
--
--  class b2BlockAllocator;
--
--  class b2PolygonAndCircleContact : public b2Contact
--  {
--  public:
--          static b2Contact* Create(b2Fixture* fixtureA, b2Fixture* fixtureB, b2BlockAllocator* allocator);
--          static void Destroy(b2Contact* contact, b2BlockAllocator* allocator);
--
--          b2PolygonAndCircleContact(b2Fixture* fixtureA, b2Fixture* fixtureB);
--          ~b2PolygonAndCircleContact() {}
--
--          void Evaluate(b2Manifold* manifold, const b2Transform& xfA, const b2Transform& xfB);
--  };
--
--  #endif


end impact.d2.Contact.polygon_circle;

