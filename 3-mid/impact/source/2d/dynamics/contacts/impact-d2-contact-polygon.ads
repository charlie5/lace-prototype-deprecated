package impact.d2.Contact.polygon
--
--
--
is


   type b2PolygonContact is new b2Contact with null record;
   type View             is access all b2PolygonContact'Class;


   overriding procedure Evaluate (Self : in out b2PolygonContact;   manifold : access collision.b2Manifold;
                                                         xfA, xfB : in     b2Transform);


   function  Create  (fixtureA, fixtureB : access Fixture.b2Fixture) return access b2Contact'Class;
   procedure Destroy (contact            : in out impact.d2.Contact.view);


--  class b2PolygonContact : public b2Contact
--  {
--  public:
--
--          b2PolygonContact(b2Fixture* fixtureA, b2Fixture* fixtureB);
--          ~b2PolygonContact() {}
--
--          void Evaluate(b2Manifold* manifold, const b2Transform& xfA, const b2Transform& xfB);
--  };
--
--  #endif


end impact.d2.Contact.polygon;
