with impact.d2.Fixture;


package impact.d2.Contact.circle
--
--
--
is

   type b2CircleContact is new b2Contact with null record;
   type View            is access all b2CircleContact'Class;


   function to_b2CircleContact (fixtureA, fixtureB : access fixture.b2Fixture'Class) return b2CircleContact;



   overriding procedure Evaluate (Self : in out b2CircleContact;   manifold : access collision.b2Manifold;
                                                        xfA, xfB : in     b2Transform);



   function  Create  (fixtureA, fixtureB : access Fixture.b2Fixture) return access b2Contact'Class;
   procedure Destroy (contact            : in out impact.d2.Contact.view);


end impact.d2.Contact.circle;
