with impact.d2.orbs.Fixture;


package impact.d2.orbs.Contact.circle
--
--
--
is

   type b2CircleContact is new b2Contact with null record;
   type View            is access all b2CircleContact'Class;


   function to_b2CircleContact (fixtureA, fixtureB : access fixture.item'Class) return b2CircleContact;



   overriding procedure Evaluate (Self : in out b2CircleContact;   manifold : access collision.b2Manifold;
                                                        xfA, xfB : in     b2Transform);



   function  Create  (fixtureA, fixtureB : access Fixture.item) return access b2Contact'Class;
   procedure Destroy (contact            : in out impact.d2.orbs.Contact.view);


end impact.d2.orbs.Contact.circle;
