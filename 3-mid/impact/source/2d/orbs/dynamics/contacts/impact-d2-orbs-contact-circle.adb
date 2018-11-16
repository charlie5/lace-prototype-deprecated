with impact.d2.orbs.Colliders,
     impact.d2.orbs.Shape;


package body impact.d2.orbs.Contact.circle
is


   function to_b2CircleContact (fixtureA, fixtureB : access fixture.item'Class) return b2CircleContact
   is
      new_Contact : Contact.circle.b2CircleContact;
   begin
      define (new_Contact, fixtureA, fixtureB);

      return new_Contact;
   end to_b2CircleContact;




   overriding procedure Evaluate (Self : in out b2CircleContact;   manifold : access collision.b2Manifold;
                                                        xfA, xfB : in     b2Transform)
   is
   begin
      colliders.b2CollideCircles (manifold,
                                  Shape.view (Self.m_fixtureA.GetShape),  xfA,
                                  Shape.view (Self.m_fixtureB.GetShape),  xfB);
   end Evaluate;



   function  Create  (fixtureA, fixtureB : access Fixture.item) return access b2Contact'Class
   is
      new_Contact : constant Contact.circle.view := new Contact.circle.b2CircleContact;
   begin
      define (new_Contact.all, fixtureA, fixtureB);
      return new_Contact.all'Access;
   end Create;




   procedure Destroy (contact            : in out impact.d2.orbs.Contact.view)
   is
   begin
      contact.destruct;
      free (contact);
   end Destroy;



end impact.d2.orbs.Contact.circle;
