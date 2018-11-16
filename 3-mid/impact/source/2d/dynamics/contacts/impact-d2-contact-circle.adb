with impact.d2.Colliders,
     impact.d2.Shape.circle;


package body impact.d2.Contact.circle
is


   function to_b2CircleContact (fixtureA, fixtureB : access fixture.b2Fixture'Class) return b2CircleContact
   is
      use type shape.Kind;
      new_Contact : Contact.circle.b2CircleContact;
   begin
      define (new_Contact, fixtureA, fixtureB);
      pragma Assert (new_Contact.m_fixtureA.GetKind = Shape.e_circle);
      pragma Assert (new_Contact.m_fixtureB.GetKind = Shape.e_circle);

      return new_Contact;
   end to_b2CircleContact;




   overriding procedure Evaluate (Self : in out b2CircleContact;   manifold : access collision.b2Manifold;
                                                        xfA, xfB : in     b2Transform)
   is
   begin
      colliders.b2CollideCircles (manifold,
                                  Shape.circle.view (Self.m_fixtureA.GetShape),  xfA,
                                  Shape.circle.view (Self.m_fixtureB.GetShape),  xfB);
   end Evaluate;



   function  Create  (fixtureA, fixtureB : access Fixture.b2Fixture) return access b2Contact'Class
   is
      new_Contact : constant Contact.circle.view := new Contact.circle.b2CircleContact;
   begin
      define (new_Contact.all, fixtureA, fixtureB);
      return new_Contact.all'Access;
   end Create;




   procedure Destroy (contact            : in out impact.d2.Contact.view)
   is
   begin
      contact.destruct;
      free (contact);
   end Destroy;



end impact.d2.Contact.circle;
