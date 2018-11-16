with impact.d2.Contact,
     impact.d2.Solid,

     ada.Unchecked_Deallocation;



package body impact.d2.Fixture
is
   use type int32;



   function getKind (Self : in b2Fixture) return shape.Kind
   is
   begin
      return Self.m_shape.getKind;
   end getKind;



   function getShape (Self : in b2Fixture) return access Shape.b2Shape'Class
   is
   begin
      return Self.m_shape;
   end getShape;




   procedure setSensor (Self : in out b2Fixture;   sensor : Boolean)
   is
   begin
      Self.m_isSensor := sensor;
   end setSensor;




   function IsSensor (Self : in b2Fixture) return Boolean
   is
   begin
      return Self.m_isSensor;
   end IsSensor;



   function GetFilterData (Self : in b2Fixture) return b2Filter
   is
   begin
      return Self.m_filter;
   end GetFilterData;






   procedure SetFilterData (Self : in out b2Fixture;   filter : in b2Filter)
   is
      edge     : access impact.d2.contact.b2ContactEdge;
      contact  : access impact.d2.contact.b2Contact;

      fixtureA,
      fixtureB : access b2Fixture;
   begin
      Self.m_filter := filter;

      if Self.m_body = null then
         return;
      end if;

      --  Flag associated contacts for filtering.
      edge := Self.m_body.getContactList;

      while edge /= null loop
         contact  := edge.contact;
         fixtureA := contact.GetFixtureA;
         fixtureB := contact.GetFixtureB;

         if        fixtureA = Self'Access
           or else fixtureB = Self'Access
         then
            contact.FlagForFiltering;
         end if;

         edge := edge.next;
      end loop;
   end SetFilterData;





   function getBody (Self : in b2Fixture) return access Solid.b2Body'Class
   is
   begin
      return Self.m_body;
   end getBody;


   function GetNext (Self : in b2Fixture) return access b2Fixture
   is
   begin
      return Self.m_next;
   end GetNext;




   function GetUserData (Self : in b2Fixture) return access Any'Class
   is
   begin
      return Self.m_userData;
   end GetUserData;


   procedure SetUserData (Self : in out b2Fixture;   data : access Any'Class)
   is
   begin
      Self.m_userData := data;
   end SetUserData;






   function TestPoint (Self : in b2Fixture;   p : in b2Vec2) return Boolean
   is
   begin
      return Self.m_shape.TestPoint (Self.m_body.getTransform, p);
   end TestPoint;



   function RayCast (Self : in b2Fixture;   output     : access collision.b2RayCastOutput;
                                            input      : in     collision.b2RayCastInput;
                                            childIndex : in     int32) return Boolean
   is
   begin
      return Self.m_shape.RayCast (output, input, Self.m_body.GetTransform);
   end RayCast;





   procedure GetMassData (Self : in b2Fixture;   massData : access shape.b2MassData)
   is
   begin
      Self.m_shape.computeMass (massData, Self.m_density);
   end GetMassData;





   procedure SetDensity (Self : in out b2Fixture;   density : in float32)
   is
   begin
      pragma Assert (b2IsValid (density) and then  density >= 0.0);
      Self.m_density := density;
   end SetDensity;


   function GetDensity (Self : in b2Fixture) return float32
   is
   begin
      return Self.m_density;
   end GetDensity;



   function GetFriction (Self : in b2Fixture) return float32
   is
   begin
      return Self.m_friction;
   end GetFriction;


   procedure SetFriction (Self : in out b2Fixture;   friction : in float32)
   is
   begin
      Self.m_friction := friction;
   end SetFriction;




   function GetRestitution (Self : in b2Fixture) return float32
   is
   begin
      return Self.m_restitution;
   end GetRestitution;


   procedure SetRestitution (Self : in out b2Fixture;   restitution : in float32)
   is
   begin
      Self.m_restitution := restitution;
   end SetRestitution;





   function GetAABB (Self : in b2Fixture) return collision.b2AABB
   is
   begin
      return Self.m_aabb;
   end GetAABB;









   procedure destruct (Self : in out b2Fixture)
   is
      use type Shape.view;
   begin
      pragma Assert (Self.m_shape = null);
      pragma Assert (Self.m_proxyId = BroadPhase.e_nullProxy);
      null;
   end destruct;




   --  We need separation create/destroy functions from the constructor/destructor because
   --  the destructor cannot access the allocator (no destructor arguments allowed by C++).
   --
   procedure Create  (Self : in out b2Fixture;   Solid : access impact.d2.Solid.b2Body;
                                                 def   : in     b2FixtureDef)
   is
   begin
      Self.m_body        := Solid.all'Unchecked_Access;
      Self.m_userData    := def.userData;
      Self.m_friction    := def.friction;
      Self.m_restitution := def.restitution;
      Self.m_filter      := def.filter;
      Self.m_isSensor    := def.isSensor;
      Self.m_shape       := def.shape.Clone;
      Self.m_density     := def.density;
   end Create;



   procedure Destroy (Self : in out b2Fixture)
   is
      procedure free is new ada.Unchecked_Deallocation (Shape.b2Shape'Class, Shape.view);
   begin
      --  The proxy must be destroyed before calling this.
      pragma Assert (Self.m_proxyId = BroadPhase.e_nullProxy);

      --  Free the child shape.
      Self.m_shape.destruct;
      free (Self.m_shape);
   end Destroy;




   --  These support body activation/deactivation.
   --
   procedure CreateProxy  (Self : in out b2Fixture;   broadPhase : access impact.d2.broadphase.b2BroadPhase;
                                                      xf         : in     b2Transform)
   is
   begin
      pragma Assert (Self.m_proxyId = impact.d2.BroadPhase.e_nullProxy);

      --  Create proxy in the broad-phase.
      Self.m_shape.computeAABB (Self.m_aabb'Access, xf);
      Self.m_proxyId := broadPhase.createProxy (Self.m_aabb, Self'Access);
   end CreateProxy;




   procedure DestroyProxy (Self : in out b2Fixture;   broadPhase : access impact.d2.broadphase.b2BroadPhase)
   is
   begin
      if Self.m_proxyId = impact.d2.BroadPhase.e_nullProxy then
         return;
      end if;

      --  Destroy proxy in the broad-phase.
      broadPhase.destroyProxy (Self.m_proxyId);
      Self.m_proxyId := impact.d2.BroadPhase.e_nullProxy;
   end DestroyProxy;





   procedure Synchronize  (Self : in out b2Fixture;   broadPhase : access impact.d2.broadphase.b2BroadPhase;
                                                      xf1, xf2   : in     b2Transform)
   is
      aabb1, aabb2 : aliased collision.b2AABB;
      displacement : b2Vec2;
   begin
      if Self.m_proxyId = impact.d2.BroadPhase.e_nullProxy then
         return;
      end if;

      --  Compute an AABB that covers the swept shape (may miss some rotation effect).
      Self.m_shape.ComputeAABB (aabb1'Access, xf1);
      Self.m_shape.ComputeAABB (aabb2'Access, xf2);

      collision.Combine (Self.m_aabb,  aabb1, aabb2);

      displacement := xf2.position - xf1.position;

      broadPhase.MoveProxy (Self.m_proxyId, Self.m_aabb, displacement);
   end Synchronize;






   --- 'protected' subprograms for use by C 'friend's.
   --

   function m_proxyId (Self : in b2Fixture) return int32
   is
   begin
      return Self.m_proxyId;
   end m_proxyId;



   procedure m_next_is (Self : in out b2Fixture;   Now : in Fixture.view)
   is
   begin
      Self.m_next := Now;
   end m_next_is;



   procedure m_body_is (Self : in out b2Fixture;   Now : in Solid_view)
   is
   begin
      Self.m_body := Now;
   end m_body_is;





   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.Unchecked_Deallocation (b2Fixture'Class, View);
   begin
      deallocate (Self);
   end free;




end impact.d2.Fixture;
