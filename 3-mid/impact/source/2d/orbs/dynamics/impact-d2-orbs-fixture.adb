with impact.d2.orbs.Contact,
     impact.d2.orbs.Solid,

     ada.Unchecked_Deallocation;



package body impact.d2.orbs.Fixture
is
   use type int32;





   function getShape (Self : access item) return Shape.view
   is
   begin
      return Self.m_shape'Access;
   end getShape;




   procedure setSensor (Self : in out item;   sensor : Boolean)
   is
   begin
      Self.m_isSensor := sensor;
   end setSensor;




   function IsSensor (Self : in item) return Boolean
   is
   begin
      return Self.m_isSensor;
   end IsSensor;



   function GetFilterData (Self : in item) return Filter
   is
   begin
      return Self.m_filter;
   end GetFilterData;






   procedure SetFilterData (Self : in out item;   filter : in fixture.Filter)
   is
      edge     : access impact.d2.orbs.contact.b2ContactEdge;
      contact  : access impact.d2.orbs.contact.b2Contact;

      fixtureA,
      fixtureB : access item;
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





   function getBody (Self : in item) return access Solid.item'Class
   is
   begin
      return Self.m_body;
   end getBody;


--     function GetNext (Self : in item) return access item
--     is
--     begin
--        return Self.m_next;
--     end;




   function GetUserData (Self : in item) return access Any'Class
   is
   begin
      return Self.m_userData;
   end GetUserData;


   procedure SetUserData (Self : in out item;   data : access Any'Class)
   is
   begin
      Self.m_userData := data;
   end SetUserData;






   function TestPoint (Self : in item;   p : in b2Vec2) return Boolean
   is
   begin
      return Self.m_shape.test_Point (Self.m_body.getTransform, p);
   end TestPoint;



   function RayCast (Self : in item;   output : access collision.b2RayCastOutput;
                                            input  : in     collision.b2RayCastInput) return Boolean
   is
   begin
      return Self.m_shape.ray_Cast (output, input, Self.m_body.GetTransform);
   end RayCast;





   procedure GetMassData (Self : in item;   massData : access shape.mass_Data)
   is
   begin
      Self.m_shape.compute_Mass (massData, Self.m_density);
   end GetMassData;





   procedure SetDensity (Self : in out item;   density : in float32)
   is
   begin
      pragma Assert (b2IsValid (density) and then  density >= 0.0);
      Self.m_density := density;
   end SetDensity;


   function GetDensity (Self : in item) return float32
   is
   begin
      return Self.m_density;
   end GetDensity;



   function GetFriction (Self : in item) return float32
   is
   begin
      return Self.m_friction;
   end GetFriction;


   procedure SetFriction (Self : in out item;   friction : in float32)
   is
   begin
      Self.m_friction := friction;
   end SetFriction;




   function GetRestitution (Self : in item) return float32
   is
   begin
      return Self.m_restitution;
   end GetRestitution;


   procedure SetRestitution (Self : in out item;   restitution : in float32)
   is
   begin
      Self.m_restitution := restitution;
   end SetRestitution;





   function GetAABB (Self : in item) return collision.b2AABB
   is
   begin
      return Self.m_aabb;
   end GetAABB;









   procedure destruct (Self : in out item)
   is
      use type Shape.view;
   begin
      pragma Assert (Self.m_proxyId = BroadPhase.e_nullProxy);
      null;
   end destruct;




   --  We need separation create/destroy functions from the constructor/destructor because
   --  the destructor cannot access the allocator (no destructor arguments allowed by C++).
   --
   procedure Create (Self : in out item;   Solid : access impact.d2.orbs.Solid.item;
                                                def   : in     Definition)
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



   procedure Destroy (Self : in out item)
   is
      procedure free is new ada.Unchecked_Deallocation (Shape.Item'Class, Shape.view);
   begin
      --  The proxy must be destroyed before calling this.
      pragma Assert (Self.m_proxyId = BroadPhase.e_nullProxy);

      --  Free the child shape.
      Self.m_shape.destruct;
   end Destroy;




   --  These support body activation/deactivation.
   --
   procedure CreateProxy (Self : in out item;   broadPhase : access impact.d2.orbs.broadphase.b2BroadPhase;
                                                     xf         : in     b2Transform)
   is
   begin
      pragma Assert (Self.m_proxyId = impact.d2.orbs.BroadPhase.e_nullProxy);

      --  Create proxy in the broad-phase.
      Self.m_shape.compute_AABB (Self.m_aabb'Access, xf);
      Self.m_proxyId := broadPhase.createProxy (Self.m_aabb, Self'Access);
   end CreateProxy;




   procedure DestroyProxy (Self : in out item;   broadPhase : access impact.d2.orbs.broadphase.b2BroadPhase)
   is
   begin
      if Self.m_proxyId = impact.d2.orbs.BroadPhase.e_nullProxy then
         return;
      end if;

      --  Destroy proxy in the broad-phase.
      broadPhase.destroyProxy (Self.m_proxyId);
      Self.m_proxyId := impact.d2.orbs.BroadPhase.e_nullProxy;
   end DestroyProxy;





   procedure Synchronize (Self : in out item;   broadPhase : access impact.d2.orbs.broadphase.b2BroadPhase;
                                                     xf1, xf2   : in     b2Transform)
   is
      aabb1, aabb2 : aliased collision.b2AABB;
      displacement : b2Vec2;
   begin
      if Self.m_proxyId = impact.d2.orbs.BroadPhase.e_nullProxy then
         return;
      end if;

      --  Compute an AABB that covers the swept shape (may miss some rotation effect).
      Self.m_shape.compute_AABB (aabb1'Access, xf1);
      Self.m_shape.compute_AABB (aabb2'Access, xf2);

      collision.Combine (Self.m_aabb,  aabb1, aabb2);

      displacement := xf2.position - xf1.position;

      broadPhase.MoveProxy (Self.m_proxyId, Self.m_aabb, displacement);
   end Synchronize;






   --- 'protected' subprograms for use by C 'friend's.
   --

   function m_proxyId (Self : in item) return int32
   is
   begin
      return Self.m_proxyId;
   end m_proxyId;



--     procedure m_next_is (Self : in out item;   Now : in Fixture.view)
--     is
--     begin
--        Self.m_next := Now;
--     end;



   procedure m_body_is (Self : in out item;   Now : in Solid_view)
   is
   begin
      Self.m_body := Now;
   end m_body_is;





   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.Unchecked_Deallocation (item'Class, View);
   begin
      deallocate (Self);
   end free;




end impact.d2.orbs.Fixture;
