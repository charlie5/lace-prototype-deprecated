with impact.d2.Solid,
     impact.d2.Contact.circle,
     impact.d2.Contact.polygon,
     impact.d2.Contact.polygon_circle,
     impact.d2.world_Callbacks,

     ada.unchecked_Deallocation;



package body impact.d2.Contact
is
   use type int32;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (b2Contact'Class, View);
   begin
      deallocate (Self);
   end free;






   function GetManifold (Self : access b2Contact) return access collision.b2Manifold
   is
   begin
      return Self.m_manifold'Unchecked_Access;
   end GetManifold;



   procedure GetWorldManifold (Self : in b2Contact;   worldManifold : access collision.b2WorldManifold)
   is
      bodyA  : constant access constant Solid.b2Body  := Self.m_fixtureA.GetBody;
      bodyB  : constant access constant Solid.b2Body  := Self.m_fixtureB.GetBody;

      shapeA : constant access constant Shape.b2Shape := Self.m_fixtureA.GetShape;
      shapeB : constant access constant Shape.b2Shape := Self.m_fixtureB.GetShape;
   begin
      collision.Initialize (worldManifold.all,   Self.m_manifold,
                                                 bodyA.GetTransform,  shapeA.m_radius,
                                                 bodyB.GetTransform,  shapeB.m_radius);
   end GetWorldManifold;



   function IsTouching (Self : in b2Contact) return Boolean
   is
      use type uint32;
   begin
      return (Self.m_flags and e_touchingFlag) = e_touchingFlag;
   end IsTouching;






   procedure SetEnabled (Self : in out b2Contact;   flag : in Boolean)
   is
      use type uint32;
   begin
      if flag then
         Self.m_flags := Self.m_flags or e_enabledFlag;
      else
         Self.m_flags := Self.m_flags and not e_enabledFlag;
      end if;
   end SetEnabled;




   function IsEnabled (Self : in b2Contact) return Boolean
   is
      use type uint32;
   begin
      return (Self.m_flags and e_enabledFlag) = e_enabledFlag;
   end IsEnabled;



   function GetNext (Self : in b2Contact) return View
   is
   begin
      return Self.m_next;
   end GetNext;




   function GetFixtureA (Self : in b2Contact) return access Fixture.b2Fixture'Class
   is
   begin
      return Self.m_fixtureA;
   end GetFixtureA;



   function GetFixtureB (Self : in b2Contact) return access Fixture.b2Fixture'Class
   is
   begin
      return Self.m_fixtureB;
   end GetFixtureB;



   function GetChildIndexA (Self : in b2Contact) return int32
   is
   begin
      return Self.m_indexA;
   end GetChildIndexA;


   function GetChildIndexB (Self : in b2Contact) return int32
   is
   begin
      return Self.m_indexB;
   end GetChildIndexB;





   procedure FlagForFiltering (Self : in out b2Contact)
   is
      use type uint32;
   begin
      Self.m_flags := Self.m_flags or e_filterFlag;
   end FlagForFiltering;





   procedure define   (Self : in out b2Contact;   fixtureA, fixtureB : access Fixture.b2Fixture'Class)
   is
   begin
      Self.m_flags         := e_enabledFlag;

      Self.m_fixtureA      := fixtureA.all'Unchecked_Access;
      Self.m_fixtureB      := fixtureB.all'Unchecked_Access;

      Self.m_manifold.pointCount := 0;

      Self.m_prev          := null;
      Self.m_next          := null;

      Self.m_nodeA.contact := null;
      Self.m_nodeA.prev    := null;
      Self.m_nodeA.next    := null;
      Self.m_nodeA.other   := null;

      Self.m_nodeB.contact := null;
      Self.m_nodeB.prev    := null;
      Self.m_nodeB.next    := null;
      Self.m_nodeB.other   := null;

      Self.m_toiCount      := 0;
   end define;








   procedure AddType (createFcn    : in     b2ContactCreateFcn;
                      destroyFcn   : in     b2ContactDestroyFcn;
                      kindA, kindB : in     shape.Kind)
   is
      use type shape.Kind;
   begin
      pragma Assert (Shape.e_unknown < kindA and then kindA < Shape.e_typeCount);
      pragma Assert (Shape.e_unknown < kindB and then kindB < Shape.e_typeCount);

      s_registers (kindA, kindB).createFcn  := createFcn;
      s_registers (kindA, kindB).destroyFcn := destroyFcn;
      s_registers (kindA, kindB).primary    := True;

      if kindA /= kindB then
         s_registers (kindB, kindA).createFcn  := createFcn;
         s_registers (kindB, kindA).destroyFcn := destroyFcn;
         s_registers (kindB, kindA).primary    := False;
      end if;
   end AddType;




   procedure InitializeRegisters
   is
   begin
      AddType (Contact.circle        .Create'Access,   Contact.circle        .Destroy'Access,   Shape.e_circle,    Shape.e_circle);
      AddType (Contact.polygon_circle.Create'Access,   Contact.polygon_circle.Destroy'Access,   Shape.e_polygon,   Shape.e_circle);
      AddType (Contact.polygon       .Create'Access,   Contact.polygon       .Destroy'Access,   Shape.e_polygon,   Shape.e_polygon);
   end InitializeRegisters;




   function  Create  (fixtureA,
                      fixtureB     : access Fixture.b2Fixture) return access b2Contact'Class
   is
      use type Shape.Kind;
      type1     : Shape.Kind;
      type2     : Shape.Kind;

      createFcn : b2ContactCreateFcn;
   begin
      if s_initialized = False then
         InitializeRegisters;
         s_initialized := True;
      end if;

      type1 := fixtureA.GetKind;
      type2 := fixtureB.GetKind;

      pragma Assert (Shape.e_unknown < type1 and then type1 < Shape.e_typeCount);
      pragma Assert (Shape.e_unknown < type2 and then type2 < Shape.e_typeCount);

      createFcn := s_registers (type1, type2).createFcn;

      if createFcn /= null then
         if s_registers (type1, type2).primary then
            return createFcn (fixtureA, fixtureB);
         else
            return createFcn (fixtureB, fixtureA);
         end if;
      else
         return null;
      end if;
   end Create;







--     procedure Destroy (contact      : access b2Contact;
--                        kindA, kindB : in     shape.Kind);




   procedure Destroy (contact      : in out impact.d2.Contact.view)
   is
      use type shape.Kind;
      typeA     : Shape.Kind;
      typeB     : Shape.Kind;

      destroyFcn : b2ContactDestroyFcn;
   begin
      pragma Assert (s_initialized);

      if contact.m_manifold.pointCount > 0 then
         contact.GetFixtureA.GetBody.SetAwake (True);
         contact.GetFixtureB.GetBody.SetAwake (True);
      end if;

      typeA := contact.GetFixtureA.GetKind;
      typeB := contact.GetFixtureB.GetKind;

      pragma Assert (Shape.e_unknown < typeA and then typeB < Shape.e_typeCount);
      pragma Assert (Shape.e_unknown < typeA and then typeB < Shape.e_typeCount);

      destroyFcn := s_registers (typeA, typeB).destroyFcn;
      destroyFcn (contact);
   end Destroy;







   --  Update the contact manifold and touching status.
   --  Note: do not assume the fixture AABBs are overlapping or are valid.
   --
   procedure Update (Self : in out b2Contact'Class;   listener : access world_callbacks.b2ContactListener'Class)
   is
      use type uint32;

      oldManifold : aliased collision.b2Manifold := Self.m_manifold;
      touching    : Boolean                      := False;

      wasTouching : constant Boolean                      := (Self.m_flags and e_touchingFlag) = e_touchingFlag;

      sensorA     : constant Boolean                      := Self.m_fixtureA.IsSensor;
      sensorB     : constant Boolean                      := Self.m_fixtureB.IsSensor;
      sensor      : constant Boolean                      := sensorA or sensorB;

      bodyA       : constant access Solid.b2Body'Class    := Self.m_fixtureA.GetBody;
      bodyB       : constant access Solid.b2Body'Class    := Self.m_fixtureB.GetBody;

      xfA         : b2Transform             renames bodyA.GetTransform;
      xfB         : b2Transform             renames bodyB.GetTransform;

      mp1, mp2    : access collision.b2ManifoldPoint;
      id2         :        collision.b2ContactID;

   begin
      --  Re-enable this contact.
      Self.m_flags := Self.m_flags or e_enabledFlag;


      --  Is this contact a sensor?
      if sensor then
         touching := collision.b2TestOverlap (Self.m_fixtureA.GetShape, Self.m_fixtureB.GetShape, xfA, xfB);

         --  Sensors don't generate manifolds.
         Self.m_manifold.pointCount := 0;
      else
         Self.Evaluate (Self.m_manifold'Access, xfA, xfB);
         touching := Self.m_manifold.pointCount > 0;

         --  Match old contact ids to new contact ids and copy the
         --  stored impulses to warm start the solver.

         for i in 1 .. Self.m_manifold.pointCount loop
            mp2                := Self.m_manifold.points (i)'Access;
            mp2.normalImpulse  := 0.0;
            mp2.tangentImpulse := 0.0;
            id2                := mp2.id;

            for j in 1 .. oldManifold.pointCount loop
               mp1 := oldManifold.points (j)'Access;

               if mp1.id.key = id2.key then
                  mp2.normalImpulse  := mp1.normalImpulse;
                  mp2.tangentImpulse := mp1.tangentImpulse;
                  exit;
               end if;
            end loop;
         end loop;

         if touching /= wasTouching then
            bodyA.SetAwake (True);
            bodyB.SetAwake (True);
         end if;
      end if;

      if touching then
         Self.m_flags := Self.m_flags or e_touchingFlag;
      else
         Self.m_flags := Self.m_flags and not e_touchingFlag;
      end if;

      if not wasTouching and then touching and then listener /= null then
         listener.BeginContact (Self'Access);
      end if;

      if wasTouching and then not touching and then listener /= null then
         listener.EndContact (Self'Access);
      end if;

      if not sensor and then touching and then listener /= null then
         listener.PreSolve (Self'Access, oldManifold);
      end if;

   end Update;



   --- 'protected' subprograms for use by C 'friend's.
   --

   function m_prev (Self : access b2Contact) return View
   is
   begin
      return Self.m_prev.all'Access;
   end m_prev;


   function m_next (Self : access b2Contact) return View
   is
   begin
      return Contact.view (Self.m_next); --.all'unchecked_access;
   end m_next;


   procedure m_prev_is (Self : in out b2Contact;   Now : in View)
   is
   begin
      Self.m_prev := Now;
   end m_prev_is;


   procedure m_next_is (Self : in out b2Contact;   Now : in View)
   is
   begin
      Self.m_next := Now;
   end m_next_is;


   function  m_toiCount  (Self : in     b2Contact'Class) return  int32
   is
   begin
      return Self.m_toiCount;
   end m_toiCount;



   procedure m_toiCount_is (Self : in out b2Contact'Class;   Now : in int32)
   is
   begin
      Self.m_toiCount := Now;
   end m_toiCount_is;



   function  m_toi  (Self : in     b2Contact'Class) return  float32
   is
   begin
      return Self.m_toi;
   end m_toi;



   procedure m_toi_is (Self : in out b2Contact'Class;   Now : in float32)
   is
   begin
      Self.m_toi := Now;
   end m_toi_is;



   procedure m_flags_are       (Self : in out b2Contact'Class;   Now : in uint32)
   is
   begin
      Self.m_flags := Now;
   end m_flags_are;



   function  m_flags     (Self : in     b2Contact'Class) return  uint32
   is
   begin
      return Self.m_flags;
   end m_flags;




--     function m_nodeA (Self : access b2Contact) return access b2ContactEdge
--     is
--     begin
--        return Self.m_nodeA'access;
--     end;
--
--
--
--     function m_nodeB (Self : access b2Contact) return access b2ContactEdge
--     is
--     begin
--        return Self.m_nodeB'access;
--     end;






end impact.d2.Contact;
