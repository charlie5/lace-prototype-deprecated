with impact.d2.orbs.Joint.distance,
     impact.d2.orbs.Joint.friction,
     impact.d2.orbs.Solid,

     ada.unchecked_Deallocation;




package body impact.d2.orbs.Joint
is


   function  Create (def  : in b2JointDef'Class) return Joint.view
   is
      use impact.d2.orbs.Joint.distance, impact.d2.orbs.Joint.friction;

      joint : access b2Joint'Class;
      pragma Unreferenced (joint);
   begin
      case def.kind is
      when e_distanceJoint  => joint := new b2DistanceJoint '(to_b2DistanceJoint  (b2DistanceJointDef  (def)));
--        when e_mouseJoint     => joint := new b2MouseJoint    ' (to_b2MouseJoint     (b2MouseJointDef     (def)));
--        when e_prismaticJoint => joint := new b2PrismaticJoint' (to_b2PrismaticJoint (b2PrismaticJointDef (def)));
--        when e_revoluteJoint  => joint := new b2RevoluteJoint ' (to_b2RevoluteJoint  (b2RevoluteJoint     (def)));
--        when e_pulleyJoint    => joint := new b2PulleyJoint   ' (to_b2PulleyJoint    (b2PulleyJointDef    (def)));
--        when e_gearJoint      => joint := new b2GearJoint     ' (to_b2GearJoint      (b2GearJointDef      (def)));
--        when e_lineJoint      => joint := new b2LineJoint     ' (to_b2LineJoint      (b2LineJointDef      (def)));
--        when e_weldJoint      => joint := new b2WeldJoint     ' (to_b2WeldJoint      (b2WeldJointDef      (def)));
      when e_frictionJoint  => joint := new b2FrictionJoint '(to_b2FrictionJoint  (b2FrictionJointDef  (def)));
      when others =>
         pragma Assert (False);
         raise Program_Error;
      end case;

      return null; -- joint;
   end Create;




   procedure Destroy (Self : in out Joint.view)
   is
      procedure free is new ada.unchecked_Deallocation (b2Joint'Class, Joint.view);
   begin
      Self.destruct;
      free (Self);
   end Destroy;







   procedure SetZero (Self : in out b2Jacobian)
   is
   begin
      Self.linearA  := (0.0, 0.0);
      Self.angularA := 0.0;

      Self.linearB  := (0.0, 0.0);
      Self.angularB := 0.0;
   end SetZero;







   procedure Set     (Self : in out b2Jacobian;   x1 : b2Vec2;   a1 : float32;
                                                  x2 : b2Vec2;   a2 : float32)
   is
   begin
      Self.linearA  := x1;
      Self.angularA := a1;

      Self.linearB  := x2;
      Self.angularB := a2;
   end Set;



   function Compute (Self : in      b2Jacobian;   x1 : b2Vec2;   a1 : float32;
                                                  x2 : b2Vec2;   a2 : float32) return float32
   is
   begin
      return    b2Dot (Self.linearA, x1)  +  Self.angularA * a1
             +  b2Dot (Self.linearB, x2)  +  Self.angularB * a2;
   end Compute;





   function GetNext (Self : in b2Joint) return Joint.view
   is
   begin
      return Self.m_next;
   end GetNext;



   procedure m_islandFlag_is                 (Self : in out b2Joint;   Now : in Boolean)
   is
   begin
      Self.m_islandFlag := Now;
   end m_islandFlag_is;




   function  m_islandFlag              (Self : in     b2Joint) return Boolean
   is
   begin
      return Self.m_islandFlag;
   end m_islandFlag;





   function m_collideConnected (Self : in b2Joint) return Boolean
   is
   begin
      return Self.m_collideConnected;
   end m_collideConnected;



   function m_edgeA (Self : access     b2Joint) return b2JointEdge_view
   is
   begin
      return Self.m_edgeA'Access;
   end m_edgeA;



   function m_edgeB (Self : access    b2Joint) return b2JointEdge_view
   is
   begin
      return Self.m_edgeB'Access;
   end m_edgeB;




   function  m_prev         (Self : access     b2Joint) return Joint.view
   is
   begin
      return Self.m_prev;
   end m_prev;



   procedure m_prev_is (Self : in out b2Joint;   Now : Joint.view)
   is
   begin
      Self.m_prev := Now;
   end m_prev_is;


   procedure m_next_is (Self : in out b2Joint;   Now : Joint.view)
   is
   begin
      Self.m_next := Now;
   end m_next_is;





   function getBodyA (Self : in b2Joint) return Solid_view
   is
   begin
      return Self.m_bodyA;
   end getBodyA;




   function getBodyB (Self : in b2Joint) return Solid_view
   is
   begin
      return Self.m_bodyB;
   end getBodyB;





   function getType (Self : in b2Joint) return b2JointType
   is
   begin
      return Self.m_type;
   end getType;






   function  GetUserData (Self : in     b2Joint) return Any_view
   is
   begin
      return Self.m_userData;
   end GetUserData;



   procedure SetUserData (Self : in out b2Joint;   data : Any_view)
   is
   begin
      Self.m_userData := data;
   end SetUserData;



   function  IsActive (Self : in     b2Joint) return Boolean
   is
   begin
      return Self.m_bodyA.IsActive and then Self.m_bodyB.IsActive;
   end IsActive;




   procedure define     (Self : in out b2Joint;   Def  : in     b2JointDef'class)
   is
   begin
      pragma Assert (def.bodyA /= def.bodyB);

      Self.m_type             := def.kind;
      Self.m_bodyA            := def.bodyA;
      Self.m_bodyB            := def.bodyB;
      Self.m_collideConnected := def.collideConnected;
      Self.m_islandFlag       := False;
      Self.m_userData         := def.userData;
   end define;


end impact.d2.orbs.Joint;
