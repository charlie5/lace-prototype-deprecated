package impact.d2.Joint.friction
--
--  This is used for top-down friction. It provides 2D translational friction and angular friction.
--
is

   type b2FrictionJoint is new b2Joint with private;



   --  Friction joint definition.
   --
   type b2FrictionJointDef is new b2JointDef with
      record
         localAnchorA : b2Vec2;    -- The local anchor point relative to bodyA's origin.
         localAnchorB : b2Vec2;    -- The local anchor point relative to bodyB's origin.
         maxForce     : float32;   -- The maximum friction force in N.
         maxTorque    : float32;   -- The maximum friction torque in N-m.
      end record;


   function to_b2FrictionJointDef return b2FrictionJointDef;


   --  Initialize the bodies, anchors, axis, and reference angle using the world anchor and world axis.
   --
   procedure initialize (Self : in out b2FrictionJointDef;   bodyA,   bodyB   : in  Solid_view;
                         anchor           : in  b2Vec2);





   overriding function GetAnchorA (Self : in b2FrictionJoint) return b2Vec2;
   overriding function GetAnchorB (Self : in b2FrictionJoint) return b2Vec2;

   overriding function GetReactionForce  (Self : in b2FrictionJoint;   inv_dt : in float32) return b2Vec2;
   overriding function GetReactionTorque (Self : in b2FrictionJoint;   inv_dt : in float32) return float32;


   --  Get/set the maximum friction force in N.
   --
   function  GetMaxForce (Self : in     b2FrictionJoint)       return float32;
   procedure SetMaxForce (Self : in out b2FrictionJoint;   force : in float32);


   --  Get/set the maximum friction torque in N*m.
   --
   function  GetMaxTorque (Self : in     b2FrictionJoint)        return float32;
   procedure SetMaxTorque (Self : in out b2FrictionJoint;   torque : in float32);




   --- 'protected' declarations
   --

   function  to_b2FrictionJoint (def : in b2FrictionJointDef'Class) return b2FrictionJoint;

   overriding procedure InitVelocityConstraints  (Self : in out b2FrictionJoint;   step : in b2TimeStep);
   overriding procedure SolveVelocityConstraints (Self : in out b2FrictionJoint;   step : in b2TimeStep);

   overriding function  SolvePositionConstraints (Self : access b2FrictionJoint;   baumgarte : in float32) return Boolean;




private

   type b2FrictionJoint is new b2Joint with
      record
         m_localAnchorA : b2Vec2;
         m_localAnchorB : b2Vec2;

         m_linearMass   : b2Mat22;
         m_angularMass  : float32;

         m_linearImpulse  : b2Vec2;
         m_angularImpulse : float32;

         m_maxForce  : float32;
         m_maxTorque : float32;
      end record;

end impact.d2.Joint.friction;
