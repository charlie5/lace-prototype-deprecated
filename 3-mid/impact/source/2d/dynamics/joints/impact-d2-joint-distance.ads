package impact.d2.Joint.distance
--
--  A distance joint constrains two points on two bodies
--  to remain at a fixed distance from each other. You can view
--  this as a massless, rigid rod.
--
is

   type b2DistanceJoint is new b2Joint with private;



   --  Distance joint definition.
   --  This requires defining an anchor point on both bodies and the non-zero length of the
   --  distance joint. The definition uses local anchor points so that the initial configuration
   --  can violate the constraint slightly. This helps when saving and loading a game.
   --
   --  Warning: Do not use a zero or short length.
   --
   type b2DistanceJointDef is new b2JointDef with
      record
         localAnchorA : b2Vec2;    -- The local anchor point relative to body1's origin.
         localAnchorB : b2Vec2;    -- The local anchor point relative to body2's origin.
         length       : float32;   -- The natural length between the anchor points.
         frequencyHz  : float32;   -- The mass-spring-damper frequency in Hertz.
         dampingRatio : float32;   -- The damping ratio. 0 = no damping, 1 = critical damping.
      end record;



   function to_b2DistanceJointDef return b2DistanceJointDef;


   --  Initialize the bodies, anchors, and length using the world anchors.
   --
   procedure initialize (Self : in out b2DistanceJointDef;   bodyA,   bodyB   : Solid_view;
                                                             anchorA, anchorB : in     b2Vec2);




   overriding function GetAnchorA (Self : in b2DistanceJoint) return b2Vec2;
   overriding function GetAnchorB (Self : in b2DistanceJoint) return b2Vec2;

   overriding function GetReactionForce  (Self : in b2DistanceJoint;   inv_dt : in float32) return b2Vec2;
   overriding function GetReactionTorque (Self : in b2DistanceJoint;   inv_dt : in float32) return float32;



   --  Set/get the natural length.
   --  Manipulating the length can lead to non-physical behavior when the frequency is zero.
   --
   function  GetLength (Self : in     b2DistanceJoint)        return float32;
   procedure SetLength (Self : in out b2DistanceJoint;   length : in float32);


   --  Set/get frequency in Hz.
   --
   function  GetFrequency (Self : in     b2DistanceJoint)    return float32;
   procedure SetFrequency (Self : in out b2DistanceJoint;   hz : in float32);


   --  Set/get damping ratio.
   --
   function  GetDampingRatio (Self : in     b2DistanceJoint)       return float32;
   procedure SetDampingRatio (Self : in out b2DistanceJoint;   ratio : in float32);




   function to_b2DistanceJoint (def : in b2DistanceJointDef'Class) return b2DistanceJoint;





   --- 'protected' declarations
   --
   overriding procedure InitVelocityConstraints  (Self : in out b2DistanceJoint;   step : in b2TimeStep);
   overriding procedure SolveVelocityConstraints (Self : in out b2DistanceJoint;   step : in b2TimeStep);

   overriding function  SolvePositionConstraints (Self : access b2DistanceJoint;   baumgarte : in float32) return Boolean;



private

   type b2DistanceJoint is new b2Joint with
      record
         m_localAnchor1 : b2Vec2;
         m_localAnchor2 : b2Vec2;
         m_u            : b2Vec2;
         m_frequencyHz  : float32;
         m_dampingRatio : float32;
         m_gamma        : float32;
         m_bias         : float32;
         m_impulse      : float32;
         m_mass         : float32;
         m_length       : float32;
      end record;


end impact.d2.Joint.distance;
