with impact.d2.Math;

limited
with impact.d2.orbs.Solid;



package impact.d2.orbs.Joint
--
--  The base joint class. Joints are used to constraint two bodies together in
--  various fashions. Some joints also feature limits and motors.
--
is
   use impact.d2.Math;


   type b2Joint is abstract tagged private;
   type View    is access all b2Joint'Class;


   type Solid_view is access all impact.d2.orbs.Solid.item'Class;
   type Any_view   is access all Any'Class;


   type b2JointType is (e_unknownJoint,
                        e_revoluteJoint,
                        e_prismaticJoint,
                        e_distanceJoint,
                        e_pulleyJoint,
                        e_mouseJoint,
                        e_gearJoint,
                        e_lineJoint,
                        e_weldJoint,
                        e_frictionJoint);

   type b2LimitState is (e_inactiveLimit,
                         e_atLowerLimit,
                         e_atUpperLimit,
                         e_equalLimits);




   type b2Jacobian is
      record
         linearA  : b2Vec2;
         angularA : float32;
         linearB  : b2Vec2;
         angularB : float32;
      end record;


   procedure SetZero (Self : in out b2Jacobian);
   procedure Set     (Self : in out b2Jacobian;   x1 : b2Vec2;   a1 : float32;
                                                  x2 : b2Vec2;   a2 : float32);

   function Compute (Self : in      b2Jacobian;   x1 : b2Vec2;   a1 : float32;
                                                  x2 : b2Vec2;   a2 : float32) return float32;










   --  A joint edge is used to connect bodies and joints together
   --  in a joint graph where each body is a node and each joint
   --  is an edge. A joint edge belongs to a doubly linked list
   --  maintained in each attached body. Each joint has two joint
   --  nodes, one for each attached body.
   --
   type b2JointEdge;
   type b2JointEdge_view is access all b2JointEdge;

   type b2JointEdge is
      record
        other : Solid_view;                     -- provides quick access to the other body attached.
        joint : impact.d2.orbs.Joint.view;      -- the joint
        prev  : b2JointEdge_view;                -- the previous joint edge in the body's joint list
        next  : b2JointEdge_view;                -- the next joint edge in the body's joint list
      end record;





   --  Joint definitions are used to construct joints.
   --
   type b2JointDef is tagged
      record
        kind             : b2JointType := e_unknownJoint;                -- The joint type is set automatically for concrete joint types.

        userData         : Any_view;                    -- Use this to attach application specific data to your joints.

        bodyA,                                          -- The first  attached body.
        bodyB            : Solid_view;                        -- The second attached body.

        collideConnected : Boolean := False;            -- Set this flag to true if the attached bodies should collide.
      end record;





   --  Get the first body attached to this joint.
   --
   function getBodyA (Self : in b2Joint) return Solid_view;


   --  Get the second body attached to this joint.
   --
   function getBodyB (Self : in b2Joint) return Solid_view;



   --  Get the next joint the world joint list.
   --
   function GetNext (Self : in b2Joint) return Joint.view;


   --  Get the type of the concrete joint.
   --
   function getType (Self : in b2Joint) return b2JointType;



   --  Get the anchor point on bodyA in world coordinates.
   --
   function GetAnchorA (Self : in b2Joint) return b2Vec2
     is abstract;


   --  Get the anchor point on bodyB in world coordinates.
   --
   function GetAnchorB (Self : in b2Joint) return b2Vec2
     is abstract;


   --  Get the reaction force on body2 at the joint anchor in Newtons.
   --
   function GetReactionForce (Self : in b2Joint;   inv_dt : in float32) return b2Vec2
     is abstract;


   --  Get the reaction torque on body2 in N*m
   --
   function GetReactionTorque (Self : in b2Joint;   inv_dt : in float32) return float32
     is abstract;


   --  Get the user data pointer.
   --
   function  GetUserData (Self : in     b2Joint) return Any_view;

   --  Set the user data pointer.
   --
   procedure SetUserData (Self : in out b2Joint;   data : Any_view);


   --  Short-cut function to determine if either body is inactive.
   --
   function  IsActive (Self : in     b2Joint) return Boolean;





   --- 'protected' subprograms for use by C 'friend's.
   --
   procedure define     (Self : in out b2Joint;   Def  : in     b2JointDef'class);
   procedure destruct   (Self : in out b2Joint) is null;

   function  m_collideConnected        (Self : in     b2Joint) return Boolean;

   function  m_edgeA        (Self : access     b2Joint) return b2JointEdge_view;
   function  m_edgeB (Self : access    b2Joint) return b2JointEdge_view;

   function  m_prev (Self : access     b2Joint) return Joint.view;

   procedure m_prev_is (Self : in out b2Joint;   Now : Joint.view);
   procedure m_next_is (Self : in out b2Joint;   Now : Joint.view);

   function  m_islandFlag              (Self : in     b2Joint) return Boolean;
   procedure m_islandFlag_is           (Self : in out b2Joint;   Now : in Boolean);


   procedure InitVelocityConstraints  (Self : in out b2Joint;   step : in b2TimeStep)
   is abstract;

   procedure SolveVelocityConstraints (Self : in out b2Joint;   step : in b2TimeStep)
   is abstract;

   --  This returns true if the position errors are within tolerance.
   --
   function SolvePositionConstraints  (Self : access b2Joint;   baumgarte : float32) return Boolean
   is abstract;


   function  Create  (def  : in     b2JointDef'Class) return Joint.view;
   procedure Destroy (Self : in out Joint.view);





private


   type b2Joint is abstract tagged
      record
         m_type  : b2JointType;

         m_prev  : Joint.view;
         m_next  : Joint.view;

         m_edgeA : aliased b2JointEdge;
         m_edgeB : aliased b2JointEdge;

         m_bodyA : Solid_view;
         m_bodyB : Solid_view;


         m_islandFlag        : Boolean;
         m_collideConnected  : Boolean;

         m_userData          : access Any'Class;

         --  Cache here per time step to reduce cache misses.
         --
         m_localCenterA,
         m_localCenterB      : b2Vec2;

         m_invMassA, m_invIA : float32;
         m_invMassB, m_invIB : float32;
      end record;


end impact.d2.orbs.Joint;
