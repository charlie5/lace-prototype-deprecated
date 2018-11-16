with impact.d3.Object,
     impact.d3.Shape,
     impact.d3.motion_State;
with impact.d3.collision.Proxy;
with impact.d3.Joint;



package impact.d3.Object.rigid
--
--
--
is



   type Item is new impact.d3.Object.item with private;
   type View is access all Item'Class;




   --- rigidFlags
   --
   bt_Disable_World_Gravity : constant Flags := 1;



   gDeactivationTime    : constant math.Real := 2.0;
   gDisableDeactivation : constant Boolean   := False;




   ----------
   --- Forge
   --


   --- ConstructionInfo
   --
   type ConstructionInfo is record
      m_mass                : Real;
      m_motionState         : access impact.d3.motion_State.Item'Class;
      m_startWorldTransform : Transform_3d;

      m_collisionShape      : access impact.d3.Shape.item'Class;

      m_localInertia        : Vector_3;

      m_linearDamping       : Real;
      m_angularDamping      : Real;

      m_friction            : Real;
      m_restitution         : Real;

      m_linearSleepingThreshold  : Real;
      m_angularSleepingThreshold : Real;

      m_additionalDamping                    : Boolean;
      m_additionalDampingFactor              : Real;
      m_additionalLinearDampingThresholdSqr  : Real;
      m_additionalAngularDampingThresholdSqr : Real;
      m_additionalAngularDampingFactor       : Real;
   end record;


   function to_ConstructionInfo (mass           : in     Real                           ;
                                 motionState    : access impact.d3.motion_State.Item'Class;
                                 collisionShape : access impact.d3.Shape       .Item'Class;
                                 localInertia   : in     Vector_3                       ) return ConstructionInfo;




   package Forge
   is
      function to_rigid_Object (mass           : in     math.Real                      ;
                                motionState    : access impact.d3.motion_State.Item'Class;
                                collisionShape : access impact.d3.Shape       .Item'Class;
                                localInertia   : in     math.Vector_3                    := (0.0, 0.0, 0.0)) return Item;
      --
      --  rigid constructor for backwards compatibility.
      --
      --  To specify friction (etc) during rigid body construction, please use the other constructor (using impact.d3.Object.rigidConstructionInfo).
   end Forge;







   ----------------
   --- Atttributes
   --

   --- Dynamics
   --

   function  Site         (Self : in     Item)     return Vector_3;
   procedure Site_is      (Self : in out Item;   Now : in Vector_3);

   function  Spin         (Self : in     Item)     return math.Matrix_3x3;
   procedure Spin_is      (Self : in out Item;   Now : in math.Matrix_3x3);

   function  xy_Spin         (Self : in     Item) return math.Radians;
   procedure xy_Spin_is      (Self : in out Item;   Now : in math.Radians);

   function  Transform    (Self : in     Item)     return math.Matrix_4x4;
   procedure Transform_is (Self : in out Item;   Now : in math.Matrix_4x4);

   function  Speed        (Self : in     Item)     return math.Vector_3;
   procedure Speed_is     (Self : in out Item;   Now : in math.Vector_3);

   function  Gyre         (Self : in     Item)     return math.Vector_3;
   procedure Gyre_is      (Self : in out Item;   Now : in math.Vector_3);


   --- Forces
   --

   procedure apply_Torque         (Self : in out Item;   Torque : in math.Vector_3);
   procedure apply_Torque_impulse (Self : in out Item;   Torque : in math.Vector_3);

   procedure apply_Force          (Self : in out Item;   Force  : in math.Vector_3);







   ---------------
   --- Operations
   --










   --- original ...
   --


   procedure RigidBody_Assert (Self : in item);

   function new_rigid_Object (constructionInfo : in rigid.ConstructionInfo) return View;

   procedure delete (Self : in Item);

   procedure setupRigidBody (Self             : in out Item;
                             constructionInfo : in     rigid.ConstructionInfo);

   procedure proceedToTransform (Self     : in out Item;
                                 newTrans : in Transform_3d);

   procedure predictIntegratedTransform (Self               : in out Item;
                                         timeStep           : in     Real;
                                         predictedTransform :    out Transform_3d);

   procedure saveKinematicState (Self     : in out Item;
                                 timeStep : in Real);



   procedure applyGravity (Self : in out Item);

   procedure setGravity   (Self         : in out Item;
                           acceleration : in Vector_3);

   function  getGravity   (Self : in     Item) return Vector_3;



   procedure setDamping (Self        : in out Item;
                         lin_damping : in     Real;
                         ang_damping : in     Real);

   function getLinearDamping (Self : in Item) return Real;

   function getAngularDamping (Self : in Item) return Real;

   function getLinearSleepingThreshold (Self : in Item) return Real;

   function getAngularSleepingThreshold (Self : in Item) return Real;

   procedure applyDamping (Self     : in out Item;
                           timeStep : in Real);

--     function getCollisionShape (Self : in Item) return access impact.d3.Shape.item'Class;

   procedure setMassProps (Self    : in out Item;
                           mass    : in Real;
                           inertia : in Vector_3);

   function getLinearFactor (Self : in Item) return Vector_3;

   procedure setLinearFactor (Self         : in out Item;
                              linearFactor : in Vector_3);

   function getInvMass (Self : in Item) return Real;

   function getInvInertiaTensorWorld (Self : in Item) return Matrix_3x3;

   procedure integrateVelocities (Self : in out Item;
                                  step : in Real);

   procedure setCenterOfMassTransform (Self  : in out Item;
                                       xform : Transform_3d);

   procedure applyCentralForce (Self  : in out Item;
                                force : in Vector_3);

   function getTotalForce (Self : in Item) return Vector_3;

   function getTotalTorque (Self : in Item) return Vector_3;

   function getInvInertiaDiagLocal (Self : in Item) return Vector_3;

   procedure setInvInertiaDiagLocal (Self           : in out Item;
                                     diagInvInertia : in Vector_3);

   procedure setSleepingThresholds (Self    : in out Item;
                                    linear  : in Real;
                                    angular : in Real);

   procedure applyTorque (Self   : in out Item;
                          torque : in Vector_3);

   procedure applyForce (Self    : in out Item;
                         force   : in Vector_3;
                         rel_pos : in Vector_3);

   procedure applyCentralImpulse (Self    : in out Item;
                                  impulse : in Vector_3);

   procedure applyTorqueImpulse (Self   : in out Item;
                                 torque : in Vector_3);

   procedure applyImpulse (Self    : in out Item;
                           impulse : in Vector_3;
                           rel_pos : in Vector_3);

   procedure clearForces (Self : in out Item);

   procedure updateInertiaTensor (Self : in out Item);

   function getCenterOfMassPosition (Self : in Item) return Vector_3;

   function getOrientation (Self : access Item) return Quaternion;

   function getCenterOfMassTransform (Self : in Item) return Transform_3d;

   function getLinearVelocity (Self : in Item) return Vector_3;

   function getAngularVelocity (Self : in Item) return Vector_3;

   procedure setLinearVelocity (Self    : in out Item;
                                lin_vel : in Vector_3);

   procedure setAngularVelocity (Self    : in out Item;
                                 ang_vel : in Vector_3);

   function getVelocityInLocalPoint (Self    : in Item;
                                     rel_pos : in Vector_3) return Vector_3;

   procedure translate (Self : in out Item;
                        v    : in Vector_3);

   procedure getAabb (Self    : in out Item;
                      aabbMin :    out     Vector_3;
                      aabbMax :    out     Vector_3);

   function computeImpulseDenominator (Self   : in Item;
                                       pos    : in Vector_3;
                                       normal : in Vector_3) return Real;

   function computeAngularImpulseDenominator (Self : in Item;
                                              axis : in Vector_3) return Real;

   procedure UpdateDeactivation (Self     : in out Item;
                                 TimeStep : in     Real);

   function wantsSleeping (Self : in Item) return Boolean;

   function getBroadphaseProxy (Self : access Item) return access impact.d3.collision.Proxy.item;

   procedure setNewBroadphaseProxy (Self            : in out Item;
                                    broadphaseProxy : access     impact.d3.collision.Proxy.item);

--     function getMotionState (Self : in     Item) return        impact.d3.motion_State.Item'Class;
   function getMotionState (Self : in Item) return access impact.d3.motion_State.Item'Class;

   procedure setMotionState (Self        : in out Item;
                             motionState : access impact.d3.motion_State.Item'Class);


   procedure setAngularFactor (Self   : in out Item;
                               angFac : in     Real);

   function getAngularFactor (Self : in Item) return Vector_3;

   function isInWorld (Self : access Item) return Boolean;

   function checkCollideWithOverride (Self : in Item;
                                      co   : access impact.d3.Object.item'Class) return Boolean;

   procedure addConstraintRef (Self : in out Item;
                               c    : access impact.d3.Joint.Item'Class);

   procedure removeConstraintRef (Self : in out Item;
                                  c    : access impact.d3.Joint.item'Class);

   function getConstraintRef (Self  : in Item;
                            index : in Integer) return access impact.d3.Joint.Item'Class;

   function getNumConstraintRefs (Self : in Item) return Ada.Containers.Count_Type;

   procedure setFlags (Self  : in out Item;
                       flags : in     d3.Flags);

   function getFlags (Self : in Item) return Flags;

   function getDeltaLinearVelocity (Self : in Item) return Vector_3;

   function getDeltaAngularVelocity (Self : in Item) return Vector_3;

   function getPushVelocity (Self : in Item) return Vector_3;

   function getTurnVelocity (Self : in Item) return Vector_3;

   function internalGetDeltaLinearVelocity (Self : access Item) return access Vector_3;

   function internalGetDeltaAngularVelocity (Self : access Item) return access Vector_3;

   function internalGetAngularFactor (Self : in Item) return Vector_3;

   function internalGetInvMass (Self : in Item) return Vector_3;

   function internalGetPushVelocity (Self : access Item) return access Vector_3;

   function internalGetTurnVelocity (Self : access Item) return access Vector_3;

   procedure internalGetVelocityInLocalPointObsolete
     (Self     : in Item;
      rel_pos  : in Vector_3;
      velocity :    out Vector_3);

   procedure internalGetAngularVelocity (Self   : in     Item;
                                         angVel :    out Vector_3);

   procedure internalApplyImpulse
     (Self             : in out Item;
      linearComponent  : in Vector_3;
      angularComponent : in Vector_3;
      impulseMagnitude : in Real);

   procedure internalApplyPushImpulse
     (Self             : in out Item;
      linearComponent  : in Vector_3;
      angularComponent : in Vector_3;
      impulseMagnitude : in Real);

   procedure internalWritebackVelocity (Self : in out Item);

   procedure internalWritebackVelocity (Self     : in out Item;
                                        timeStep : in Real);




private

--     type impact.d3.Joint_view is access all impact.d3.Joint.Item'Class;
--
--     package impact.d3.Joint_Vectors is new ada.containers.Vectors (Positive, impact.d3.Joint_view);
--     subtype impact.d3.Joint_Vector  is impact.d3.Joint_Vectors.Vector;


   type Item is new impact.d3.Object.item with
      record
         m_invInertiaTensorWorld : Matrix_3x3;
         m_linearVelocity        : Vector_3;
         m_angularVelocity       : Vector_3;
         m_inverseMass           : Real;
         m_linearFactor          : Vector_3;
         m_gravity               : Vector_3;
         m_gravity_acceleration  : Vector_3;
         m_invInertiaLocal       : Vector_3;
         m_totalForce            : Vector_3;
         m_totalTorque           : Vector_3;
         m_linearDamping         : Real;
         m_angularDamping        : Real;
         m_additionalDamping     : Boolean;
         m_additionalDampingFactor  : Real;
         m_additionalLinearDampingThresholdSqr  : Real;
         m_additionalAngularDampingThresholdSqr : Real;
         m_additionalAngularDampingFactor       : Real;
         m_linearSleepingThreshold  : Real;
         m_angularSleepingThreshold : Real;
         m_optionalMotionState   : access impact.d3.motion_State.Item'Class;
         m_constraintRefs        : impact.d3.Joint.Vector;
         m_rigidbodyFlags        : Flags;
         m_debugBodyId           : Integer;
         m_deltaLinearVelocity   : aliased Vector_3;
         m_deltaAngularVelocity  : aliased Vector_3;
         m_angularFactor         : Vector_3;
         m_invMass               : Vector_3;
         m_pushVelocity          : aliased Vector_3;
         m_turnVelocity          : aliased Vector_3;
         m_contactSolverType     : Integer;
         m_frictionSolverType    : Integer;
      end record;


end impact.d3.Object.rigid;



--  #ifndef BT_RIGIDBODY_H
--  #define BT_RIGIDBODY_H
--
--  #include "LinearMath/btAlignedObjectArray.h"
--  #include "LinearMath/impact.d3.Transform.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Proxy.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.Object.h"
--
--  class impact.d3.Shape;
--  class impact.d3.motion_State;
--  class impact.d3.Joint;
--
--
--
--  #ifdef BT_USE_DOUBLE_PRECISION
--  #define impact.d3.Object.rigidData        impact.d3.Object.rigidDoubleData
--  #define impact.d3.Object.rigidDataName        "impact.d3.Object.rigidDoubleData"
--  #else
--  #define impact.d3.Object.rigidData        impact.d3.Object.rigidFloatData
--  #define impact.d3.Object.rigidDataName        "impact.d3.Object.rigidFloatData"
--  #endif //BT_USE_DOUBLE_PRECISION
--
--
--  enum        impact.d3.Object.rigidFlags
--  {
--          BT_DISABLE_WORLD_GRAVITY = 1
--  };
--
--
--  ///The impact.d3.Object.rigid is the main class for rigid body objects. It is derived from impact.d3.Object, so it keeps a pointer to a impact.d3.Shape.
--  ///It is recommended for performance and memory use to share impact.d3.Shape objects whenever possible.
--  ///There are 3 types of rigid bodies:
--  ///- A) Dynamic rigid bodies, with positive mass. Motion is controlled by rigid body dynamics.
--  ///- B) Fixed objects with zero mass. They are not moving (basically collision objects)
--  ///- C) Kinematic objects, which are objects without mass, but the user can move them. There is on-way interaction, and Bullet calculates a velocity based on the timestep and previous and current world transform.
--  ///Bullet automatically deactivates dynamic rigid bodies, when the velocity is below a threshold for a given time.
--  ///Deactivated (sleeping) rigid bodies don't take any processing time, except a minor broadphase collision detection impact (to allow active objects to activate/wake up sleeping objects)
--  class impact.d3.Object.rigid  : public impact.d3.Object
--  {
--
--          impact.d3.Matrix        m_invInertiaTensorWorld;
--          impact.d3.Vector                m_linearVelocity;
--          impact.d3.Vector                m_angularVelocity;
--          impact.d3.Scalar                m_inverseMass;
--          impact.d3.Vector                m_linearFactor;
--
--          impact.d3.Vector                m_gravity;
--          impact.d3.Vector                m_gravity_acceleration;
--          impact.d3.Vector                m_invInertiaLocal;
--          impact.d3.Vector                m_totalForce;
--          impact.d3.Vector                m_totalTorque;
--
--          impact.d3.Scalar                m_linearDamping;
--          impact.d3.Scalar                m_angularDamping;
--
--          bool                        m_additionalDamping;
--          impact.d3.Scalar                m_additionalDampingFactor;
--          impact.d3.Scalar                m_additionalLinearDampingThresholdSqr;
--          impact.d3.Scalar                m_additionalAngularDampingThresholdSqr;
--          impact.d3.Scalar                m_additionalAngularDampingFactor;
--
--
--          impact.d3.Scalar                m_linearSleepingThreshold;
--          impact.d3.Scalar                m_angularSleepingThreshold;
--
--          //m_optionalMotionState allows to automatic synchronize the world transform for active objects
--          impact.d3.motion_State*        m_optionalMotionState;
--
--          //keep track of typed constraints referencing this rigid body
--          btAlignedObjectArray<impact.d3.Joint*> m_constraintRefs;
--
--          int                                m_rigidbodyFlags;
--
--          int                                m_debugBodyId;
--
--
--  protected:
--
--          ATTRIBUTE_ALIGNED64(impact.d3.Vector                m_deltaLinearVelocity);
--          impact.d3.Vector                m_deltaAngularVelocity;
--          impact.d3.Vector                m_angularFactor;
--          impact.d3.Vector                m_invMass;
--          impact.d3.Vector                m_pushVelocity;
--          impact.d3.Vector                m_turnVelocity;
--
--
--  public:
--
--
--          ///The impact.d3.Object.rigidConstructionInfo structure provides information to create a rigid body. Setting mass to zero creates a fixed (non-dynamic) rigid body.
--          ///For dynamic objects, you can use the collision shape to approximate the local inertia tensor, otherwise use the zero vector (default argument)
--          ///You can use the motion state to synchronize the world transform between physics and graphics objects.
--          ///And if the motion state is provided, the rigid body will initialize its initial world transform from the motion state,
--          ///m_startWorldTransform is only used when you don't provide a motion state.
--          struct        impact.d3.Object.rigidConstructionInfo
--          {
--                  impact.d3.Scalar                        m_mass;
--
--                  ///When a motionState is provided, the rigid body will initialize its world transform from the motion state
--                  ///In this case, m_startWorldTransform is ignored.
--                  impact.d3.motion_State*                m_motionState;
--                  impact.d3.Transform        m_startWorldTransform;
--
--                  impact.d3.Shape*        m_collisionShape;
--                  impact.d3.Vector                        m_localInertia;
--                  impact.d3.Scalar                        m_linearDamping;
--                  impact.d3.Scalar                        m_angularDamping;
--
--                  ///best simulation results when friction is non-zero
--                  impact.d3.Scalar                        m_friction;
--                  ///best simulation results using zero restitution.
--                  impact.d3.Scalar                        m_restitution;
--
--                  impact.d3.Scalar                        m_linearSleepingThreshold;
--                  impact.d3.Scalar                        m_angularSleepingThreshold;
--
--                  //Additional damping can help avoiding lowpass jitter motion, help stability for ragdolls etc.
--                  //Such damping is undesirable, so once the overall simulation quality of the rigid body dynamics system has improved, this should become obsolete
--                  bool                                m_additionalDamping;
--                  impact.d3.Scalar                        m_additionalDampingFactor;
--                  impact.d3.Scalar                        m_additionalLinearDampingThresholdSqr;
--                  impact.d3.Scalar                        m_additionalAngularDampingThresholdSqr;
--                  impact.d3.Scalar                        m_additionalAngularDampingFactor;
--
--                  impact.d3.Object.rigidConstructionInfo(        impact.d3.Scalar mass, impact.d3.motion_State* motionState, impact.d3.Shape* collisionShape, const impact.d3.Vector& localInertia=impact.d3.Vector(0,0,0)):
--                  m_mass(mass),
--                          m_motionState(motionState),
--                          m_collisionShape(collisionShape),
--                          m_localInertia(localInertia),
--                          m_linearDamping(impact.d3.Scalar(0.)),
--                          m_angularDamping(impact.d3.Scalar(0.)),
--                          m_friction(impact.d3.Scalar(0.5)),
--                          m_restitution(impact.d3.Scalar(0.)),
--                          m_linearSleepingThreshold(impact.d3.Scalar(0.8)),
--                          m_angularSleepingThreshold(impact.d3.Scalar(1.f)),
--                          m_additionalDamping(false),
--                          m_additionalDampingFactor(impact.d3.Scalar(0.005)),
--                          m_additionalLinearDampingThresholdSqr(impact.d3.Scalar(0.01)),
--                          m_additionalAngularDampingThresholdSqr(impact.d3.Scalar(0.01)),
--                          m_additionalAngularDampingFactor(impact.d3.Scalar(0.01))
--                  {
--                          m_startWorldTransform.setIdentity();
--                  }
--          };
--
--          ///impact.d3.Object.rigid constructor using construction info
--          impact.d3.Object.rigid(        const impact.d3.Object.rigidConstructionInfo& constructionInfo);
--
--
--
--          virtual ~impact.d3.Object.rigid()
--          {
--                  //No constraints should point to this rigidbody
--                  //Remove constraints from the dynamics world before you delete the related rigidbodies.
--                  btAssert(m_constraintRefs.size()==0);
--          }
--
--  protected:
--
--          ///setupRigidBody is only used internally by the constructor
--          void        setupRigidBody(const impact.d3.Object.rigidConstructionInfo& constructionInfo);
--
--  public:
--
--          void                        proceedToTransform(const impact.d3.Transform& newTrans);
--
--          ///to keep collision detection and dynamics separate we don't store a rigidbody pointer
--          ///but a rigidbody is derived from impact.d3.Object, so we can safely perform an upcast
--          static const impact.d3.Object.rigid*        upcast(const impact.d3.Object* colObj)
--          {
--                  if (colObj->getInternalType()&impact.d3.Object::CO_RIGID_BODY)
--                          return (const impact.d3.Object.rigid*)colObj;
--                  return 0;
--          }
--          static impact.d3.Object.rigid*        upcast(impact.d3.Object* colObj)
--          {
--                  if (colObj->getInternalType()&impact.d3.Object::CO_RIGID_BODY)
--                          return (impact.d3.Object.rigid*)colObj;
--                  return 0;
--          }
--
--          /// continuous collision detection needs prediction
--          void                        predictIntegratedTransform(impact.d3.Scalar step, impact.d3.Transform& predictedTransform) ;
--
--          void                        saveKinematicState(impact.d3.Scalar step);
--
--          void                        applyGravity();
--
--          void                        setGravity(const impact.d3.Vector& acceleration);
--
--          const impact.d3.Vector&        getGravity() const
--          {
--                  return m_gravity_acceleration;
--          }
--
--          void                        setDamping(impact.d3.Scalar lin_damping, impact.d3.Scalar ang_damping);
--
--          impact.d3.Scalar getLinearDamping() const
--          {
--                  return m_linearDamping;
--          }
--
--          impact.d3.Scalar getAngularDamping() const
--          {
--                  return m_angularDamping;
--          }
--
--          impact.d3.Scalar getLinearSleepingThreshold() const
--          {
--                  return m_linearSleepingThreshold;
--          }
--
--          impact.d3.Scalar getAngularSleepingThreshold() const
--          {
--                  return m_angularSleepingThreshold;
--          }
--
--          void                        applyDamping(impact.d3.Scalar timeStep);
--
--
--          void                        setMassProps(impact.d3.Scalar mass, const impact.d3.Vector& inertia);
--
--          const impact.d3.Vector& getLinearFactor() const
--          {
--                  return m_linearFactor;
--          }
--          void setLinearFactor(const impact.d3.Vector& linearFactor)
--          {
--                  m_linearFactor = linearFactor;
--                  m_invMass = m_linearFactor*m_inverseMass;
--          }
--          impact.d3.Scalar                getInvMass() const { return m_inverseMass; }
--          const impact.d3.Matrix& getInvInertiaTensorWorld() const {
--                  return m_invInertiaTensorWorld;
--          }
--
--          void                        integrateVelocities(impact.d3.Scalar step);
--
--          void                        setCenterOfMassTransform(const impact.d3.Transform& xform);
--
--          void                        applyCentralForce(const impact.d3.Vector& force)
--          {
--                  m_totalForce += force*m_linearFactor;
--          }
--
--          const impact.d3.Vector& getTotalForce() const
--          {
--                  return m_totalForce;
--          };
--
--          const impact.d3.Vector& getTotalTorque() const
--          {
--                  return m_totalTorque;
--          };
--
--          const impact.d3.Vector& getInvInertiaDiagLocal() const
--          {
--                  return m_invInertiaLocal;
--          };
--
--          void        setInvInertiaDiagLocal(const impact.d3.Vector& diagInvInertia)
--          {
--                  m_invInertiaLocal = diagInvInertia;
--          }
--
--          void        setSleepingThresholds(impact.d3.Scalar linear,impact.d3.Scalar angular)
--          {
--                  m_linearSleepingThreshold = linear;
--                  m_angularSleepingThreshold = angular;
--          }
--
--          void        applyTorque(const impact.d3.Vector& torque)
--          {
--                  m_totalTorque += torque*m_angularFactor;
--          }
--
--          void        applyForce(const impact.d3.Vector& force, const impact.d3.Vector& rel_pos)
--          {
--                  applyCentralForce(force);
--                  applyTorque(rel_pos.cross(force*m_linearFactor));
--          }
--
--          void applyCentralImpulse(const impact.d3.Vector& impulse)
--          {
--                  m_linearVelocity += impulse *m_linearFactor * m_inverseMass;
--          }
--
--            void applyTorqueImpulse(const impact.d3.Vector& torque)
--          {
--                          m_angularVelocity += m_invInertiaTensorWorld * torque * m_angularFactor;
--          }
--
--          void applyImpulse(const impact.d3.Vector& impulse, const impact.d3.Vector& rel_pos)
--          {
--                  if (m_inverseMass != impact.d3.Scalar(0.))
--                  {
--                          applyCentralImpulse(impulse);
--                          if (m_angularFactor)
--                          {
--                                  applyTorqueImpulse(rel_pos.cross(impulse*m_linearFactor));
--                          }
--                  }
--          }
--
--          void clearForces()
--          {
--                  m_totalForce.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
--                  m_totalTorque.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
--          }
--
--          void updateInertiaTensor();
--
--          const impact.d3.Vector&     getCenterOfMassPosition() const {
--                  return m_worldTransform.getOrigin();
--          }
--          impact.d3.Quaternion getOrientation() const;
--
--          const impact.d3.Transform&  getCenterOfMassTransform() const {
--                  return m_worldTransform;
--          }
--          const impact.d3.Vector&   getLinearVelocity() const {
--                  return m_linearVelocity;
--          }
--          const impact.d3.Vector&    getAngularVelocity() const {
--                  return m_angularVelocity;
--          }
--
--
--          inline void setLinearVelocity(const impact.d3.Vector& lin_vel)
--          {
--                  m_linearVelocity = lin_vel;
--          }
--
--          inline void setAngularVelocity(const impact.d3.Vector& ang_vel)
--          {
--                  m_angularVelocity = ang_vel;
--          }
--
--          impact.d3.Vector getVelocityInLocalPoint(const impact.d3.Vector& rel_pos) const
--          {
--                  //we also calculate lin/ang velocity for kinematic objects
--                  return m_linearVelocity + m_angularVelocity.cross(rel_pos);
--
--                  //for kinematic objects, we could also use use:
--                  //                return         (m_worldTransform(rel_pos) - m_interpolationWorldTransform(rel_pos)) / m_kinematicTimeStep;
--          }
--
--          void translate(const impact.d3.Vector& v)
--          {
--                  m_worldTransform.getOrigin() += v;
--          }
--
--
--          void        getAabb(impact.d3.Vector& aabbMin,impact.d3.Vector& aabbMax) const;
--
--
--
--
--
--          SIMD_FORCE_INLINE impact.d3.Scalar computeImpulseDenominator(const impact.d3.Vector& pos, const impact.d3.Vector& normal) const
--          {
--                  impact.d3.Vector r0 = pos - getCenterOfMassPosition();
--
--                  impact.d3.Vector c0 = (r0).cross(normal);
--
--                  impact.d3.Vector vec = (c0 * getInvInertiaTensorWorld()).cross(r0);
--
--                  return m_inverseMass + normal.dot(vec);
--
--          }
--
--          SIMD_FORCE_INLINE impact.d3.Scalar computeAngularImpulseDenominator(const impact.d3.Vector& axis) const
--          {
--                  impact.d3.Vector vec = axis * getInvInertiaTensorWorld();
--                  return axis.dot(vec);
--          }
--
--          SIMD_FORCE_INLINE void        updateDeactivation(impact.d3.Scalar timeStep)
--          {
--                  if ( (getActivationState() == ISLAND_SLEEPING) || (getActivationState() == DISABLE_DEACTIVATION))
--                          return;
--
--                  if ((getLinearVelocity().length2() < m_linearSleepingThreshold*m_linearSleepingThreshold) &&
--                          (getAngularVelocity().length2() < m_angularSleepingThreshold*m_angularSleepingThreshold))
--                  {
--                          m_deactivationTime += timeStep;
--                  } else
--                  {
--                          m_deactivationTime=impact.d3.Scalar(0.);
--                          setActivationState(0);
--                  }
--
--          }
--
--          SIMD_FORCE_INLINE bool        wantsSleeping()
--          {
--
--                  if (getActivationState() == DISABLE_DEACTIVATION)
--                          return false;
--
--                  //disable deactivation
--                  if (gDisableDeactivation || (gDeactivationTime == impact.d3.Scalar(0.)))
--                          return false;
--
--                  if ( (getActivationState() == ISLAND_SLEEPING) || (getActivationState() == WANTS_DEACTIVATION))
--                          return true;
--
--                  if (m_deactivationTime> gDeactivationTime)
--                  {
--                          return true;
--                  }
--                  return false;
--          }
--
--
--
--          const impact.d3.collision.Proxy*        getBroadphaseProxy() const
--          {
--                  return m_broadphaseHandle;
--          }
--          impact.d3.collision.Proxy*        getBroadphaseProxy()
--          {
--                  return m_broadphaseHandle;
--          }
--          void        setNewBroadphaseProxy(impact.d3.collision.Proxy* broadphaseProxy)
--          {
--                  m_broadphaseHandle = broadphaseProxy;
--          }
--
--          //impact.d3.motion_State allows to automatic synchronize the world transform for active objects
--          impact.d3.motion_State*        getMotionState()
--          {
--                  return m_optionalMotionState;
--          }
--          const impact.d3.motion_State*        getMotionState() const
--          {
--                  return m_optionalMotionState;
--          }
--          void        setMotionState(impact.d3.motion_State* motionState)
--          {
--                  m_optionalMotionState = motionState;
--                  if (m_optionalMotionState)
--                          motionState->getWorldTransform(m_worldTransform);
--          }
--
--          //for experimental overriding of friction/contact solver func
--          int        m_contactSolverType;
--          int        m_frictionSolverType;
--
--          void        setAngularFactor(const impact.d3.Vector& angFac)
--          {
--                  m_angularFactor = angFac;
--          }
--
--          void        setAngularFactor(impact.d3.Scalar angFac)
--          {
--                  m_angularFactor.setValue(angFac,angFac,angFac);
--          }
--          const impact.d3.Vector&        getAngularFactor() const
--          {
--                  return m_angularFactor;
--          }
--
--          //is this rigidbody added to a impact.d3.Space/impact.d3.Space.dynamic/btBroadphase?
--          bool        isInWorld() const
--          {
--                  return (getBroadphaseProxy() != 0);
--          }
--
--          virtual bool checkCollideWithOverride(impact.d3.Object* co);
--
--          void addConstraintRef(impact.d3.Joint* c);
--          void removeConstraintRef(impact.d3.Joint* c);
--
--          impact.d3.Joint* getConstraintRef(int index)
--          {
--                  return m_constraintRefs[index];
--          }
--
--          int getNumConstraintRefs() const
--          {
--                  return m_constraintRefs.size();
--          }
--
--          void        setFlags(int flags)
--          {
--                  m_rigidbodyFlags = flags;
--          }
--
--          int getFlags() const
--          {
--                  return m_rigidbodyFlags;
--          }
--
--          const impact.d3.Vector& getDeltaLinearVelocity() const
--          {
--                  return m_deltaLinearVelocity;
--          }
--
--          const impact.d3.Vector& getDeltaAngularVelocity() const
--          {
--                  return m_deltaAngularVelocity;
--          }
--
--          const impact.d3.Vector& getPushVelocity() const
--          {
--                  return m_pushVelocity;
--          }
--
--          const impact.d3.Vector& getTurnVelocity() const
--          {
--                  return m_turnVelocity;
--          }
--
--
--          ////////////////////////////////////////////////
--          ///some internal methods, don't use them
--
--          impact.d3.Vector& internalGetDeltaLinearVelocity()
--          {
--                  return m_deltaLinearVelocity;
--          }
--
--          impact.d3.Vector& internalGetDeltaAngularVelocity()
--          {
--                  return m_deltaAngularVelocity;
--          }
--
--          const impact.d3.Vector& internalGetAngularFactor() const
--          {
--                  return m_angularFactor;
--          }
--
--          const impact.d3.Vector& internalGetInvMass() const
--          {
--                  return m_invMass;
--          }
--
--          impact.d3.Vector& internalGetPushVelocity()
--          {
--                  return m_pushVelocity;
--          }
--
--          impact.d3.Vector& internalGetTurnVelocity()
--          {
--                  return m_turnVelocity;
--          }
--
--          SIMD_FORCE_INLINE void        internalGetVelocityInLocalPointObsolete(const impact.d3.Vector& rel_pos, impact.d3.Vector& velocity ) const
--          {
--                  velocity = getLinearVelocity()+m_deltaLinearVelocity + (getAngularVelocity()+m_deltaAngularVelocity).cross(rel_pos);
--          }
--
--          SIMD_FORCE_INLINE void        internalGetAngularVelocity(impact.d3.Vector& angVel) const
--          {
--                  angVel = getAngularVelocity()+m_deltaAngularVelocity;
--          }
--
--
--          //Optimization for the iterative solver: avoid calculating constant terms involving inertia, normal, relative position
--          SIMD_FORCE_INLINE void internalApplyImpulse(const impact.d3.Vector& linearComponent, const impact.d3.Vector& angularComponent,const impact.d3.Scalar impulseMagnitude)
--          {
--                  if (m_inverseMass)
--                  {
--                          m_deltaLinearVelocity += linearComponent*impulseMagnitude;
--                          m_deltaAngularVelocity += angularComponent*(impulseMagnitude*m_angularFactor);
--                  }
--          }
--
--          SIMD_FORCE_INLINE void internalApplyPushImpulse(const impact.d3.Vector& linearComponent, const impact.d3.Vector& angularComponent,impact.d3.Scalar impulseMagnitude)
--          {
--                  if (m_inverseMass)
--                  {
--                          m_pushVelocity += linearComponent*impulseMagnitude;
--                          m_turnVelocity += angularComponent*(impulseMagnitude*m_angularFactor);
--                  }
--          }
--
--          void        internalWritebackVelocity()
--          {
--                  if (m_inverseMass)
--                  {
--                          setLinearVelocity(getLinearVelocity()+ m_deltaLinearVelocity);
--                          setAngularVelocity(getAngularVelocity()+m_deltaAngularVelocity);
--                          //m_deltaLinearVelocity.setZero();
--                          //m_deltaAngularVelocity .setZero();
--                          //m_originalBody->setCompanionId(-1);
--                  }
--          }
--
--
--          void        internalWritebackVelocity(impact.d3.Scalar timeStep);
--
--
--
--          ///////////////////////////////////////////////
--
--          virtual        int        calculateSerializeBufferSize()        const;
--
--          ///fills the dataBuffer and returns the struct name (and 0 on failure)
--          virtual        const char*        serialize(void* dataBuffer,  class btSerializer* serializer) const;
--
--          virtual void serializeSingleObject(class btSerializer* serializer) const;
--
--  };
--
--  //@todo add m_optionalMotionState and m_constraintRefs to impact.d3.Object.rigidData
--  ///do not change those serialization structures, it requires an updated sBulletDNAstr/sBulletDNAstr64
--  struct        impact.d3.Object.rigidFloatData
--  {
--          impact.d3.ObjectFloatData        m_collisionObjectData;
--          impact.d3.MatrixFloatData                m_invInertiaTensorWorld;
--          impact.d3.VectorFloatData                m_linearVelocity;
--          impact.d3.VectorFloatData                m_angularVelocity;
--          impact.d3.VectorFloatData                m_angularFactor;
--          impact.d3.VectorFloatData                m_linearFactor;
--          impact.d3.VectorFloatData                m_gravity;
--          impact.d3.VectorFloatData                m_gravity_acceleration;
--          impact.d3.VectorFloatData                m_invInertiaLocal;
--          impact.d3.VectorFloatData                m_totalForce;
--          impact.d3.VectorFloatData                m_totalTorque;
--          float                                        m_inverseMass;
--          float                                        m_linearDamping;
--          float                                        m_angularDamping;
--          float                                        m_additionalDampingFactor;
--          float                                        m_additionalLinearDampingThresholdSqr;
--          float                                        m_additionalAngularDampingThresholdSqr;
--          float                                        m_additionalAngularDampingFactor;
--          float                                        m_linearSleepingThreshold;
--          float                                        m_angularSleepingThreshold;
--          int                                                m_additionalDamping;
--  };
--
--  ///do not change those serialization structures, it requires an updated sBulletDNAstr/sBulletDNAstr64
--  struct        impact.d3.Object.rigidDoubleData
--  {
--          impact.d3.ObjectDoubleData        m_collisionObjectData;
--          impact.d3.MatrixDoubleData                m_invInertiaTensorWorld;
--          impact.d3.VectorDoubleData                m_linearVelocity;
--          impact.d3.VectorDoubleData                m_angularVelocity;
--          impact.d3.VectorDoubleData                m_angularFactor;
--          impact.d3.VectorDoubleData                m_linearFactor;
--          impact.d3.VectorDoubleData                m_gravity;
--          impact.d3.VectorDoubleData                m_gravity_acceleration;
--          impact.d3.VectorDoubleData                m_invInertiaLocal;
--          impact.d3.VectorDoubleData                m_totalForce;
--          impact.d3.VectorDoubleData                m_totalTorque;
--          double                                        m_inverseMass;
--          double                                        m_linearDamping;
--          double                                        m_angularDamping;
--          double                                        m_additionalDampingFactor;
--          double                                        m_additionalLinearDampingThresholdSqr;
--          double                                        m_additionalAngularDampingThresholdSqr;
--          double                                        m_additionalAngularDampingFactor;
--          double                                        m_linearSleepingThreshold;
--          double                                        m_angularSleepingThreshold;
--          int                                                m_additionalDamping;
--          char        m_padding[4];
--  };
--
--
--
--  #endif //BT_RIGIDBODY_H
