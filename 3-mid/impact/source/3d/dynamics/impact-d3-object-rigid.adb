with impact.d3.Vector; use impact.d3.Vector;
with impact.d3.Transform;
with impact.d3.Matrix,
     impact.d3.min_max;
with impact.d3.Transform_Util;



package body impact.d3.Object.rigid
is
   use math.Algebra.linear.d3;

   function to_ConstructionInfo (mass           : in     Real;
                                 motionState    : access impact.d3.motion_State.Item'Class;
                                 collisionShape : access impact.d3.Shape.Item'Class;
                                 localInertia   : in     Vector_3)
                                 return ConstructionInfo is
      Self : ConstructionInfo;
   begin
      Self.m_mass           := mass;
      Self.m_motionState    := motionState;
      Self.m_collisionShape := collisionShape;
      Self.m_localInertia   := localInertia;
      Self.m_linearDamping  := Real (0.0);
      Self.m_angularDamping := Real (0.0);
      Self.m_friction       := Real (0.5);
      Self.m_restitution    := Real (0.0);

      Self.m_linearSleepingThreshold              := Real (0.8);
      Self.m_angularSleepingThreshold             := Real (1.0);
      Self.m_additionalDamping                    := False;
      Self.m_additionalDampingFactor              := Real (0.005);
      Self.m_additionalLinearDampingThresholdSqr  := Real (0.01);
      Self.m_additionalAngularDampingThresholdSqr := Real (0.01);
      Self.m_additionalDampingFactor              := Real (0.01);

      Self.m_startWorldTransform := impact.d3.Transform.getIdentity;

      return Self;
   end to_ConstructionInfo;

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





   package body Forge
   is

      function to_rigid_Object (mass           : in     math.Real;
                                motionState    : access impact.d3.motion_State.Item'Class;
                                collisionShape : access impact.d3.Shape.Item'Class;
                                localInertia   : in     math.Vector_3              := (0.0, 0.0, 0.0)) return Item
      is
         Self  : Item;
         cinfo : constant ConstructionInfo := to_ConstructionInfo (mass, motionState, collisionShape, localInertia);
      begin
         Self.setupRigidBody (cinfo);
         return Self;
      end to_rigid_Object;

      --     impact.d3.Object.rigidConstructionInfo cinfo(mass,motionState,collisionShape,localInertia);
      --     setupRigidBody(cinfo);

   end Forge;










   ----------------
   --- Atttributes
   --


   --- Dynamics
   --

   function Site (Self : in    Item) return Vector_3
   is
      the_Transform : Transform_3d;
   begin
      Self.getMotionState.getWorldTransform (the_Transform);
      return the_Transform.Translation;
   end Site;



   procedure Site_is (Self : in out Item;   Now : in Vector_3)
   is
      the_Transform : Transform_3d;
   begin
      Self.getMotionState.getWorldTransform (the_Transform);
      the_Transform.Translation := Now;
      Self.getMotionState.setWorldTransform (the_Transform);

      Self.getWorldTransform.Translation := Now;

      Self.activate (True);
   end Site_is;



--      btTransform&   trans    = getWorldTransform ();
--      btTransform    trans2; //   = getWorldTransform ();
--
--      getMotionState()->getWorldTransform (trans2);
--
--      trans.setOrigin (btVector3 (Now.x, Now.y, Now.z));
--      trans2.setOrigin (btVector3 (Now.x, Now.y, Now.z));
--
--  //    setActivationState (1);
--      activate();
--      getMotionState()->setWorldTransform (trans2);



   function Spin (Self : in Item) return math.Matrix_3x3
   is
      trans : constant Transform_3d := Self.WorldTransform;
   begin
      return trans.Rotation;   -- tbd: check that no transpose is required (check others below also)

   end Spin;

--      btTransform&   trans    = as_btCollisionObject().getWorldTransform ();
--      btMatrix3x3    the_Spin = trans.getBasis();
--
--      btVector3&     R1       = the_Spin [0];
--      btVector3&     R2       = the_Spin [1];
--      btVector3&     R3       = the_Spin [2];
--
--      return c_physics::Matrix_3x3 (R1 [0],  R1 [1],  R1 [2],
--                                        R2 [0],  R2 [1],  R2 [2],
--                                    R3 [0],  R3 [1],  R3 [2]);
--

   procedure Spin_is      (Self : in out Item;   Now : in math.Matrix_3x3)
   is
      trans : Transform_3d := Self.WorldTransform;
   begin
      trans.Rotation := Now;
      Self.setWorldTransform (trans);

--        declare
--           trans2 : Transform_3d := Self.WorldTransform;
--        begin
--           Self.getMotionState.getWorldTransform (trans2);
--           trans2.Rotation := Now;
--           Self.activate;
--           Self.getMotionState.setWorldTransform (trans2);
--        end;

   end Spin_is;



   function  xy_Spin         (Self : in     Item) return math.Radians
   is
   begin
      raise Program_Error with "TBD";
      return 0.0;
   end xy_Spin;



   procedure xy_Spin_is      (Self : in out Item;   Now : in math.Radians)
   is
   begin
      raise Program_Error with "TBD";
   end xy_Spin_is;



   --  for object ...

--      btTransform&   trans = as_btCollisionObject().getWorldTransform ();
--
--      trans.setBasis (btMatrix3x3 (Now.m00, Now.m01, Now.m02,
--                                   Now.m10, Now.m11, Now.m12,
--                                   Now.m20, Now.m21, Now.m22));



   --  for rigid ...

--      btTransform&   trans = as_btCollisionObject().getWorldTransform ();
--
--      trans.setBasis (btMatrix3x3 (Now.m00, Now.m01, Now.m02,
--                                   Now.m10, Now.m11, Now.m12,
--                                   Now.m20, Now.m21, Now.m22));
--
--
--      btTransform    trans2; //   = getWorldTransform ();
--
--      getMotionState()->getWorldTransform (trans2);
--
--      trans2.setBasis (btMatrix3x3 (Now.m00, Now.m01, Now.m02,
--                                   Now.m10, Now.m11, Now.m12,
--                                   Now.m20, Now.m21, Now.m22));
--  //    setActivationState (1);
--      activate();
--      getMotionState()->setWorldTransform (trans2);






   function  Transform    (Self : in     Item)     return math.Matrix_4x4
   is
   begin
      return to_transform_Matrix (Self.WorldTransform);
   end Transform;


--      btTransform&   trans          = as_btCollisionObject().getWorldTransform ();
--      btScalar       gl_Matrix [16];
--
--      trans.getOpenGLMatrix (gl_Matrix);
--
--      return c_physics::Matrix_4x4 (gl_Matrix);





   procedure Transform_is (Self : in out Item;   Now : in math.Matrix_4x4)
   is
      the_Transform : constant Transform_3d := to_Transform (Now);
   begin
      Self.getWorldTransform.all := the_Transform;
      Self.getMotionState.setWorldTransform (the_Transform);
   end Transform_is;


--    void
--    Rigid::
--    Transform_is (c_physics::Matrix_4x4&   Now)
--    {
--      btTransform&   trans    = getWorldTransform ();
--
--      trans.setFromOpenGLMatrix (&Now.m00);
--      activate();
--
--      getMotionState()->setWorldTransform (trans);
--    }






   function  Speed        (Self : in     Item)     return math.Vector_3
   is
   begin
      return Self.getLinearVelocity;
   end Speed;


--    c_physics::Vector_3
--    Rigid::
--    Speed ()
--    {
--      btVector3      the_Speed = getLinearVelocity ();
--
--      return c_physics::Vector_3 (the_Speed [0],  the_Speed [1],  the_Speed [2]);
--    }




   procedure Speed_is (Self : in out Item;   Now : in math.Vector_3)
   is
   begin
      Self.setLinearVelocity (Now);

      if         Now (1) = 0.0
        and then Now (2) = 0.0
        and then Now (3) = 0.0
      then
         Self.setActivationState (0);
      else
         Self.activate;
      end if;
   end Speed_is;






   function  Gyre         (Self : in     Item)     return math.Vector_3
   is
   begin
      return Self.getAngularVelocity;
   end Gyre;



   procedure Gyre_is      (Self : in out Item;   Now : in math.Vector_3)
   is
   begin
      Self.setAngularVelocity (Now);
      Self.activate;
   end Gyre_is;







   --- Forces
   --

   procedure apply_Torque         (Self : in out Item;   Torque : in math.Vector_3)
   is
   begin
      Self.applyTorque (Torque);
      Self.activate;
   end apply_Torque;



   procedure apply_Torque_impulse (Self : in out Item;   Torque : in math.Vector_3)
   is
   begin
      Self.applyTorqueImpulse (Torque);
      Self.activate;
   end apply_Torque_impulse;




   procedure apply_Force          (Self : in out Item;   Force  : in math.Vector_3)
   is
   begin
      Self.applyCentralImpulse (Force);
      Self.activate;
   end apply_Force;















   --- old ...


   procedure RigidBody_Assert (Self : in Item) is
      use  ada.containers;
      pragma Assert (Self.m_constraintRefs.Length = 0);
   begin
      null;
   end RigidBody_Assert;

   --          virtual ~impact.d3.Object.rigid()
   --          {
   --                  //No constraints should point to this rigidbody
   --                  //Remove constraints from the dynamics world before you delete the related rigidbodies.
   --                  btAssert(m_constraintRefs.size()==0);
   --          }





   function new_rigid_Object (constructionInfo : in rigid.ConstructionInfo) return View
   is
      Self : constant View := new Item;
   begin
      setupRigidBody (Self.all, constructionInfo);

      return Self;
--        return Self : Item
--        do
--           setupRigidBody (Self, constructionInfo);
--
--        end return;

   end new_rigid_Object;





   procedure delete (Self : in Item)
   is
   begin
      null;
   end delete;





   procedure setupRigidBody (Self             : in out Item;
                             constructionInfo : in     rigid.ConstructionInfo)
   is
   begin
      impact.d3.Object.define (impact.d3.Object.item (Self));

      Self.setInternalType (impact.d3.Object.CO_RIGID_BODY);

      Self.m_linearVelocity       := (0.0, 0.0, 0.0);
      Self.m_angularVelocity      := (0.0, 0.0, 0.0);
      Self.m_angularFactor        := (1.0, 1.0, 1.0);
      Self.m_linearFactor         := (1.0, 1.0, 1.0);
      Self.m_gravity              := (0.0, 0.0, 0.0);
      Self.m_gravity_acceleration := (0.0, 0.0, 0.0);
      Self.m_totalForce           := (0.0, 0.0, 0.0);
      Self.m_totalTorque          := (0.0, 0.0, 0.0);

      setDamping (Self, constructionInfo.m_linearDamping, constructionInfo.m_angularDamping);

      Self.m_linearSleepingThreshold              := constructionInfo.m_linearSleepingThreshold;
      Self.m_angularSleepingThreshold             := constructionInfo.m_angularSleepingThreshold;
      Self.m_optionalMotionState                  := constructionInfo.m_motionState;
      Self.m_contactSolverType                    := 0;
      Self.m_frictionSolverType                   := 0;
      Self.m_additionalDamping                    := constructionInfo.m_additionalDamping;
      Self.m_additionalDampingFactor              := constructionInfo.m_additionalDampingFactor;
      Self.m_additionalLinearDampingThresholdSqr  := constructionInfo.m_additionalLinearDampingThresholdSqr;
      Self.m_additionalAngularDampingThresholdSqr := constructionInfo.m_additionalAngularDampingThresholdSqr;
      Self.m_additionalAngularDampingFactor       := constructionInfo.m_additionalAngularDampingFactor;

      if Self.m_optionalMotionState /= null then
         Self.m_optionalMotionState.getWorldTransform (Self.getWorldTransform.all);
      else
         Self.getWorldTransform.all := constructionInfo.m_startWorldTransform;
      end if;

      Self.setInterpolationWorldTransform (Self.getworldTransform.all);
      Self.setInterpolationLinearVelocity ((0.0, 0.0, 0.0));
      Self.setInterpolationAngularVelocity ((0.0, 0.0, 0.0));

      --  moved to impact.d3.Object
      Self.setFriction (constructionInfo.m_friction);
      Self.setRestitution (constructionInfo.m_restitution);

      Self.setCollisionShape (constructionInfo.m_collisionShape);

      Self.setMassProps (constructionInfo.m_mass, constructionInfo.m_localInertia);
      updateInertiaTensor (Self);

      Self.m_rigidbodyFlags := 0;

      setZero (Self.m_deltaLinearVelocity);
      setZero (Self.m_deltaAngularVelocity);

      Self.m_invMass :=  Self.m_inverseMass * Self.m_linearFactor;

      setZero (Self.m_pushVelocity);
      setZero (Self.m_turnVelocity);
   end setupRigidBody;

--     void        impact.d3.Object.rigid::setupRigidBody(const impact.d3.Object.rigid::impact.d3.Object.rigidConstructionInfo& constructionInfo)
--  {

--          m_internalType=CO_RIGID_BODY;

--          m_linearVelocity.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
--          m_angularVelocity.setValue(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.));
--          m_angularFactor.setValue(1,1,1);
--          m_linearFactor.setValue(1,1,1);
--          m_gravity.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
--          m_gravity_acceleration.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
--          m_totalForce.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
--          m_totalTorque.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0)),
--      setDamping(constructionInfo.m_linearDamping, constructionInfo.m_angularDamping);

--          m_linearSleepingThreshold = constructionInfo.m_linearSleepingThreshold;
--          m_angularSleepingThreshold = constructionInfo.m_angularSleepingThreshold;
--          m_optionalMotionState = constructionInfo.m_motionState;
--          m_contactSolverType = 0;
--          m_frictionSolverType = 0;
--          m_additionalDamping = constructionInfo.m_additionalDamping;
--          m_additionalDampingFactor = constructionInfo.m_additionalDampingFactor;
--          m_additionalLinearDampingThresholdSqr = constructionInfo.m_additionalLinearDampingThresholdSqr;
--          m_additionalAngularDampingThresholdSqr = constructionInfo.m_additionalAngularDampingThresholdSqr;
--          m_additionalAngularDampingFactor = constructionInfo.m_additionalAngularDampingFactor;

--          if (m_optionalMotionState)
--          {
--                  m_optionalMotionState->getWorldTransform(m_worldTransform);
--          } else
--          {
--                  m_worldTransform = constructionInfo.m_startWorldTransform;
--          }

--          m_interpolationWorldTransform = m_worldTransform;
--          m_interpolationLinearVelocity.setValue(0,0,0);
--          m_interpolationAngularVelocity.setValue(0,0,0);

--          //moved to impact.d3.Object
--          m_friction = constructionInfo.m_friction;
--          m_restitution = constructionInfo.m_restitution;

--          setCollisionShape( constructionInfo.m_collisionShape );
--          m_debugBodyId = uniqueId++;

--          setMassProps(constructionInfo.m_mass, constructionInfo.m_localInertia);
--          updateInertiaTensor();

--          m_rigidbodyFlags = 0;


--          m_deltaLinearVelocity.setZero();
--          m_deltaAngularVelocity.setZero();
--          m_invMass = m_inverseMass*m_linearFactor;
--          m_pushVelocity.setZero();
--          m_turnVelocity.setZero();



--  }

   procedure proceedToTransform (Self     : in out Item;
                                 newTrans : in Transform_3d)
   is
   begin
      setCenterOfMassTransform (Self, newTrans);
   end proceedToTransform;


--  void impact.d3.Object.rigid::proceedToTransform(const impact.d3.Transform& newTrans)
--  {
--          setCenterOfMassTransform( newTrans );
--  }

   procedure predictIntegratedTransform (Self               : in out Item;
                                         timeStep           : in     Real;
                                         predictedTransform :    out Transform_3d)
   is
   begin
      impact.d3.transform_Util.integrateTransform (Self.getWorldTransform.all,
                                          Self.m_linearVelocity,
                                          Self.m_angularVelocity,
                                          timeStep,
                                          predictedTransform);
   end predictIntegratedTransform;

--     void impact.d3.Object.rigid::predictIntegratedTransform(impact.d3.Scalar timeStep,impact.d3.Transform& predictedTransform)
--  {
--          impact.d3.TransformUtil::integrateTransform(m_worldTransform,m_linearVelocity,m_angularVelocity,timeStep,predictedTransform);
--  }

   procedure saveKinematicState (Self     : in out Item;
                                 timeStep : in Real)
   is
      use impact.d3.motion_State;
   begin
      if timeStep /= 0.0 then

         if Self.getMotionState /= null then
            Self.getMotionState.getWorldTransform (Self.getWorldTransform.all);
         end if;

         impact.d3.transform_Util.calculateVelocity (Self.getInterpolationWorldTransform,
                                            Self.getWorldTransform.all,
                                            timeStep,
                                            Self.m_linearVelocity,
                                            Self.m_angularVelocity);

         Self.SetInterpolationLinearVelocity  (Self.m_linearVelocity);
         Self.SetInterpolationAngularVelocity (Self.m_angularVelocity);
         Self.SetInterpolationWorldTransform  (Self.getWorldTransform.all);

      end if;
   end saveKinematicState;

--     void                        impact.d3.Object.rigid::saveKinematicState(impact.d3.Scalar timeStep)
--  {
--          //todo: clamp to some (user definable) safe minimum timestep, to limit maximum angular/linear velocities
--          if (timeStep != impact.d3.Scalar(0.))
--          {
--                  //if we use motionstate to synchronize world transforms, get the new kinematic/animated world transform
--                  if (getMotionState())
--                          getMotionState()->getWorldTransform(m_worldTransform);
--                  impact.d3.Vector linVel,angVel;

--                  impact.d3.TransformUtil::calculateVelocity(m_interpolationWorldTransform,m_worldTransform,timeStep,m_linearVelocity,m_angularVelocity);
--                  m_interpolationLinearVelocity = m_linearVelocity;
--                  m_interpolationAngularVelocity = m_angularVelocity;
--                  m_interpolationWorldTransform = m_worldTransform;
--                  //printf("angular = %f %f %f\n",m_angularVelocity.getX(),m_angularVelocity.getY(),m_angularVelocity.getZ());
--          }
--  }

   procedure applyGravity (Self : in out Item)
   is
   begin
      if isStaticOrKinematicObject (Self) then
         null;
      else
         applyCentralForce (Self, Self.m_gravity);
      end if;
   end applyGravity;

--     void impact.d3.Object.rigid::applyGravity()
--  {
--          if (isStaticOrKinematicObject())
--                  return;

--          applyCentralForce(m_gravity);

--  }

   procedure setGravity (Self         : in out Item;
                         acceleration : in Vector_3)
   is
   begin
      if Self.m_inverseMass  /= 0.0 then
         Self.m_gravity := acceleration * (1.0 / Self.m_inverseMass);
      end if;

      Self.m_gravity_acceleration := acceleration;
   end setGravity;

--     void impact.d3.Object.rigid::setGravity(const impact.d3.Vector& acceleration)
--  {
--          if (m_inverseMass != impact.d3.Scalar(0.0))
--          {
--                  m_gravity = acceleration * (impact.d3.Scalar(1.0) / m_inverseMass);
--          }
--          m_gravity_acceleration = acceleration;
--  }


   function getGravity (Self : in     Item) return Vector_3
   is
   begin
      return Self.m_gravity_acceleration;
   end getGravity;

--          const impact.d3.Vector&        getGravity() const
--          {
--                  return m_gravity_acceleration;
--          }

   procedure setDamping (Self        : in out Item;
                         lin_damping : in Real;
                         ang_damping : in Real)
   is
      use impact.d3.min_max;
   begin
      Self.m_linearDamping := btClamped (lin_damping, 0.0, 1.0);
      Self.m_angularDamping := btClamped (ang_damping, 0.0, 1.0);
   end setDamping;

--     void impact.d3.Object.rigid::setDamping(impact.d3.Scalar lin_damping, impact.d3.Scalar ang_damping)
--  {
--          m_linearDamping = btClamped(lin_damping, (impact.d3.Scalar)impact.d3.Scalar(0.0), (impact.d3.Scalar)impact.d3.Scalar(1.0));
--          m_angularDamping = btClamped(ang_damping, (impact.d3.Scalar)impact.d3.Scalar(0.0), (impact.d3.Scalar)impact.d3.Scalar(1.0));
--  }

   function getLinearDamping (Self : in Item) return Real
   is
   begin
      return Self.m_linearDamping;
   end getLinearDamping;

   --          impact.d3.Scalar getLinearDamping() const
   --          {
   --                  return m_linearDamping;
   --          }

   function getAngularDamping (Self : in Item) return Real
   is
   begin
      return Self.m_angularDamping;
   end getAngularDamping;

   --          impact.d3.Scalar getAngularDamping() const
   --          {
   --                  return m_angularDamping;
   --          }


   function getLinearSleepingThreshold (Self : in Item) return Real
   is
   begin
      return Self.m_linearSleepingThreshold;
   end getLinearSleepingThreshold;

   --          impact.d3.Scalar getLinearSleepingThreshold() const
   --          {
   --                  return m_linearSleepingThreshold;
   --          }

   function getAngularSleepingThreshold (Self : in Item) return Real
   is
   begin
      return Self.m_angularSleepingThreshold;
   end getAngularSleepingThreshold;

   --          impact.d3.Scalar getAngularSleepingThreshold() const
   --          {
   --                  return m_angularSleepingThreshold;
   --          }

   procedure applyDamping (Self     : in out Item;
                           timeStep : in Real)
   is
      use math.Functions;
   begin
      Self.m_linearVelocity  := Self.m_linearVelocity  * (1.0 - Self.m_linearDamping)**timeStep;
      Self.m_angularVelocity := Self.m_angularVelocity * (1.0 - Self.m_angularDamping)**timeStep;


      if Self.m_additionalDamping then

         if         length2 (Self.m_angularVelocity) < Self.m_additionalAngularDampingThresholdSqr
           and then length2 (Self.m_linearVelocity) < Self.m_additionalLinearDampingThresholdSqr
         then
            Self.m_angularVelocity := Self.m_angularVelocity * Self.m_additionalDampingFactor;
            Self.m_linearVelocity  := Self.m_linearVelocity  * Self.m_additionalDampingFactor;
         end if;


         declare
            speed   : constant Real   := length (Self.m_linearVelocity);
            dampVel : constant Real   := 0.005;
            dir     : constant Vector_3 := normalized (Self.m_linearVelocity);
         begin
            if speed < Self.m_linearDamping then
               if speed > dampVel then
                  Self.m_linearVelocity := Self.m_linearVelocity - dir * dampVel;
               else
                  Self.m_linearVelocity := (0.0, 0.0, 0.0);
               end if;
            end if;
         end;


         declare
            angSpeed   : constant Real := length (Self.m_angularVelocity);
            angDampVel : constant Real := 0.005;
            dir        : constant Vector_3 := normalized (Self.m_angularVelocity);
         begin
            if angSpeed < Self.m_angularDamping then
               if angSpeed > angDampVel then
                  Self.m_angularVelocity := Self.m_angularVelocity - dir * angDampVel;
               else
                  Self.m_angularVelocity := (0.0, 0.0, 0.0);
               end if;
            end if;
         end;

      end if;

   end applyDamping;

--  ///applyDamping damps the velocity, using the given m_linearDamping and m_angularDamping
--  void                        impact.d3.Object.rigid::applyDamping(impact.d3.Scalar timeStep)
--  {
--          //On new damping: see discussion/issue report here: http://code.google.com/p/bullet/issues/detail?id=74
--          //todo: do some performance comparisons (but other parts of the engine are probably bottleneck anyway

--  //#define USE_OLD_DAMPING_METHOD 1
--  #ifdef USE_OLD_DAMPING_METHOD
--          m_linearVelocity *= GEN_clamped((impact.d3.Scalar(1.) - timeStep * m_linearDamping), (impact.d3.Scalar)impact.d3.Scalar(0.0), (impact.d3.Scalar)impact.d3.Scalar(1.0));
--          m_angularVelocity *= GEN_clamped((impact.d3.Scalar(1.) - timeStep * m_angularDamping), (impact.d3.Scalar)impact.d3.Scalar(0.0), (impact.d3.Scalar)impact.d3.Scalar(1.0));
--  #else
--          m_linearVelocity *= btPow(impact.d3.Scalar(1)-m_linearDamping, timeStep);
--          m_angularVelocity *= btPow(impact.d3.Scalar(1)-m_angularDamping, timeStep);
--  #endif

--          if (m_additionalDamping)
--          {
--                  //Additional damping can help avoiding lowpass jitter motion, help stability for ragdolls etc.
--                  //Such damping is undesirable, so once the overall simulation quality of the rigid body dynamics system has improved, this should become obsolete
--                  if ((m_angularVelocity.length2() < m_additionalAngularDampingThresholdSqr) &&
--                          (m_linearVelocity.length2() < m_additionalLinearDampingThresholdSqr))
--                  {
--                          m_angularVelocity *= m_additionalDampingFactor;
--                          m_linearVelocity *= m_additionalDampingFactor;
--                  }


--                  impact.d3.Scalar speed = m_linearVelocity.length();
--                  if (speed < m_linearDamping)
--                  {
--                          impact.d3.Scalar dampVel = impact.d3.Scalar(0.005);
--                          if (speed > dampVel)
--                          {
--                                  impact.d3.Vector dir = m_linearVelocity.normalized();
--                                  m_linearVelocity -=  dir * dampVel;
--                          } else
--                          {
--                                  m_linearVelocity.setValue(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.));
--                          }
--                  }

--                  impact.d3.Scalar angSpeed = m_angularVelocity.length();
--                  if (angSpeed < m_angularDamping)
--                  {
--                          impact.d3.Scalar angDampVel = impact.d3.Scalar(0.005);
--                          if (angSpeed > angDampVel)
--                          {
--                                  impact.d3.Vector dir = m_angularVelocity.normalized();
--                                  m_angularVelocity -=  dir * angDampVel;
--                          } else
--                          {
--                                  m_angularVelocity.setValue(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.));
--                          }
--                  }
--          }
--  }







--     function getCollisionShape (Self : in Item) return access impact.d3.Shape.item'Class
--     is
--     begin
--        return Self.m_collisionShape;
--     end getCollisionShape;


--          SIMD_FORCE_INLINE const impact.d3.Shape*        getCollisionShape() const {
--                  return m_collisionShape;
--          }
--
--          SIMD_FORCE_INLINE impact.d3.Shape*        getCollisionShape() {
--                          return m_collisionShape;
--          }





   procedure setMassProps (Self    : in out Item;
                           mass    : in     Real;
                           inertia : in     Vector_3)
   is
   begin
      if mass = 0.0 then
         Self.setCollisionFlags (Self.getCollisionFlags or impact.d3.Object.CF_STATIC_OBJECT);
         Self.m_inverseMass := 0.0;
      else
         Self.setCollisionFlags (Self.getCollisionFlags and (not impact.d3.Object.CF_STATIC_OBJECT));
         Self.m_inverseMass := 1.0 / mass;
      end if;

      Self.m_gravity := mass * Self.m_gravity_acceleration;

      declare
         A, B, C : Real;
      begin
         if inertia (1) /= 0.0 then
            A := 1.0 / inertia (1);
         else
            A := 0.0;
         end if;

         if inertia (2) /= 0.0 then
            B := 1.0 / inertia (2);
         else
            B := 0.0;
         end if;

         if inertia (3) /= 0.0 then
            C := 1.0 / inertia (3);
         else
            C := 0.0;
         end if;

         Self.m_invInertiaLocal := (A, B, C);
         Self.m_invMass         := Self.m_linearFactor * Self.m_inverseMass;
      end;

      Self.m_invMass :=  Self.m_linearFactor * Self.m_inverseMass;
   end setMassProps;

--     void impact.d3.Object.rigid::setMassProps(impact.d3.Scalar mass, const impact.d3.Vector& inertia)
--  {
--          if (mass == impact.d3.Scalar(0.))
--          {
--                  m_collisionFlags |= impact.d3.Object::CF_STATIC_OBJECT;
--                  m_inverseMass = impact.d3.Scalar(0.);
--          } else
--          {
--                  m_collisionFlags &= (~impact.d3.Object::CF_STATIC_OBJECT);
--                  m_inverseMass = impact.d3.Scalar(1.0) / mass;
--          }

--          //Fg = m * a
--          m_gravity = mass * m_gravity_acceleration;

--          m_invInertiaLocal.setValue(inertia.x() != impact.d3.Scalar(0.0) ? impact.d3.Scalar(1.0) / inertia.x(): impact.d3.Scalar(0.0),
--                                     inertia.y() != impact.d3.Scalar(0.0) ? impact.d3.Scalar(1.0) / inertia.y(): impact.d3.Scalar(0.0),
--                                     inertia.z() != impact.d3.Scalar(0.0) ? impact.d3.Scalar(1.0) / inertia.z(): impact.d3.Scalar(0.0));

--          m_invMass = m_linearFactor*m_inverseMass;
--  }

   function getLinearFactor (Self : in Item) return Vector_3
   is
   begin
      return Self.m_linearFactor;
   end getLinearFactor;

   --          const impact.d3.Vector& getLinearFactor() const
   --          {
   --                  return m_linearFactor;
   --          }

   procedure setLinearFactor (Self         : in out Item;
                              linearFactor : in Vector_3)
   is
   begin
      Self.m_linearFactor := linearFactor;
      Self.m_invMass :=  Self.m_linearFactor * Self.m_inverseMass;
   end setLinearFactor;

   --          void setLinearFactor(const impact.d3.Vector& linearFactor)
   --          {
   --                  m_linearFactor = linearFactor;
   --                  m_invMass = m_linearFactor*m_inverseMass;
   --          }

   function getInvMass (Self : in Item) return Real
   is
   begin
      return Self.m_inverseMass;
   end getInvMass;

   --          impact.d3.Scalar                getInvMass() const { return m_inverseMass; }

   function getInvInertiaTensorWorld (Self : in Item) return Matrix_3x3
   is
   begin
      return Self.m_invInertiaTensorWorld;
   end getInvInertiaTensorWorld;

   --          const impact.d3.Matrix& getInvInertiaTensorWorld() const {
   --                  return m_invInertiaTensorWorld;
   --          }





   procedure integrateVelocities (Self : in out Item;
                                  step : in Real)
   is
      MAX_ANGVEL : constant math.Real := math.Pi / 2.0;
      angvel     :          math.Real;
   begin
      if isStaticOrKinematicObject (Self) then
         return;
      end if;


      Self.m_linearVelocity  := Self.m_linearVelocity   +  Self.m_totalForce            * (Self.m_inverseMass * step);
      Self.m_angularVelocity := Self.m_angularVelocity  +  Self.m_invInertiaTensorWorld * (Self.m_totalTorque * step);

      --  Clamp angular velocity. collision calculations will fail on higher angular velocities.
      --
      angvel := Length (Self.m_angularVelocity);

      if angvel * step  >  MAX_ANGVEL then
         Self.m_angularVelocity := Self.m_angularVelocity  *  (MAX_ANGVEL / step) / angvel;
      end if;
   end integrateVelocities;







   procedure setCenterOfMassTransform (Self  : in out Item;
                                       xform : Transform_3d)
   is
   begin
      if isStaticOrKinematicObject (Self) then
         Self.setInterpolationWorldTransform (Self.getWorldTransform.all);
      else
         Self.setInterpolationWorldTransform (xform);
      end if;

      Self.setInterpolationLinearVelocity  (Self.getLinearVelocity);
      Self.setInterpolationAngularVelocity (Self.getAngularVelocity);
      Self.getWorldTransform.all := xform;

      updateInertiaTensor (Self);
   end setCenterOfMassTransform;

--     void impact.d3.Object.rigid::setCenterOfMassTransform(const impact.d3.Transform& xform)
--  {

--          if (isStaticOrKinematicObject())
--          {
--                  m_interpolationWorldTransform = m_worldTransform;
--          } else
--          {
--                  m_interpolationWorldTransform = xform;
--          }
--          m_interpolationLinearVelocity = getLinearVelocity();
--          m_interpolationAngularVelocity = getAngularVelocity();
--          m_worldTransform = xform;
--          updateInertiaTensor();
--  }

   procedure applyCentralForce (Self  : in out Item;
                                force : in Vector_3)
   is
   begin
      Self.m_totalForce := Self.m_totalForce  +  Scaled (force, by => Self.m_linearFactor);
   end applyCentralForce;

   --          void                        applyCentralForce(const impact.d3.Vector& force)
   --          {
   --                  m_totalForce += force*m_linearFactor;
   --          }

   function getTotalForce (Self : in Item) return Vector_3
   is
   begin
      return Self.m_totalForce;
   end getTotalForce;

   --          const impact.d3.Vector& getTotalForce() const
   --          {
   --                  return m_totalForce;
   --          };

   function getTotalTorque (Self : in Item) return Vector_3
   is
   begin
      return Self.m_totalTorque;
   end getTotalTorque;

   --          const impact.d3.Vector& getTotalTorque() const
   --          {
   --                  return m_totalTorque;
   --          };


   function getInvInertiaDiagLocal (Self : in Item) return Vector_3
   is
   begin
      return Self.m_invInertiaLocal;
   end getInvInertiaDiagLocal;

   --          const impact.d3.Vector& getInvInertiaDiagLocal() const
   --          {
   --                  return m_invInertiaLocal;
   --          };

   procedure setInvInertiaDiagLocal (Self           : in out Item;
                                     diagInvInertia : in Vector_3)
   is
   begin
      Self.m_invInertiaLocal := diagInvInertia;
   end setInvInertiaDiagLocal;

   --          void        setInvInertiaDiagLocal(const impact.d3.Vector& diagInvInertia)
   --          {
   --                  m_invInertiaLocal = diagInvInertia;
   --          }

   procedure setSleepingThresholds (Self    : in out Item;
                                    linear  : in     Real;
                                    angular : in     Real)
   is
   begin
      Self.m_linearSleepingThreshold  := linear;
      Self.m_angularSleepingThreshold := angular;
   end setSleepingThresholds;

   --          void        setSleepingThresholds(impact.d3.Scalar linear,impact.d3.Scalar angular)
   --          {
   --                  m_linearSleepingThreshold = linear;
   --                  m_angularSleepingThreshold = angular;
   --          }





   procedure applyTorque (Self   : in out Item;
                          torque : in Vector_3)
   is
   begin
      Self.m_totalTorque := Scaled (torque, by => Self.m_angularFactor);
   end applyTorque;

   --          void        applyTorque(const impact.d3.Vector& torque)
   --          {
   --                  m_totalTorque += torque*m_angularFactor;
   --          }




   procedure applyForce (Self    : in out Item;
                         force   : in Vector_3;
                         rel_pos : in Vector_3)
   is
   begin
      applyCentralForce (Self, force);
      Self.applyTorque (cross (rel_pos, Scaled (force, by => Self.m_linearFactor)));
   end applyForce;

   --          void        applyForce(const impact.d3.Vector& force, const impact.d3.Vector& rel_pos)
   --          {
   --                  applyCentralForce(force);
   --                  applyTorque(rel_pos.cross(force*m_linearFactor));
   --          }




   procedure applyCentralImpulse (Self    : in out Item;
                                  impulse : in Vector_3)
   is
   begin
      Self.m_linearVelocity := Self.m_linearVelocity  +  Scaled (impulse, by =>  Self.m_linearFactor * Self.m_inverseMass);
   end applyCentralImpulse;

   --          void applyCentralImpulse(const impact.d3.Vector& impulse)
   --          {
   --                  m_linearVelocity += impulse *m_linearFactor * m_inverseMass;
   --          }





   procedure applyTorqueImpulse (Self   : in out Item;
                                 torque : in Vector_3)
   is
   begin
      Self.m_angularVelocity := Self.m_angularVelocity  +  Self.m_invInertiaTensorWorld * Scaled (torque, by => Self.m_angularFactor);
   end applyTorqueImpulse;

   --            void applyTorqueImpulse(const impact.d3.Vector& torque)
   --          {
   --                          m_angularVelocity += m_invInertiaTensorWorld * torque * m_angularFactor;
   --          }





   procedure applyImpulse (Self    : in out Item;
                           impulse : in Vector_3;
                           rel_pos : in Vector_3)
   is
      use Math.Vectors;
   begin
      if Self.m_inverseMass /= 0.0 then
         applyCentralImpulse (Self, impulse);

         if Self.m_angularFactor /= math.Origin_3d then   -- tbd: ok ?
            Self.applyTorqueImpulse (cross (rel_pos, Scaled (impulse, by => Self.m_linearFactor)));
         end if;

      end if;
   end applyImpulse;

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






   procedure clearForces (Self : in out Item)
   is
   begin
      Self.m_totalForce  := (0.0, 0.0, 0.0);
      Self.m_totalTorque := (0.0, 0.0, 0.0);
   end clearForces;

   --          void clearForces()
   --          {
   --                  m_totalForce.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
   --                  m_totalTorque.setValue(impact.d3.Scalar(0.0), impact.d3.Scalar(0.0), impact.d3.Scalar(0.0));
   --          }





   procedure updateInertiaTensor (Self : in out Item)
   is
      use impact.d3.Matrix, math.Vectors;
   begin
      Self.m_invInertiaTensorWorld := scaled (Self.getWorldTransform.Rotation, Self.m_invInertiaLocal)  * Transpose (Self.GetWorldTransform.Rotation);

--        for row in 1..3 loop
--           for col in 1..3 loop
--              if not Self.m_invInertiaTensorWorld (row, col)'Valid then
--                 raise Constraint_Error;
--              end if;
--           end loop;
--        end loop;

   end updateInertiaTensor;

--  void impact.d3.Object.rigid::updateInertiaTensor()
--  {
--          m_invInertiaTensorWorld = m_worldTransform.getBasis().scaled(m_invInertiaLocal) * m_worldTransform.getBasis().transpose();
--  }






   function getCenterOfMassPosition (Self : in Item) return Vector_3
   is
   begin
      return Self.WorldTransform.Translation;
   end getCenterOfMassPosition;

   --          const impact.d3.Vector&     getCenterOfMassPosition() const {
   --                  return m_worldTransform.getOrigin();
   --          }





   function getOrientation (Self : access Item) return Quaternion
   is
      use impact.d3.Matrix;
      orn : Quaternion;
   begin
      getRotation (Self.getWorldTransform.Rotation, orn);
      return orn;
   end getOrientation;

--  impact.d3.Quaternion impact.d3.Object.rigid::getOrientation() const
--  {
--                  impact.d3.Quaternion orn;
--                  m_worldTransform.getBasis().getRotation(orn);
--                  return orn;
--  }





   function getCenterOfMassTransform (Self : in Item) return Transform_3d
   is
   begin
      return Self.WorldTransform;
   end getCenterOfMassTransform;

   --          const impact.d3.Transform&  getCenterOfMassTransform() const {
   --                  return m_worldTransform;
   --          }




   function getLinearVelocity (Self : in Item) return Vector_3
   is
   begin
      return Self.m_linearVelocity;
   end getLinearVelocity;

   --          const impact.d3.Vector&   getLinearVelocity() const {
   --                  return m_linearVelocity;
   --          }

   function getAngularVelocity (Self : in Item) return Vector_3
   is
   begin
      return Self.m_angularVelocity;
   end getAngularVelocity;

   --          const impact.d3.Vector&    getAngularVelocity() const {
   --                  return m_angularVelocity;
   --          }

   procedure setLinearVelocity (Self    : in out Item;
                                lin_vel : in Vector_3)
   is
   begin
      Self.m_linearVelocity := lin_vel;
   end setLinearVelocity;

   --          inline void setLinearVelocity(const impact.d3.Vector& lin_vel)
   --          {
   --                  m_linearVelocity = lin_vel;
   --          }

   procedure setAngularVelocity (Self    : in out Item;
                                 ang_vel : in Vector_3)
   is
   begin
      Self.m_angularVelocity := ang_vel;
   end setAngularVelocity;

   --          inline void setAngularVelocity(const impact.d3.Vector& ang_vel)
   --          {
   --                  m_angularVelocity = ang_vel;
   --          }




   function getVelocityInLocalPoint (Self    : in Item;
                                     rel_pos : in Vector_3) return Vector_3
   is
   begin
      return Self.m_linearVelocity + cross (Self.m_angularVelocity, rel_pos);
   end getVelocityInLocalPoint;

   --          impact.d3.Vector getVelocityInLocalPoint(const impact.d3.Vector& rel_pos) const
   --          {
   --                  //we also calculate lin/ang velocity for kinematic objects
   --                  return m_linearVelocity + m_angularVelocity.cross(rel_pos);
   --
   --                  //for kinematic objects, we could also use use:
   --                  //                return         (m_worldTransform(rel_pos) - m_interpolationWorldTransform(rel_pos)) / m_kinematicTimeStep;
   --          }




   procedure translate (Self : in out Item;
                        v    : in Vector_3)
   is
   begin
      Self.getWorldTransform.Translation := Self.getWorldTransform.Translation + v;
   end translate;

   --          void translate(const impact.d3.Vector& v)
   --          {
   --                  m_worldTransform.getOrigin() += v;
   --          }





   procedure getAabb (Self    : in out Item;
                      aabbMin :    out     Vector_3;
                      aabbMax :    out     Vector_3)
   is
   begin
      Self.getCollisionShape.getAabb (Self.getWorldTransform.all,  aabbMin, aabbMax);
   end getAabb;






   function computeImpulseDenominator (Self   : in Item;
                                       pos    : in Vector_3;
                                       normal : in Vector_3) return Real
   is
      r0  : constant Vector_3 := pos - getCenterOfMassPosition (Self);
      c0  : constant Vector_3 := cross (r0, normal);
      vec : constant Vector_3 := cross (c0 * getInvInertiaTensorWorld (Self),  r0);
   begin
      return Self.m_inverseMass + dot (normal, vec);
   end computeImpulseDenominator;

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





   function computeAngularImpulseDenominator (Self : in Item;
                                              axis : in Vector_3) return Real
   is
      vec : constant Vector_3 := axis * getInvInertiaTensorWorld (Self);
   begin
      return dot (axis, vec);
   end computeAngularImpulseDenominator;

   --          SIMD_FORCE_INLINE impact.d3.Scalar computeAngularImpulseDenominator(const impact.d3.Vector& axis) const
   --          {
   --                  impact.d3.Vector vec = axis * getInvInertiaTensorWorld();
   --                  return axis.dot(vec);
   --          }





   procedure UpdateDeactivation (Self     : in out Item;
                                 TimeStep : in     Real)
   is
   begin

      if        Self.getActivationState = impact.d3.Object.ISLAND_SLEEPING
        or else Self.getActivationState = impact.d3.Object.DISABLE_DEACTIVATION
      then
         null;

      elsif      length2 (Self.getLinearVelocity)  <  Self.m_linearSleepingThreshold  * Self.m_linearSleepingThreshold
        and then length2 (Self.getAngularVelocity)  <  Self.m_angularSleepingThreshold * Self.m_angularSleepingThreshold
      then
         Self.setDeactivationTime (Self.getDeactivationTime + timeStep);

      else
         Self.setDeactivationTime (0.0);
         Self.setActivationState  (0);
      end if;

   end UpdateDeactivation;

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




   function wantsSleeping (Self : in Item) return Boolean
   is
   begin
      if getActivationState (Self) = impact.d3.Object.DISABLE_DEACTIVATION then
         return False;
      end if;

      if        gDisableDeactivation
        or else gDeactivationTime = 0.0
      then
         return False;
      end if;

      if        getActivationState (Self) = impact.d3.Object.ISLAND_SLEEPING
        or else getActivationState (Self) = impact.d3.Object.WANTS_DEACTIVATION
      then
         return True;
      end if;

      if Self.getDeactivationTime > gDeactivationTime then
         return True;
      end if;


      return False;
   end wantsSleeping;

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

   function getBroadphaseProxy (Self : access Item) return access impact.d3.collision.Proxy.item
   is
   begin
      return Self.getBroadphaseHandle;
   end getBroadphaseProxy;

   --         impact.d3.collision.Proxy*        getBroadphaseProxy()
   --          {
   --                  return m_broadphaseHandle;
   --          }

   procedure setNewBroadphaseProxy (Self            : in out Item;
                                    broadphaseProxy : access     impact.d3.collision.Proxy.item)
   is
   begin
      Self.setBroadphaseHandle (broadphaseProxy);
   end setNewBroadphaseProxy;

   --          void        setNewBroadphaseProxy(impact.d3.collision.Proxy* broadphaseProxy)
   --          {
   --                  m_broadphaseHandle = broadphaseProxy;
   --          }


--     function getMotionState (Self : in Item) return impact.d3.motion_State.Item'Class is
--     begin
--        return Self.m_OptionalMotionState.all;
--     end getMotionState;


   function getMotionState (Self : in Item) return access impact.d3.motion_State.Item'Class is
   begin
      return Self.m_OptionalMotionState;
   end getMotionState;

   --          impact.d3.motion_State*        getMotionState()
   --          {
   --                  return m_optionalMotionState;
   --          }





   procedure setMotionState (Self        : in out Item;
                             motionState : access impact.d3.motion_State.Item'Class) is
   begin
      Self.m_optionalMotionState := motionState;

      if Self.m_optionalMotionState /= null then
         motionState.getWorldTransform (Self.getWorldTransform.all);
      end if;
   end setMotionState;

   --          void        setMotionState(impact.d3.motion_State* motionState)
   --          {
   --                  m_optionalMotionState = motionState;
   --                  if (m_optionalMotionState)
   --                          motionState->getWorldTransform(m_worldTransform);
   --          }




   procedure setAngularFactor (Self   : in out Item;
                               angFac : in     Vector_3)
   is
   begin
      Self.m_angularFactor := angFac;
   end setAngularFactor;

   --          void        setAngularFactor(const impact.d3.Vector& angFac)
   --          {
   --                  m_angularFactor = angFac;
   --          }

   procedure setAngularFactor (Self   : in out Item;
                               angFac : in     Real)
   is
   begin
      Self.m_angularFactor := (angFac, angFac, angFac);
   end setAngularFactor;

   --          void        setAngularFactor(impact.d3.Scalar angFac)
   --          {
   --                  m_angularFactor.setValue(angFac,angFac,angFac);
   --          }

   function getAngularFactor (Self : in Item) return Vector_3
   is
   begin
      return Self.m_angularFactor;
   end getAngularFactor;

   --          const impact.d3.Vector&        getAngularFactor() const
   --          {
   --                  return m_angularFactor;
   --          }

   function isInWorld (Self : access Item) return Boolean
   is
      use impact.d3.collision.Proxy;
   begin
      return Self.getBroadphaseProxy /= null;
   end isInWorld;

   --          //is this rigidbody added to a impact.d3.Space/impact.d3.Space.dynamic/btBroadphase?
   --          bool        isInWorld() const
   --          {
   --                  return (getBroadphaseProxy() != 0);
   --          }





   overriding function checkCollideWithOverride (Self : in Item;
                                      co   : access impact.d3.Object.item'Class) return Boolean
   is
      type rigid_Object_view is access all impact.d3.Object.rigid.item'Class;

      otherRb : constant access impact.d3.Object.rigid      .item'Class := impact.d3.Object.rigid.view (co);
      c       : access impact.d3.Joint.Item'Class;
   begin
      if otherRb = null then
         return True;
      end if;


      for i in 1 .. Integer (Self.m_constraintRefs.Length)
      loop
         c := Self.m_constraintRefs.Element (i);

         if        c.getRigidBodyA = otherRb
           or else c.getRigidBodyB = otherRb
         then
            return False;
         end if;
      end loop;


      return True;
   end checkCollideWithOverride;



--     bool impact.d3.Object.rigid::checkCollideWithOverride(impact.d3.Object* co)
--  {
--          impact.d3.Object.rigid* otherRb = impact.d3.Object.rigid::upcast(co);
--          if (!otherRb)
--                  return true;

--          for (int i = 0; i < m_constraintRefs.size(); ++i)
--          {
--                  impact.d3.Joint* c = m_constraintRefs[i];
--                  if (&c->getRigidBodyA() == otherRb || &c->getRigidBodyB() == otherRb)
--                          return false;
--          }

--          return true;
--  }






   procedure addConstraintRef (Self : in out Item;
                               c    : access impact.d3.Joint.Item'Class)
   is
      use impact.d3.Joint.Vectors;
      index : constant Integer := Self.m_constraintRefs.find_Index (c.all'Access);
   begin
      if index = 0 then -- Integer (Self.m_constraintRefs.Length) then
         Self.m_constraintRefs.append (c.all'Access);
      end if;

      Self.setCheckCollideWith (True);
   end addConstraintRef;

--     void impact.d3.Object.rigid::addConstraintRef(impact.d3.Joint* c)
--  {
--          int index = m_constraintRefs.findLinearSearch(c);
--          if (index == m_constraintRefs.size())
--                  m_constraintRefs.push_back(c);

--          m_checkCollideWith = true;
--  }





   procedure removeConstraintRef (Self : in out Item;
                                  c    : access impact.d3.Joint.item'Class)
   is
      use type ada.Containers.Count_Type;
   begin
      Self.m_constraintRefs.delete (Self.m_constraintRefs.find_Index (c.all'Access));
      Self.setCheckCollideWith     (Self.m_constraintRefs.Length > 0);
   end removeConstraintRef;





   function getConstraintRef (Self  : in Item;
                              index : in Integer) return access impact.d3.Joint.Item'Class
   is
   begin
      return Self.m_constraintRefs.Element (index);
   end getConstraintRef;

      --          impact.d3.Joint* getConstraintRef(int index)
   --          {
   --                  return m_constraintRefs[index];
   --          }

   function getNumConstraintRefs (Self : in Item) return Ada.Containers.Count_Type
   is
   begin
      return Self.m_constraintRefs.Length;
   end getNumConstraintRefs;

   --          int getNumConstraintRefs() const
   --          {
   --                  return m_constraintRefs.size();
   --          }

   procedure setFlags (Self  : in out Item;
                       flags : in     d3.Flags)
   is
   begin
      Self.m_rigidbodyFlags := flags;
   end setFlags;

   --          void        setFlags(int flags)
   --          {
   --                  m_rigidbodyFlags = flags;
   --          }

   function getFlags (Self : in Item) return Flags
   is
   begin
      return Self.m_rigidbodyFlags;
   end getFlags;

   --          int getFlags() const
   --          {
   --                  return m_rigidbodyFlags;
   --          }

   function getDeltaLinearVelocity (Self : in Item) return Vector_3
   is
   begin
      return Self.m_deltaLinearVelocity;
   end getDeltaLinearVelocity;

   --          const impact.d3.Vector& getDeltaLinearVelocity() const
   --          {
   --                  return m_deltaLinearVelocity;
   --          }

   function getDeltaAngularVelocity (Self : in Item) return Vector_3
   is
   begin
      return Self.m_deltaAngularVelocity;
   end getDeltaAngularVelocity;

   --          const impact.d3.Vector& getDeltaAngularVelocity() const
   --          {
   --                  return m_deltaAngularVelocity;
   --          }

   function getPushVelocity (Self : in Item) return Vector_3
   is
   begin
      return Self.m_pushVelocity;
   end getPushVelocity;

   --          const impact.d3.Vector& getPushVelocity() const
   --          {
   --                  return m_pushVelocity;
   --          }

   function getTurnVelocity (Self : in Item) return Vector_3
   is
   begin
      return Self.m_turnVelocity;
   end getTurnVelocity;

   --          const impact.d3.Vector& getTurnVelocity() const
   --          {
   --                  return m_turnVelocity;
   --          }

   function internalGetDeltaLinearVelocity (Self : access Item) return access Vector_3
   is
   begin
      return Self.m_deltaLinearVelocity'Access;
   end internalGetDeltaLinearVelocity;

   --          impact.d3.Vector& internalGetDeltaLinearVelocity()
   --          {
   --                  return m_deltaLinearVelocity;
   --          }

   function internalGetDeltaAngularVelocity (Self : access Item) return access Vector_3
   is
   begin
      return Self.m_deltaAngularVelocity'Access;
   end internalGetDeltaAngularVelocity;

   --          impact.d3.Vector& internalGetDeltaAngularVelocity()
   --          {
   --                  return m_deltaAngularVelocity;
   --          }

   function internalGetAngularFactor (Self : in Item) return Vector_3
   is
   begin
      return Self.m_angularFactor;
   end internalGetAngularFactor;

   --          const impact.d3.Vector& internalGetAngularFactor() const
   --          {
   --                  return m_angularFactor;
   --          }

   function internalGetInvMass (Self : in Item) return Vector_3
   is
   begin
      return Self.m_invMass;
   end internalGetInvMass;

   --          const impact.d3.Vector& internalGetInvMass() const
   --          {
   --                  return m_invMass;
   --          }

   function internalGetPushVelocity (Self : access Item) return access Vector_3
   is
   begin
      return Self.m_pushVelocity'Access;
   end internalGetPushVelocity;

   --          impact.d3.Vector& internalGetPushVelocity()
   --          {
   --                  return m_pushVelocity;
   --          }

   function internalGetTurnVelocity (Self : access Item) return access Vector_3
   is
   begin
      return Self.m_turnVelocity'Access;
   end internalGetTurnVelocity;

   --          impact.d3.Vector& internalGetTurnVelocity()
   --          {
   --                  return m_turnVelocity;
   --          }

   procedure internalGetVelocityInLocalPointObsolete
     (Self     : in Item;
      rel_pos  : in Vector_3;
      velocity :    out Vector_3)
   is
   begin
      velocity :=   getLinearVelocity (Self)
                  + Self.m_deltaLinearVelocity
                  + cross (getAngularVelocity (Self) + Self.m_deltaAngularVelocity,
                           rel_pos);
   end InternalGetVelocityInLocalPointObsolete;

   --          SIMD_FORCE_INLINE void        internalGetVelocityInLocalPointObsolete(const impact.d3.Vector& rel_pos, impact.d3.Vector& velocity ) const
   --          {
   --                  velocity = getLinearVelocity()+m_deltaLinearVelocity + (getAngularVelocity()+m_deltaAngularVelocity).cross(rel_pos);
   --          }





   procedure internalGetAngularVelocity (Self   : in     Item;
                                         angVel :    out Vector_3)
   is
   begin
      angVel := getAngularVelocity (Self) + Self.m_deltaAngularVelocity;
   end internalGetAngularVelocity;

   --          SIMD_FORCE_INLINE void        internalGetAngularVelocity(impact.d3.Vector& angVel) const
   --          {
   --                  angVel = getAngularVelocity()+m_deltaAngularVelocity;
   --          }





   procedure internalApplyImpulse
     (Self             : in out Item;
      linearComponent  : in Vector_3;
      angularComponent : in Vector_3;
      impulseMagnitude : in Real)
   is

      --- These vector functions are for performance ('ada.numerics.generic_real_arrays' appears slow).
      --

      function "+" (L, R : in Vector_3) return Vector_3
      is
      begin
         return (L (1) + R (1),
                 L (2) + R (2),
                 L (3) + R (3));
      end;

      function "*" (L : in Vector_3;   R : in Real) return Vector_3
      is
      begin
         return (L (1) * R,
                 L (2) * R,
                 L (3) * R);
      end;

      pragma Inline_Always ("+");
      pragma Inline_Always ("*");


   begin
      if Self.m_inverseMass /= 0.0 then

         Self.m_deltaLinearVelocity :=
           Self.m_deltaLinearVelocity + linearComponent * impulseMagnitude;

--           put_Line ("Self.m_deltaLinearVelocity => " & Image (Self.m_deltaLinearVelocity, 9));

         Self.m_deltaAngularVelocity :=
           Self.m_deltaAngularVelocity + Scaled (angularComponent, by => (Self.m_angularFactor * impulseMagnitude));

      end if;
   end InternalApplyImpulse;

   --          //Optimization for the iterative solver: avoid calculating constant terms involving inertia, normal, relative position

   --          SIMD_FORCE_INLINE void internalApplyImpulse(const impact.d3.Vector& linearComponent, const impact.d3.Vector& angularComponent,const impact.d3.Scalar impulseMagnitude)
   --          {
   --                  if (m_inverseMass)
   --                  {
   --                          m_deltaLinearVelocity += linearComponent*impulseMagnitude;
   --                          m_deltaAngularVelocity += angularComponent*(impulseMagnitude*m_angularFactor);
   --                  }
   --          }





   procedure internalApplyPushImpulse
     (Self             : in out Item;
      linearComponent  : in Vector_3;
      angularComponent : in Vector_3;
      impulseMagnitude : in Real)
   is
   begin
      if Self.m_inverseMass /= 0.0 then

         Self.m_pushVelocity :=
           Self.m_pushVelocity + linearComponent * impulseMagnitude;

         Self.m_turnVelocity :=
           Self.m_turnVelocity + Scaled (angularComponent, by => (impulseMagnitude * Self.m_angularFactor));

      end if;
   end InternalApplyPushImpulse;

   --          SIMD_FORCE_INLINE void internalApplyPushImpulse(const impact.d3.Vector& linearComponent, const impact.d3.Vector& angularComponent,impact.d3.Scalar impulseMagnitude)
   --          {
   --                  if (m_inverseMass)
   --                  {
   --                          m_pushVelocity += linearComponent*impulseMagnitude;
   --                          m_turnVelocity += angularComponent*(impulseMagnitude*m_angularFactor);
   --                  }
   --          }






   procedure internalWritebackVelocity (Self : in out Item)
   is
   begin
      if Self.m_inverseMass /= 0.0 then
         setLinearVelocity  (Self, getLinearVelocity  (Self) + Self.m_deltaLinearVelocity);
         setAngularVelocity (Self, getAngularVelocity (Self) + Self.m_deltaAngularVelocity);
      end if;
   end internalWritebackVelocity;

   --          void        internalWritebackVelocity()
   --          {
   --                  if (m_inverseMass)
   --                  {
   --                          setLinearVelocity(getLinearVelocity()+ m_deltaLinearVelocity);
   --                          setAngularVelocity(getAngularVelocity()+m_deltaAngularVelocity);
   --                          //m_deltaLinearVelocity.setZero();
   --                          //m_deltaAngularVelocity .setZero();
   --                          //TranslationalBody->setCompanionId(-1);
   --                  }
   --          }





   procedure internalWritebackVelocity (Self     : in out Item;
                                        timeStep : in Real)
   is
      use impact.d3.transform_Util;
   begin
      if Self.m_inverseMass /= 0.0 then
         setLinearVelocity  (Self, getLinearVelocity  (Self) + Self.m_deltaLinearVelocity);
         setAngularVelocity (Self, getAngularVelocity (Self) + Self.m_deltaAngularVelocity);

         declare
            newTransform : Transform_3d;
         begin
            integrateTransform (Self.getWorldTransform.all,
                                Self.m_pushVelocity,
                                Self.m_turnVelocity,
                                timeStep,
                                newTransform);

            setWorldTransform (Self, newTransform);
         end;
      end if;
   end internalWritebackVelocity;

--     void        impact.d3.Object.rigid::internalWritebackVelocity(impact.d3.Scalar timeStep)
--  {
--      (void) timeStep;
--          if (m_inverseMass)
--          {
--                  setLinearVelocity(getLinearVelocity()+ m_deltaLinearVelocity);
--                  setAngularVelocity(getAngularVelocity()+m_deltaAngularVelocity);

--                  //correct the position/orientation based on push/turn recovery
--                  impact.d3.Transform newTransform;
--                  impact.d3.TransformUtil::integrateTransform(getWorldTransform(),m_pushVelocity,m_turnVelocity,timeStep,newTransform);
--                  setWorldTransform(newTransform);
--                  //TranslationalBody->setCompanionId(-1);
--          }
--  //        m_deltaLinearVelocity.setZero();
--  //        m_deltaAngularVelocity .setZero();
--  //        m_pushVelocity.setZero();
--  //        m_turnVelocity.setZero();
--  }

end impact.d3.Object.rigid;
