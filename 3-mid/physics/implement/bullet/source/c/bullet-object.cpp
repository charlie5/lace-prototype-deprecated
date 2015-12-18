#include "bullet-object.h"
#include "btBulletDynamicsCommon.h"



///////////////
/// C++ Support
//

class KinematicMotionState : public btMotionState 
{
public:
              KinematicMotionState (const btTransform &initialpos)       { mPos1 = initialpos; }
    virtual ~ KinematicMotionState ()                                    { }

    virtual void getWorldTransform (      btTransform &worldTrans) const { worldTrans = mPos1; }
            void setKinematicPos   (      btTransform &currentPos)       { mPos1 = currentPos; }
    virtual void setWorldTransform (const btTransform &worldTrans)       { }

protected:
    btTransform   mPos1;
};




///////////
/// Utility
//

btRigidBody*
to_bullet (Object*   From)
{
  return (btRigidBody*) From;
}



Object*
to_bt3 (btRigidBody*   From)
{
  return (Object*) From;
}




///////////////
/// C Interface
//

extern "C"
{
  

int
is_Kinematic (btRigidBody*   Self)
{
  return   Self->getCollisionFlags()
         & btCollisionObject::CF_KINEMATIC_OBJECT;  
}




struct Object*
b3d_new_Object (Real     Mass,
                Shape*   the_Shape,
                int      is_Kinematic)
{
  btCollisionShape*   bt_Shape   = (btCollisionShape*) (the_Shape);
  btScalar            mass       = Mass;
  bool                isDynamic  = (mass != 0.f);
  btVector3           localInertia (0,0,0);
  btTransform         groundTransform;

  groundTransform.setIdentity();

  if (isDynamic)
    bt_Shape->calculateLocalInertia (mass, localInertia);


  KinematicMotionState*                      myMotionState = new KinematicMotionState (groundTransform);
  btRigidBody::btRigidBodyConstructionInfo   rbInfo              (mass, myMotionState, bt_Shape, localInertia);
  btRigidBody*                               body          = new btRigidBody (rbInfo);

    
  if (is_Kinematic)
    {
      body->setCollisionFlags (  body->getCollisionFlags()
			       | btCollisionObject::CF_KINEMATIC_OBJECT);

      body->setActivationState (DISABLE_DEACTIVATION);    
    }
  
  if (isDynamic)
    body->setActivationState (DISABLE_DEACTIVATION);    

  return (Object*) body;
}




Shape*
b3d_Object_Shape          (Object*   Self)
{
  btRigidBody*   the_Body = to_bullet (Self);

  return (Shape*) the_Body->getCollisionShape ();

}




void*
b3d_Object_user_Data      (Object*   Self)
{
  btRigidBody*   the_Body = to_bullet (Self);

  return the_Body->getUserPointer ();
}



void
b3d_Object_user_Data_is   (Object*   Self,   void*   Now)
{
  btRigidBody*   the_Body = to_bullet (Self);

  the_Body->setUserPointer (Now);
}




Real
b3d_Object_Mass (Object*   Self)
{
  btRigidBody*   the_Body = to_bullet (Self);
  Real           inv_Mass = the_Body->getInvMass();

  if (inv_Mass == 0.0)
    return 0.0;
  else
    return 1.0 / inv_Mass;
}



void
b3d_Object_Friction_is    (Object*   Self,   Real   Now)
{
  btRigidBody*   the_Body = to_bullet (Self);

  the_Body->setFriction (Now);
}



void
b3d_Object_Restitution_is    (Object*   Self,   Real   Now)
{
  btRigidBody*   the_Body = to_bullet (Self);

  the_Body->setRestitution (Now);
}




Vector_3
b3d_Object_Site (Object*   Self)
{
  btRigidBody*   the_Body = to_bullet (Self);
  Vector_3       the_Site;

  btTransform&   trans    = the_Body->getWorldTransform ();
  btVector3      bt_Site  = trans.getOrigin();

  the_Site.x = bt_Site.x();
  the_Site.y = bt_Site.y();
  the_Site.z = bt_Site.z();

  return the_Site;
}



void
b3d_Object_Site_is (Object*   Self,   Vector_3*   Now)
{
  btRigidBody*   the_Body = to_bullet (Self);
  btTransform&   trans    = the_Body->getWorldTransform ();
  
  trans.setOrigin (btVector3 (Now->x, Now->y, Now->z));
  the_Body->activate();

  if (is_Kinematic (the_Body))
    {
      KinematicMotionState*    the_Motion_State = (KinematicMotionState*) the_Body->getMotionState();

      the_Motion_State->setKinematicPos (trans);
    }
}




Matrix_3x3
b3d_Object_Spin (Object*   Self)
{
  btRigidBody*   the_Body = to_bullet (Self);
  Vector_3       the_Site;

  btTransform&   trans    = the_Body->getWorldTransform ();
  btMatrix3x3    the_Spin = trans.getBasis();

  btVector3&     R1       = the_Spin [0];
  btVector3&     R2       = the_Spin [1];
  btVector3&     R3       = the_Spin [2];

  return Matrix_3x3 (R1 [0],  R1 [1],  R1 [2],
                     R2 [0],  R2 [1],  R2 [2],
                     R3 [0],  R3 [1],  R3 [2]);
}


void
b3d_Object_Spin_is (Object*   Self,   Matrix_3x3*   Now)
{
  btRigidBody*   the_Body = to_bullet (Self);
  btTransform&   trans    = the_Body->getWorldTransform();

  trans.setBasis (btMatrix3x3 (Now->m00, Now->m01, Now->m02,
                               Now->m10, Now->m11, Now->m12,
                               Now->m20, Now->m21, Now->m22));

  if (is_Kinematic (the_Body))
    {
      KinematicMotionState*    the_Motion_State = (KinematicMotionState*) the_Body->getMotionState();

      the_Motion_State->setKinematicPos (trans);
    }
}



Matrix_4x4
b3d_Object_Transform (Object*   Self)
{
  btRigidBody*   the_Body      = to_bullet (Self);
  btTransform&   trans         = the_Body->getWorldTransform ();
  btScalar       gl_Matrix [16];

  trans.getOpenGLMatrix (gl_Matrix);

  return Matrix_4x4 (gl_Matrix);
}



void
b3d_Object_Transform_is (Object*   Self,   Matrix_4x4*   Now)
{
  btRigidBody*   the_Body      = to_bullet (Self);
  
  
  if (is_Kinematic (the_Body))
    {
      btTransform              trans;
      KinematicMotionState*    the_Motion_State = (KinematicMotionState*) the_Body->getMotionState();

      trans.setFromOpenGLMatrix (&Now->m00);
      the_Motion_State->setKinematicPos (trans);
    }
  else
    {
      btTransform&   trans         = the_Body->getWorldTransform ();

      trans.setFromOpenGLMatrix (&Now->m00);
    }
}





Vector_3
b3d_Object_Speed (Object*   Self)
{
  btRigidBody*   the_Body = to_bullet (Self);
  Vector_3       the_Speed;
  btVector3      bt_Speed = the_Body->getLinearVelocity ();

  the_Speed.x = bt_Speed.x();
  the_Speed.y = bt_Speed.y();
  the_Speed.z = bt_Speed.z();

  return the_Speed;
}



void
b3d_Object_Speed_is (Object*   Self,   Vector_3*   Now)
{
  btRigidBody*   the_Body = to_bullet (Self);

  the_Body->setLinearVelocity (btVector3 (Now->x, Now->y, Now->z));
}




Vector_3
b3d_Object_Gyre (Object*   Self)
{
  btRigidBody*   the_Body = to_bullet (Self);
  Vector_3       the_Gyre;
  btVector3      bt_Gyre  = the_Body->getAngularVelocity ();

  the_Gyre.x = bt_Gyre.x();
  the_Gyre.y = bt_Gyre.y();
  the_Gyre.z = bt_Gyre.z();

  return the_Gyre;
}



void
b3d_Object_Gyre_is (Object*   Self,   Vector_3*   Now)
{
  btRigidBody*   the_Body = to_bullet (Self);

  the_Body->setAngularVelocity (btVector3 (Now->x, Now->y, Now->z));
}




void
b3d_Object_apply_Torque (Object*   Self,   Vector_3*   Torque)
{
  btRigidBody*   the_Body = to_bullet (Self);

  the_Body->applyTorque (btVector3 (Torque->x, Torque->y, Torque->z));
}



void
b3d_Object_apply_Torque_impulse (Object*   Self,   Vector_3*   Torque)
{
  btRigidBody*   the_Body = to_bullet (Self);

  the_Body->applyTorqueImpulse (btVector3 (Torque->x, Torque->y, Torque->z));
}



void
b3d_Object_apply_Force (Object*   Self,   Vector_3*   Force)
{
  btRigidBody*   the_Body = to_bullet (Self);

  the_Body->applyCentralImpulse (btVector3 (Force->x, Force->y, Force->z));
}


} // extern "C"
