#include "bullet-space.h"
#include "bullet-conversions.h"
#include "btBulletDynamicsCommon.h"


extern "C"
{

struct Space
{
  btDefaultCollisionConfiguration*       collisionConfiguration;
  btCollisionDispatcher*                 dispatcher;
  btBroadphaseInterface*                 overlappingPairCache;
  btSequentialImpulseConstraintSolver*   solver;
  btDiscreteDynamicsWorld*               dynamicsWorld;

  unsigned                               moved_Count;
};



struct Space*
b3d_new_Space ()
{
  Space*    Self = new Space();

  // collision configuration contains default setup for memory, collision setup. Advanced users can create their own configuration.
  //
  Self->collisionConfiguration = new btDefaultCollisionConfiguration();


  // use the default collision dispatcher. For parallel processing you can use a diffent dispatcher (see Extras/BulletMultiThreaded)
  //
  Self->dispatcher = new btCollisionDispatcher (Self->collisionConfiguration);


  // btDbvtBroadphase is a good general purpose broadphase. You can also try out btAxis3Sweep.
  //
  Self->overlappingPairCache = new btDbvtBroadphase();


  // the default constraint solver. For parallel processing you can use a different solver (see Extras/BulletMultiThreaded)
  //
  Self->solver = new btSequentialImpulseConstraintSolver;


  Self->dynamicsWorld = new btDiscreteDynamicsWorld (Self->dispatcher,
                                                     Self->overlappingPairCache,
                                                     Self->solver,
                                                     Self->collisionConfiguration);
  return Self;
}




void
b3d_free_Space (Space*   Self)
{
    //delete dynamics world
	delete Self->dynamicsWorld;

	//delete solver
	delete Self->solver;

	//delete broadphase
	delete Self->overlappingPairCache;

	//delete dispatcher
	delete Self->dispatcher;

	delete Self->collisionConfiguration;

    delete Self;
}



void
b3d_Space_Gravity_is (Space*   Self,    Vector_3*     Now)
{
  Self->dynamicsWorld->setGravity (btVector3 (Now->x, Now->y, Now->z));
}



void
b3d_Space_evolve (Space*   Self,     float   By)
{
  Self->dynamicsWorld->stepSimulation (By,  10);
}



void
b3d_Space_add_Object (Space*   Self,    Object*   the_Object)
{
  Self->dynamicsWorld->addRigidBody ((btRigidBody*) (the_Object));
}



void
b3d_Space_rid_Object (Space*   Self,    Object*   the_Object)
{
  Self->dynamicsWorld->removeRigidBody ((btRigidBody*) (the_Object));
}



void
b3d_Space_add_Joint (Space*   Self,    Joint*   the_Joint)
{
  bool    disable_Collisions = true;

  Self->dynamicsWorld->addConstraint ((btTypedConstraint*) (the_Joint),
                                      disable_Collisions);
}



ray_Collision
b3d_Space_cast_Ray (Space*   Self,    Vector_3*   From,
                                      Vector_3*   To)
{
  btVector3                                    rayFrom    = to_btVector3 (From);
  btVector3                                    rayTo      = to_btVector3 (To);
  btCollisionWorld::ClosestRayResultCallback   rayCallback (rayFrom, rayTo);

  Self->dynamicsWorld->rayTest (rayFrom, rayTo,
                                rayCallback);

  ray_Collision   the_Collision;

  the_Collision.near_Object  = (Object*) (rayCallback.m_collisionObject);
  the_Collision.hit_Fraction = rayCallback.m_closestHitFraction;
  the_Collision.Normal_world = to_Vector_3 (rayCallback.m_hitNormalWorld);
  the_Collision.Site_world   = to_Vector_3 (rayCallback.m_hitPointWorld);

  return the_Collision;
}



} // extern "C"

