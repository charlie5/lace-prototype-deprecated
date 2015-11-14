#ifndef C_BULLET_CONVERSIONS_H
#define C_BULLET_CONVERSIONS_H

#include "bullet-joint.h"
#include <BulletDynamics/ConstraintSolver/btHingeConstraint.h>
#include <BulletDynamics/Dynamics/btRigidBody.h>



  btVector3            to_btVector3    (Vector_3*            From);
  Vector_3             to_Vector_3     (btVector3&           From);


  btTypedConstraint*   to_bullet_Joint (Joint*               From);
  Joint*               to_bt3_Joint    (btTypedConstraint*   From);


  btRigidBody*         to_bullet_Object (Object*        From);
  Object*              to_bt3_Object    (btRigidBody*   From);


#endif

