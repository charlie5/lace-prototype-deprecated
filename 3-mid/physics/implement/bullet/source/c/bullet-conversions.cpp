#include "bullet-conversions.h"


btVector3
to_btVector3 (Vector_3*   From)
{
  return btVector3 (From->x,
                    From->y,
                    From->z);
}



Vector_3
to_Vector_3 (btVector3&   From)
{
  Vector_3    Result;

  Result.x = From [0];
  Result.y = From [1];
  Result.z = From [2];

  return Result;
}



btTypedConstraint*
to_bullet_Joint (Joint*   From)
{
  return (btTypedConstraint*) From;
}



Joint*
to_bt3_Joint (btTypedConstraint*   From)
{
  return (Joint*) From;
}




btRigidBody*
to_bullet_Object (Object*   From)
{
  return (btRigidBody*) From;
}


Object*
to_bt3_Object (btRigidBody*   From)
{
  return (Object*) From;
}


