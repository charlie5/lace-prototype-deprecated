#ifndef C_BULLET_OBJECT_H
#define C_BULLET_OBJECT_H


#include "bullet.h"
#include "bullet-shape.h"


extern "C"
{
  struct Object;

  struct Object*       b3d_new_Object            (Real     Mass,
						  Shape*   the_Shape,
						  int      is_Kinematic);

  Shape*               b3d_Object_Shape          (Object*   Self);

  void*                b3d_Object_user_Data      (Object*   Self);
  void                 b3d_Object_user_Data_is   (Object*   Self,   void*   Now);


  Real                 b3d_Object_Mass           (Object*   Self);
  void                 b3d_Object_Friction_is    (Object*   Self,   Real   Now);
  void                 b3d_Object_Restitution_is (Object*   Self,   Real   Now);


  Vector_3             b3d_Object_Site           (Object*   Self);
  void                 b3d_Object_Site_is        (Object*   Self,   Vector_3*   Now);

  Matrix_3x3           b3d_Object_Spin           (Object*   Self);
  void                 b3d_Object_Spin_is        (Object*   Self,   Matrix_3x3*   Now);

  Matrix_4x4           b3d_Object_Transform      (Object*   Self);
  void                 b3d_Object_Transform_is   (Object*   Self,   Matrix_4x4*   Now);


  Vector_3             b3d_Object_Speed          (Object*   Self);
  void                 b3d_Object_Speed_is       (Object*   Self,   Vector_3*   Now);

  Vector_3             b3d_Object_Gyre           (Object*   Self);
  void                 b3d_Object_Gyre_is        (Object*   Self,   Vector_3*   Now);


  void                 b3d_Object_apply_Force          (Object*   Self,   Vector_3*   Force);
  void                 b3d_Object_apply_Torque         (Object*   Self,   Vector_3*   Torque);
  void                 b3d_Object_apply_Torque_impulse (Object*   Self,   Vector_3*   Torque);

}; // extern "C"


#endif

