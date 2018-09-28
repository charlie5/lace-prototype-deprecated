#ifndef C_BOX2D_OBJECT_H
#define C_BOX2D_OBJECT_H


#include "box2d.h"
#include "box2d-shape.h"


extern "C"
{
  struct Object;

  struct Object*   b2d_new_Object            (Vector_2* Site,
                                              Real      Mass,
                                              Real      Friction,
                                              Real      Restitution,
					      Shape*    the_Shape);

  void             b2d_free_Object           (Object*   Self);


  void             b2d_Object_Scale_is       (Object*   Self,   Vector_2*     Now);

  Shape*           b2d_Object_Shape          (Object*   Self);

  void*            b2d_Object_user_Data      (Object*   Self);
  void             b2d_Object_user_Data_is   (Object*   Self,   void*         Now);


  Real             b2d_Object_Mass           (Object*   Self);
  void             b2d_Object_Friction_is    (Object*   Self,   Real          Now);
  void             b2d_Object_Restitution_is (Object*   Self,   Real          Now);


  Vector_3         b2d_Object_Site           (Object*   Self);
  void             b2d_Object_Site_is        (Object*   Self,   Vector_3*     Now);

  Matrix_3x3       b2d_Object_Spin           (Object*   Self);
  void             b2d_Object_Spin_is        (Object*   Self,   Matrix_3x3*   Now);

  Real             b2d_Object_xy_Spin        (Object*   Self);
  void             b2d_Object_xy_Spin_is     (Object*   Self,   Real          Now);

  Matrix_4x4       b2d_Object_Transform      (Object*   Self);
  void             b2d_Object_Transform_is   (Object*   Self,   Matrix_4x4*   Now);


  Vector_3         b2d_Object_Speed          (Object*   Self);
  void             b2d_Object_Speed_is       (Object*   Self,   Vector_3*     Now);

  Vector_3         b2d_Object_Gyre           (Object*   Self);
  void             b2d_Object_Gyre_is        (Object*   Self,   Vector_3*     Now);


  void             b2d_Object_apply_Force          (Object*   Self,   Vector_3*   Force);
  void             b2d_Object_apply_Torque         (Object*   Self,   Vector_3*   Torque);
  void             b2d_Object_apply_Torque_impulse (Object*   Self,   Vector_3*   Torque);

  void             b2d_dump (Object*   Self);
} // extern "C"


#endif
