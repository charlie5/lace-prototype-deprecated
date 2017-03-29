#ifndef C_BULLET_SPACE_H
#define C_BULLET_SPACE_H


#include "bullet.h"
#include "bullet-object.h"
#include "bullet-joint.h"



extern "C"
{
  struct Space;


  struct Space*      b3d_new_Space ();
  void               b3d_free_Space (Space*   Self);

  void               b3d_Space_add_Object (Space*   Self,    Object*     the_Object);
  void               b3d_Space_rid_Object (Space*   Self,    Object*     the_Object);

  void               b3d_Space_add_Joint  (Space*   Self,    Joint*      the_Joint);

  void               b3d_Space_Gravity_is (Space*   Self,    Vector_3*   Now);

  void               b3d_Space_evolve     (Space*   Self,    float       By);


  //  Ray Casting
  //
  struct ray_Collision
  {
      const Object*    near_Object;
      Real             hit_Fraction;
      Vector_3         Normal_world;
      Vector_3         Site_world;
  };

  ray_Collision      b3d_Space_cast_Ray (Space*   Self,    Vector_3*   From,
                                                           Vector_3*   To);

} // extern "C"



#endif
