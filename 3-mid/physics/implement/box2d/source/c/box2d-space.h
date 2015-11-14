#ifndef C_BOX2D_SPACE_H
#define C_BOX2D_SPACE_H


#include "box2d.h"
#include "box2d-object.h"
#include "box2d-joint.h"



extern "C"
{
  struct Space;


  struct Space*      b2d_new_Space ();
  void               b2d_free_Space       (Space*   Self);

  void               b2d_Space_add_Object (Space*   Self,    Object*     the_Object);
  void               b2d_Space_rid_Object (Space*   Self,    Object*     the_Object);

  void               b2d_Space_add_Joint  (Space*   Self,    Joint*      the_Joint);
  void               b2d_Space_rid_Joint  (Space*   Self,    Joint*      the_Joint);


  struct b2Joint;
  void*              b2d_b2Joint_user_Data (b2Joint*   the_Joint);


  struct joint_Cursor
  {
    b2Joint*   Joint;
  };

  joint_Cursor       b2d_Space_first_Joint    (Space*          Self);
  void               b2d_Space_next_Joint    (joint_Cursor*   Cursor);
  b2Joint*           b2d_Space_joint_Element (joint_Cursor*   Cursor);


  void               b2d_Space_Gravity_is (Space*   Self,    Vector_3*   Now);
  void               b2d_Space_evolve     (Space*   Self,    float       By);



  //  Ray Casting
  //
  struct b2d_ray_Collision
  {
      const Object*    near_Object;
      Real             hit_Fraction;
      Vector_3         Normal_world;
      Vector_3         Site_world;
  };

  b2d_ray_Collision      b2d_Space_cast_Ray (Space*   Self,    Vector_3*   From,
                                                               Vector_3*   To);

  //  Collisions
  //

  struct b2d_Contact
  {
    Object*          Object_A;
    Object*          Object_B;
    Vector_3         Site;
  };

  int                b2d_space_contact_Count (Space*   Self);
  b2d_Contact        b2d_space_Contact       (Space*   Self,   int   contact_Id);

} // extern "C"


#endif

