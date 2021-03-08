#ifndef C_BOX2D_JOINT_H
#define C_BOX2D_JOINT_H

#include "box2d.h"
#include "box2d-object.h"


extern "C"
{
  
  struct Joint;
  struct Space;

  
  /////////
  /// Forge
  //
  
  Joint*     b2d_new_hinge_Joint_with_local_anchors
                                         (Space*        in_Space,
					  Object*       Object_A,
                                          Object*       Object_B,
                                          Vector_3*     Anchor_in_A,
                                          Vector_3*     Anchor_in_B,
					  float         low_Limit,
					  float         high_Limit,
					  bool          collide_Connected);
					  
  Joint*     b2d_new_hinge_Joint         (Space*        in_Space,
					  Object*       Object_A,
                                          Object*       Object_B,
                                          Matrix_4x4*   Frame_A,
                                          Matrix_4x4*   Frame_B,
					  float         low_Limit,
					  float         high_Limit,
					  bool          collide_Connected);
					  
  void       b2d_free_hinge_Joint        (Joint*        Self);

  Joint*     b2d_new_space_hinge_Joint   (Object*       Object_A,
                                          Matrix_4x4*   Frame_A);


  Joint*     b2d_new_DoF6_Joint          (Object*       Object_A,
                                          Object*       Object_B,
                                          Matrix_4x4*   Frame_A,
                                          Matrix_4x4*   Frame_B);

  Joint*     b2d_new_cone_twist_Joint    (Object*       Object_A,
                                          Object*       Object_B,
                                          Matrix_4x4*   Frame_A,
                                          Matrix_4x4*   Frame_B);

  Joint*     b2d_new_slider_Joint        (Object*       Object_A,
                                          Object*       Object_B,
                                          Matrix_4x4*   Frame_A,
                                          Matrix_4x4*   Frame_B);

  Joint*     b2d_new_ball_Joint          (Object*       Object_A,
                                          Object*       Object_B,
                                          Vector_3*     Pivot_in_A,
                                          Vector_3*     Pivot_in_B);

 
  //////////////
  /// Attributes
  //

  void*             b2d_Joint_user_Data        (Joint*   Self);
  void              b2d_Joint_user_Data_is     (Joint*   Self,   void*         Now);

  Object*           b2d_Joint_Object_A         (Joint*   Self);
  Object*           b2d_Joint_Object_B         (Joint*   Self);

  Matrix_4x4        b2d_Joint_Frame_A          (Joint*   Self);
  Matrix_4x4        b2d_Joint_Frame_B          (Joint*   Self);

  void              b2d_Joint_Frame_A_is       (Joint*   Self,   Matrix_4x4*   Now);
  void              b2d_Joint_Frame_B_is       (Joint*   Self,   Matrix_4x4*   Now);

  void              b2d_Joint_set_local_Anchor (Joint*   Self,   bool          is_Anchor_A,
						                 Vector_3*     local_Anchor);
  
  
  bool              b2d_Joint_is_Limited       (Joint*   Self,   int           DoF);
  bool              b2d_Joint_Extent           (Joint*   Self,   int           DoF);

  void              b2d_Joint_Velocity_is      (Joint*   Self,   int           DoF,
                                                                 Real          Now);

  Vector_3          b2d_Joint_reaction_Force   (Joint*   Self);
  Real              b2d_Joint_reaction_Torque  (Joint*   Self);
  
  
  
  /// Hinge
  //
  
  void              b2d_Joint_hinge_Limits_are (Joint*   Self,   Real          Low, 
                                                                 Real          High);
  
  
} // extern "C"


#endif
