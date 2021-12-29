#include "bullet-joint.h"
#include "bullet-space.h"
#include "bullet-conversions.h"

#include <btBulletDynamicsCommon.h>
#include <BulletDynamics/ConstraintSolver/btHingeConstraint.h>


extern "C"
{
  /////////
  /// Forge
  //


  Joint*
  b3d_new_hinge_Joint (Object*       Object_A,
                       Object*       Object_B,
                       Matrix_4x4*   Frame_A,
                       Matrix_4x4*   Frame_B)
  {
    btTransform     Trans_A;
    btTransform     Trans_B;

    Trans_A.setFromOpenGLMatrix (&Frame_A->m00);
    Trans_B.setFromOpenGLMatrix (&Frame_B->m00);

    btTypedConstraint*   Self = (btTypedConstraint*) (new btHingeConstraint (*to_bullet_Object (Object_A),
                                                                             *to_bullet_Object (Object_B),
                                                                             Trans_A,
                                                                             Trans_B));
    return (Joint*) Self;
  }



  Joint*
  b3d_new_space_hinge_Joint (Object*       Object_A,
                             Matrix_4x4*   Frame_A)
  {
    btTransform     Trans_A;

    Trans_A.setFromOpenGLMatrix (&Frame_A->m00);

    btTypedConstraint*     Self = (btTypedConstraint*) (new btHingeConstraint (*to_bullet_Object (Object_A),
                                                                               Trans_A));
    return (Joint*) Self;
  }



  Joint*
  b3d_new_DoF6_Joint (Object*       Object_A,
                      Object*       Object_B,
                      Matrix_4x4*   Frame_A,
                      Matrix_4x4*   Frame_B)
  {
    btTransform     Trans_A;
    btTransform     Trans_B;

    Trans_A.setFromOpenGLMatrix (&Frame_A->m00);
    Trans_B.setFromOpenGLMatrix (&Frame_B->m00);

    btTypedConstraint*     Self = (btTypedConstraint*) (new btGeneric6DofConstraint (*to_bullet_Object (Object_A),
                                                                                     *to_bullet_Object (Object_B),
                                                                                     Trans_A,
                                                                                     Trans_B,
                                                                                     0));
    return (Joint*) Self;
  }



  Joint*
  b3d_new_cone_twist_Joint (Object*       Object_A,
                            Object*       Object_B,
                            Matrix_4x4*   Frame_A,
                            Matrix_4x4*   Frame_B)
  {
    btTransform     Trans_A;
    btTransform     Trans_B;

    Trans_A.setFromOpenGLMatrix (&Frame_A->m00);
    Trans_B.setFromOpenGLMatrix (&Frame_B->m00);

    btTypedConstraint*     Self = (btTypedConstraint*) (new btHingeConstraint (*to_bullet_Object (Object_A),
                                                                               *to_bullet_Object (Object_B),
                                                                               Trans_A,
                                                                               Trans_B));
    return (Joint*) Self;
  }



  Joint*
  b3d_new_slider_Joint (Object*       Object_A,
                        Object*       Object_B,
                        Matrix_4x4*   Frame_A,
                        Matrix_4x4*   Frame_B)
  {
    btTransform     Trans_A;
    btTransform     Trans_B;

    Trans_A.setFromOpenGLMatrix (&Frame_A->m00);
    Trans_B.setFromOpenGLMatrix (&Frame_B->m00);

    btTypedConstraint*     Self = (btTypedConstraint*) (new btSliderConstraint (*to_bullet_Object (Object_A),
                                                                                *to_bullet_Object (Object_B),
                                                                                Trans_A,
                                                                                Trans_B,
                                                                                0));
    return (Joint*) Self;
  }



  Joint*
  b3d_new_ball_Joint (Object*       Object_A,
                      Object*       Object_B,
                      Vector_3*     Pivot_in_A,
                      Vector_3*     Pivot_in_B)
  {
    btVector3     pivot_A = btVector3 (Pivot_in_A->x, Pivot_in_A->y, Pivot_in_A->z);
    btVector3     pivot_B = btVector3 (Pivot_in_B->x, Pivot_in_B->y, Pivot_in_B->z);

    btTypedConstraint*     Self = (btTypedConstraint*) (new btPoint2PointConstraint (*to_bullet_Object (Object_A),
                                                                                     *to_bullet_Object (Object_B),
                                                                                     pivot_A,
                                                                                     pivot_B));
    return (Joint*) Self;
  }



  //////////////
  /// Attributes
  //

  void*
  b3d_Joint_user_Data (Joint*   Self)
  {
    return NULL;
  }



  void b3d_Joint_user_Data_is(Joint*   Self,
                              void*    Now)
  {
    //  btTypedConstraint*   the_Joint = to_bullet_Joint (Self);

    // TODO: the_Joint->setUserPointer (Now);
  }



  Object*
  b3d_Joint_Object_A (Joint*   Self)
  {
    btTypedConstraint*     c_Self = to_bullet_Joint (Self);

    return to_bt3_Object (&c_Self->getRigidBodyA());
  }



  Object*
  b3d_Joint_Object_B (Joint*   Self)
  {
    btTypedConstraint*     c_Self = to_bullet_Joint (Self);

    return to_bt3_Object (&c_Self->getRigidBodyB());
  }



  bool b3d_Joint_Extent (Joint*   Self,
                         int      DoF)
  {
    // btTypedConstraint*     c_Self = to_bullet_Joint (Self);
    // return c_Self->Extent;

    printf ("TODO: b3d_Joint_Extent");
    return false;
  }



  // Below are for hinges, it seems.
  //

  Matrix_4x4
  b3d_Joint_Frame_A (Joint*   Self)
  {
    // TODO: This does not apply to all types ... have to check type and then use switch to convert to correct bullet joint pointer.

    btHingeConstraint*     c_Self = (btHingeConstraint*) to_bullet_Joint (Self);
    btTransform&           trans  = c_Self->getAFrame();
    btScalar               gl_Matrix [16];

    trans.getOpenGLMatrix (gl_Matrix);

    return Matrix_4x4 (gl_Matrix);
  }



  Matrix_4x4
  b3d_Joint_Frame_B (Joint*   Self)
  {
    btHingeConstraint*     c_Self = (btHingeConstraint*) to_bullet_Joint (Self);
    btTransform&           trans  = c_Self->getBFrame();
    btScalar               gl_Matrix [16];

    trans.getOpenGLMatrix (gl_Matrix);

    return Matrix_4x4 (gl_Matrix);
  }



  void b3d_Joint_Frame_A_is (Joint*        Self,
                             Matrix_4x4*   Now)
  {
    printf ("TODO: b3d_Joint_Frame_A_is");
  }



  void b3d_Joint_Frame_B_is (Joint*        Self,
                             Matrix_4x4*   Now)
  {
    printf ("TODO: b3d_Joint_Frame_B_is");
  }



  bool b3d_Joint_is_Limited(Joint*   Self,
                            int      DoF)
  {
    printf ("TODO: b3d_Joint_is_Limited");

    return false;
  }



  void b3d_Joint_Velocity_is (Joint*   Self,
                              int      DoF,
                              Real     Velocity)
  {
    printf ("TODO: b3d_Joint_Velocity_is");
  }



  // Hinge Joint
  //

  void b3d_Joint_hinge_Limits_are (Joint*   Self,
                                   Real     Lower,
                                   Real     Upper,
                                   Real     Softeness,
                                   Real     bias_Factor,
                                   Real     relaxation_Factor)
  {
    btHingeConstraint*     c_Self = (btHingeConstraint*) to_bullet_Joint (Self);

    c_Self->setLimit (Lower,
                      Upper,
                      Softeness,
                      bias_Factor,
                      relaxation_Factor);
  }



  // 6 Degrees of Freedom Joint (6DoF)
  //

  void b3d_Joint_6DoF_lower_Limit_is (Joint*   Self,
                                      int      DoF,
                                      Real     Now)
  {
    btGeneric6DofConstraint*     c_Self    = (btGeneric6DofConstraint*) to_bullet_Joint (Self);
    btRotationalLimitMotor*      the_Motor = c_Self->getRotationalLimitMotor (DoF - 4);

    the_Motor->m_loLimit = Now;

    c_Self->setOverrideNumSolverIterations (2000);   // Improves joint limit stiffness.
  }



  void b3d_Joint_6DoF_upper_Limit_is (Joint*   Self,
                                      int      DoF,
                                      Real     Now)
  {
    btGeneric6DofConstraint*     c_Self    = (btGeneric6DofConstraint*) to_bullet_Joint (Self);
    btRotationalLimitMotor *     the_Motor = c_Self->getRotationalLimitMotor (DoF - 4);

    the_Motor->m_hiLimit = Now;
  }



  Real b3d_Joint_6DoF_lower_Limit (Joint*   Self,
                                   int      DoF)
  {
    btGeneric6DofConstraint*     c_Self = (btGeneric6DofConstraint*) to_bullet_Joint (Self);

    return c_Self->getRotationalLimitMotor (DoF - 4)->m_loLimit;
  }



  Real b3d_Joint_6DoF_upper_Limit(Joint*   Self,
                                  int      DoF)
  {
    btGeneric6DofConstraint*     c_Self = (btGeneric6DofConstraint*) to_bullet_Joint (Self);

    return c_Self->getRotationalLimitMotor (DoF - 4)->m_hiLimit;
  }


} // extern "C"
