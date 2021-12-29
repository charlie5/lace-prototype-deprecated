#include "box2d-joint.h"
#include "box2d-space.h"
#include "box2d-conversions.h"
#include "box2d-object-private.h"

#include <box2d/box2d.h>
#include <stdio.h>



////////////////
///  C++ Support
//


class my_b2RevoluteJoint : public b2RevoluteJoint   // This is needed to expose the m_localAnchorA/B members for modification.
{
public:
	/// The local anchor point relative to bodyA's origin.
	b2Vec2&       GetLocalAnchorA()       { return m_localAnchorA; }

	/// The local anchor point relative to bodyB's origin.
	b2Vec2&       GetLocalAnchorB()        { return m_localAnchorB; }
};






extern "C"
{

//////////
///  Forge
//


Joint*
b2d_new_hinge_Joint_with_local_anchors
  (Space*        in_Space,
   Object*       Object_A,
   Object*       Object_B,
   Vector_3*     Anchor_in_A,
   Vector_3*     Anchor_in_B,
   float          low_Limit,
   float          high_Limit,
   bool          collide_Connected)
{
  b2RevoluteJointDef*   Self = new b2RevoluteJointDef();


  Self->bodyA = (b2Body*) Object_A;   // Using the jointDefs' bodyA/B to hold pointers to our 'fat' Object_A/B.
  Self->bodyB = (b2Body*) Object_B;   // The actual b2Body will be substituted when the joint is added to the world.

  Self->localAnchorA = b2Vec2 (Anchor_in_A->x,
			       Anchor_in_A->y);

  Self->localAnchorB = b2Vec2 (Anchor_in_B->x,
			       Anchor_in_B->y);

  Self->lowerAngle  = low_Limit;
  Self->upperAngle  = high_Limit;
  Self->enableLimit = true;

  Self->collideConnected = collide_Connected;

  return (Joint*) dynamic_cast <b2JointDef*> (Self);
}



Joint*
b2d_new_hinge_Joint (Space*        in_Space,
		     Object*       Object_A,
		     Object*       Object_B,
		     Matrix_4x4*   Frame_A,
		     Matrix_4x4*   Frame_B,
		     float          low_Limit,
                     float          high_Limit,
		     bool          collide_Connected)
{
  b2RevoluteJointDef*   Self = new b2RevoluteJointDef();


  Self->bodyA = (b2Body*) Object_A;   // Using the jointDefs' bodyA/B to hold pointers to our 'fat' Object.
  Self->bodyB = (b2Body*) Object_B;   // The actual b2Body will be substituted when the joint is added to the world.


  Self->lowerAngle  = low_Limit;
  Self->upperAngle  = high_Limit;
  Self->enableLimit = true;

  Self->collideConnected = collide_Connected;

  return (Joint*) dynamic_cast <b2JointDef*> (Self);
}



Joint*
b2d_new_space_hinge_Joint (Object*       Object_A,
                           Matrix_4x4*   Frame_A)
{
  return 0;
}



void
b2d_free_hinge_Joint (Joint*        Self)
{
   b2JointDef*            b2_Self          = (b2JointDef*) Self;
   b2RevoluteJointDef*    b2_revolute_Self = (b2RevoluteJointDef*) b2_Self;

   delete b2_revolute_Self;
}



Joint*
b2d_new_DoF6_Joint (Object*       Object_A,
                    Object*       Object_B,
                    Matrix_4x4*   Frame_A,
                    Matrix_4x4*   Frame_B)
{
  return 0;
}


Joint*
b2d_new_cone_twist_Joint (Object*       Object_A,
                          Object*       Object_B,
                          Matrix_4x4*   Frame_A,
                          Matrix_4x4*   Frame_B)
{
  return 0;
}



Joint*
b2d_new_slider_Joint (Object*       Object_A,
                      Object*       Object_B,
                      Matrix_4x4*   Frame_A,
                      Matrix_4x4*   Frame_B)
{
  return 0;
}



Joint*
b2d_new_ball_Joint (Object*       Object_A,
                    Object*       Object_B,
                    Vector_3*     Pivot_in_A,
                    Vector_3*     Pivot_in_B)
{
  return 0;
}






///////////////
///  Attributes
//



void
b2d_Joint_set_local_Anchor (Joint*   Self,   bool        is_Anchor_A,
                                             Vector_3*   local_Anchor)
{
  b2JointDef*            b2_Self           = (b2JointDef*)         Self;
  b2RevoluteJointDef*    b2_revolute_Self  = (b2RevoluteJointDef*) b2_Self;
  b2Joint*               b2_Joint          = (b2Joint*)            b2_revolute_Self->userData.pointer;
  my_b2RevoluteJoint*    b2_revolute_Joint = static_cast <my_b2RevoluteJoint*> (b2_Joint);

  if (is_Anchor_A)
    b2_revolute_Joint->GetLocalAnchorA() = (b2Vec2 (local_Anchor->x,
                                                    local_Anchor->y));
  else
    b2_revolute_Joint->GetLocalAnchorB() = (b2Vec2 (local_Anchor->x,
						    local_Anchor->y));
}



Vector_3
b2d_Joint_reaction_Force  (Joint*   Self)
{
  b2JointDef*            b2_Self   = (b2JointDef*) Self;
  b2Joint*               b2_Joint  = (b2Joint*)    b2_Self->userData.pointer;
  b2Vec2                 the_Force = b2_Joint->GetReactionForce (1.0 / 60.0);

  return {the_Force.x, the_Force.y, 0.0};
}



Real
b2d_Joint_reaction_Torque (Joint*   Self)
{
  b2JointDef*            b2_Self  = (b2JointDef*) Self;
  b2Joint*               b2_Joint = (b2Joint*)    b2_Self->userData.pointer;

  return b2_Joint->GetReactionTorque (1.0 / 60.0);
}



void*
b2d_Joint_user_Data      (Joint*   Self)
{
  b2JointDef*            b2_Self  = (b2JointDef*) Self;
  b2Joint*               b2_Joint = (b2Joint*)    b2_Self->userData.pointer;

  return (void*) b2_Joint->GetUserData().pointer;
}


//void
//b2d_Joint_user_Data_is   (Joint*   Self,   void*   Now)
//{
//  b2JointDef*            b2_Self  = (b2JointDef*) Self;
//  b2Joint*               b2_Joint = (b2Joint*)    b2_Self->userData.pointer;

//  return b2_Joint->SetUserData (Now);
//}



Object*
b2d_Joint_Object_A (Joint*   Self)
{
  return 0;
}



Object*
b2d_Joint_Object_B (Joint*   Self)
{
  return 0;
}




Matrix_4x4
b2d_Joint_Frame_A (Joint*   Self)
{
  Matrix_4x4   dummy;
  return dummy;
}



Matrix_4x4
b2d_Joint_Frame_B        (Joint*   Self)
{
  Matrix_4x4   dummy;
  return dummy;
}


void
b2d_Joint_Frame_A_is     (Joint*   Self,   Matrix_4x4*   Now)
{
    printf ("TODO: b3d_Joint_Frame_A_is");
}


void
b2d_Joint_Frame_B_is     (Joint*   Self,   Matrix_4x4*   Now)
{
    printf ("TODO: b2d_Joint_Frame_B_is");
}


bool
b2d_Joint_is_Limited     (Joint*   Self,   int           DoF)
{
    printf ("TODO: b2d_Joint_is_Limited");
    return false;
}


bool
b2d_Joint_Extent         (Joint*   Self,   int           DoF)
{
    printf ("TODO: b2d_Joint_Extent");
    return false;
}


void
b2d_Joint_Velocity_is    (Joint*   Self,   int           DoF,
                                           float         Real)
{
    printf ("TODO: b2d_Joint_Velocity_is");
}



/// Hinge
//

void
b2d_Joint_hinge_Limits_are (Joint*   Self,   Real   Low,
                                             Real   High)
{
  b2JointDef*            b2_Self  = (b2JointDef*) Self;
  b2Joint*               b2_Joint = (b2Joint*)    b2_Self->userData.pointer;
  b2RevoluteJoint*       b2_Hinge = dynamic_cast <b2RevoluteJoint*> (b2_Joint);

  if (b2_Hinge)
    b2_Hinge->SetLimits (Low, High);
  else
    {
      b2RevoluteJointDef*    b2_revolute_Self  = (b2RevoluteJointDef*) b2_Self;

      b2_revolute_Self->lowerAngle = Low;
      b2_revolute_Self->upperAngle = High;
    }
}


} // extern "C"
