#include "box2d-space.h"
#include "box2d-conversions.h"

#include <box2d/box2d.h>
#include "box2d-object-private.h"

#include <stdio.h>



///////////////
/// Conversions
//

b2World*
to_World (Space*   From)
{
  return (b2World*) From;
}


Space*
to_Space (b2World*   From)
{
  return (Space*) From;
}




///////////////
/// C++ Support
//


///  Raycasts
//

class my_raycast_Callback : public b2RayCastCallback
{
public:

  b2Fixture*   Nearest;

  float
  ReportFixture
    (b2Fixture*      fixture,
		 const b2Vec2&   point,
		 const b2Vec2&   normal,
		 float           fraction)
  {
    Nearest = fixture;

    return fraction;
  }

};


/// Collisions
//

const int32     k_maxContactPoints = 4 * 2048;

struct ContactPoint
{
	b2Fixture*      fixtureA;
	b2Fixture*      fixtureB;
	b2Vec2          normal;
	b2Vec2          position;
	b2PointState    state;
	float           normalImpulse;
	float           tangentImpulse;
	float           separation;
};


class contact_Listener : public b2ContactListener
{
public:
	         contact_Listener();
	virtual ~contact_Listener();


	virtual void BeginContact (b2Contact*   contact) { B2_NOT_USED(contact); }
	virtual void EndContact   (b2Contact*   contact) { B2_NOT_USED(contact); }
	virtual void PreSolve     (b2Contact*   contact, const b2Manifold*         oldManifold);
	virtual void PostSolve    (b2Contact*   contact, const b2ContactImpulse*   impulse)
	{
		B2_NOT_USED(contact);
		B2_NOT_USED(impulse);
	}

	ContactPoint   m_points[k_maxContactPoints];
	int32          m_pointCount;
};


contact_Listener::
contact_Listener()
{
  m_pointCount = 0;
}


contact_Listener::
~contact_Listener()
{
}


void
contact_Listener::
PreSolve (b2Contact*          contact,
	  const b2Manifold*   oldManifold)
{
  if (m_pointCount == k_maxContactPoints)
    return;

  const b2Manifold*     manifold = contact->GetManifold();

  if (manifold->pointCount == 0)
    return;


  b2Fixture*            fixtureA = contact->GetFixtureA();
  b2Fixture*            fixtureB = contact->GetFixtureB();

  b2PointState          state1 [b2_maxManifoldPoints],
                        state2 [b2_maxManifoldPoints];

  b2GetPointStates (state1,      state2,
		    oldManifold, manifold);

  b2WorldManifold       worldManifold;

  contact->GetWorldManifold (&worldManifold);

  ContactPoint*         cp = m_points + m_pointCount;

  cp->fixtureA = fixtureA;
  cp->fixtureB = fixtureB;
  cp->position.SetZero();

  for (int32 i = 0;   i < manifold->pointCount;   ++i)
    {
      cp->position      += worldManifold.points [i];
      cp->normal         = worldManifold.normal;
      cp->state          = state2 [i];
      cp->normalImpulse  = manifold->points [i].normalImpulse;
      cp->tangentImpulse = manifold->points [i].tangentImpulse;
      cp->separation     = worldManifold.separations [i];
    }

  if (manifold->pointCount > 1)
    cp->position *= (1.0 / float (manifold->pointCount));   // Calculate middle site.

  ++m_pointCount;
}


///////////////
/// C Interface
//

extern "C"
{

int
b2d_space_contact_Count (Space*   Self)
{
  b2World*            the_World            = to_World (Self);
  contact_Listener*   the_contact_Listener = dynamic_cast <contact_Listener*> (the_World->GetContactManager().m_contactListener);

  return the_contact_Listener->m_pointCount;
}


b2d_Contact
b2d_space_Contact       (Space*   Self,   int   contact_Id)
{
  b2World*            the_World            = to_World (Self);
  contact_Listener*   the_contact_Listener = dynamic_cast <contact_Listener*> (the_World->GetContactManager().m_contactListener);

  ContactPoint*       point                = the_contact_Listener->m_points + contact_Id;

  b2Body*             body1                = point->fixtureA->GetBody();
  b2Body*             body2                = point->fixtureB->GetBody();

  b2d_Contact         the_Contact;

  the_Contact.Object_A = (Object*) (body1->GetUserData().pointer);
  the_Contact.Object_B = (Object*) (body2->GetUserData().pointer);

  the_Contact.Site.x = point->position.x;
  the_Contact.Site.y = point->position.y;
  the_Contact.Site.z = 0.0;

  return the_Contact;
}



struct Space*
b2d_new_Space ()
{
  b2World*    Self = new b2World (b2Vec2 (0.0, -9.8));

  Self->SetContactListener (new contact_Listener());

  return to_Space (Self);
}


void
b2d_free_Space (struct Space*    Self)
{
  b2World*   the_World = to_World (Self);

  delete the_World->GetContactManager().m_contactListener;
  delete the_World;
}


void
b2d_Space_Gravity_is (Space*   Self,    Vector_3*     Now)
{
  b2World*   the_World = to_World (Self);

  the_World->SetGravity (b2Vec2 (Now->x, Now->y));
}


void
b2d_Space_evolve (Space*   Self,     float   By)
{
  b2World*            the_World            = to_World (Self);
  contact_Listener*   the_contact_Listener = dynamic_cast <contact_Listener*> (the_World->GetContactManager().m_contactListener);

  the_contact_Listener->m_pointCount = 0;

  the_World->Step (By,  6, 2);
}


void
b2d_Space_add_Object (Space*   Self,    Object*   the_Object)
{
  b2World*   the_World = (b2World*)Self;

  the_Object->body = the_World->CreateBody (&the_Object->bodyDef);
  // the_Object->body->SetUserData (the_Object);

  the_Object->body->CreateFixture (&the_Object->fixtureDef);
}


void
b2d_Space_rid_Object (Space*   Self,    Object*   the_Object)
{
  ((b2World*)Self)->DestroyBody (the_Object->body);
  the_Object->body = 0;
}


void
b2d_Space_add_Joint (Space*   Self,    Joint*   the_Joint)
{
  b2World*           the_World = (b2World*)    Self;
  b2JointDef*        jointDef  = (b2JointDef*) the_Joint;

  Object*            Object_A  = (Object*) jointDef->bodyA;
  Object*            Object_B  = (Object*) jointDef->bodyB;

  jointDef->bodyA = Object_A->body;
  jointDef->bodyB = Object_B->body;

  if (jointDef->type == e_revoluteJoint)
    {
      b2RevoluteJointDef*    revolute_Def = static_cast <b2RevoluteJointDef*> (jointDef);
      b2RevoluteJoint*       the_Joint;

      the_Joint                  = (b2RevoluteJoint*) the_World->CreateJoint (revolute_Def);
      jointDef->userData.pointer = (uintptr_t) dynamic_cast <b2Joint*> (the_Joint);
    }
  else
    {
      printf ("TODO: b2d_Space_add_Joint");
    }
}


void
b2d_Space_rid_Joint (Space*   Self,    Joint*   the_Joint)
{
  b2World*           the_World     = (b2World*)    Self;

  b2JointDef*        the_Joint_Def = (b2JointDef*) the_Joint;
  b2Joint*           b2d_Joint     = (b2Joint*)    the_Joint_Def->userData.pointer;

  Object*            Object_A      = (Object*)     the_Joint_Def->bodyA->GetUserData().pointer;
  Object*            Object_B      = (Object*)     the_Joint_Def->bodyB->GetUserData().pointer;


  the_World->DestroyJoint (b2d_Joint);

  the_Joint_Def->bodyA = (b2Body*) Object_A;
  the_Joint_Def->bodyB = (b2Body*) Object_B;
}


void*
b2d_b2Joint_user_Data (b2Joint*   the_Joint)
{
  return (void*) the_Joint->GetUserData().pointer;
}



/// Joint Cursor
//

joint_Cursor
b2d_Space_first_Joint    (Space*          Self)
{
  b2World*           the_World     = (b2World*)    Self;

  return {the_World->GetJointList()};
}


void
b2d_Space_next_Joint    (joint_Cursor*   Cursor)
{
  Cursor->Joint = Cursor->Joint->GetNext();
}


b2Joint*
b2d_Space_joint_Element (joint_Cursor*   Cursor)
{
  return Cursor->Joint;
}



///  Raycasts
//

b2d_ray_Collision
b2d_Space_cast_Ray (Space*   Self,    Vector_3*   From,
                                      Vector_3*   To)
{
  b2World*              the_World     = (b2World*)    Self;
  my_raycast_Callback   the_Callback;

  the_Callback.Nearest = 0;

  the_World->RayCast (&the_Callback,
		      b2Vec2 (From->x, From->y),
		      b2Vec2 (To  ->x, To  ->y));

  b2d_ray_Collision   the_Collision;

  if (the_Callback.Nearest == 0)
    the_Collision.near_Object = 0;
  else
    the_Collision.near_Object  = (Object*) (the_Callback.Nearest->GetBody()->GetUserData().pointer);

  the_Collision.hit_Fraction = 0.0;
  the_Collision.Normal_world = Vector_3 (0.0, 0.0, 0.0);
  the_Collision.Site_world   = Vector_3 (0.0, 0.0, 0.0);

  return the_Collision;
}


} // extern "C"
