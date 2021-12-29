#include "box2d-object.h"
#include "box2d-object-private.h"
#include <box2d/box2d.h>

#include <cmath>
#include <stdio.h>


extern "C" {

struct Object*
b2d_new_Object (Vector_2*   Site,
                Real        Mass,
                Real        Friction,
                Real        Restitution,
                Shape*      the_Shape)
{
  Object*    Self     = new Object;
  b2Shape*   b2_Shape = (b2Shape*) (the_Shape);

  if (Mass > 0.0)
    Self->bodyDef.type = b2_dynamicBody;

  Self->body = 0;
  Self->bodyDef.position.Set (Site->x,
                              Site->y);

  Self->fixtureDef.shape       = b2_Shape;
  Self->fixtureDef.density     = Mass;
  Self->fixtureDef.friction    = Friction;
  Self->fixtureDef.restitution = Restitution;

  Self->Scale = b2Vec2 (1.0, 1.0);

  return Self;
}



void
b2d_free_Object (Object*   Self)
{
  delete (Self);
}



void
b2d_Object_Scale_is (Object*     Self,
                     Vector_2*   Now)
{
  b2Vec2     old_Scale = Self->Scale;

  Self->Scale = b2Vec2 (Now->x, Now->y);

  //  Shape
  //
  b2Shape*   the_Shape = (b2Shape*) Self->fixtureDef.shape;

  if (the_Shape->GetType() == b2Shape::e_circle)
    {
       the_Shape->m_radius = Now->x / 2.0;
    }
  else if (the_Shape->GetType() == b2Shape::e_polygon)
    {
      b2PolygonShape*   the_Polygon = (b2PolygonShape*) the_Shape;

      for (int i = 0;  i < the_Polygon->m_count;  i++)
	      {
	         the_Polygon->m_vertices [i].x = the_Polygon->m_vertices [i].x / old_Scale.x * Self->Scale.x;
	         the_Polygon->m_vertices [i].y = the_Polygon->m_vertices [i].y / old_Scale.x * Self->Scale.y;
	      }

        the_Polygon->Set (the_Polygon->m_vertices,
                          the_Polygon->m_count);
    }


  //  Body
  //
  if (Self->body)
    {
      Self->body->DestroyFixture (Self->body->GetFixtureList());
      Self->body->CreateFixture  (&Self->fixtureDef);
    }
}



Shape*
b2d_Object_Shape (Object*   Self)
{
  return (Shape*) Self->fixtureDef.shape;
}



void*
b2d_Object_user_Data (Object*   Self)
{
  return Self->userData;
}



void b2d_Object_user_Data_is (Object*   Self,
                              void*     Now)
{
  Self->userData = Now;
}



Real
b2d_Object_Mass (Object*   Self)
{
  if (Self->body)
    return Self->body->GetMass();

  return Self->fixtureDef.density;
}



void
b2d_Object_Friction_is (Object*   Self,
                        Real      Now)
{
  Self->fixtureDef.friction = Now;
}



void
b2d_Object_Restitution_is (Object*   Self,
                           Real      Now)
{
  Self->fixtureDef.restitution = Now;
}



Vector_3
b2d_Object_Site (Object*   Self)
{
  Vector_3     the_Site;

  if (Self->body)
    {
      b2Vec2     Pos = Self->body->GetPosition();

      the_Site.x = Pos.x;
      the_Site.y = Pos.y;
    }
  else
    {
      the_Site.x = Self->bodyDef.position (0);
      the_Site.y = Self->bodyDef.position (1);
    }

    the_Site.z = 0.0;

  return the_Site;
}



void
b2d_Object_Site_is (Object*     Self,
                    Vector_3*   Now)
{
  if (Self->body)
    {
      b2Vec2     the_Site;

      the_Site.x = Now->x;
      the_Site.y = Now->y;

      Self->body->SetTransform (the_Site,
                                Self->body->GetAngle());
    }
  else
    {
      Self->bodyDef.position.Set (Now->x, Now->y);
    }
}



Matrix_3x3
b2d_Object_Spin (Object*   Self)
{
  b2Vec2     x_Axis;
  b2Vec2     y_Axis;
  b2Rot      b2_Rotation;

  if (Self->body)
    {
      b2Transform     b2_Transform = Self->body->GetTransform();

      b2_Rotation = b2_Transform.q;
    }
  else
    {
      b2_Rotation = b2Rot (Self->bodyDef.angle);
    }

  x_Axis = b2_Rotation.GetXAxis();
  y_Axis = b2_Rotation.GetYAxis();

  return Matrix_3x3 (x_Axis (0),  x_Axis (1),  0.0,
                     y_Axis (0),  y_Axis (1),  0.0,
                            0.0,         0.0,  1.0);
}



void
b2d_Object_Spin_is (Object*       Self,
                    Matrix_3x3*   Now)
{
  b2Vec2     Pos   = b2Vec2 (Now->m20, Now->m21);
  float      Angle = atan2  (Now->m10, Now->m00);

  if (Self->body)
    {
      Self->body->SetTransform (Pos, Angle);
    }
  else
    {
      Self->bodyDef.position = Pos;
      Self->bodyDef.angle    = Angle;
    }
}



Real
b2d_Object_xy_Spin (Object*   Self)
{
  b2Vec2     x_Axis;
  b2Vec2     y_Axis;
  b2Rot      b2_Rotation;

  if (Self->body)
    {
      return Self->body->GetAngle();
    }
  else
    {
      return Self->bodyDef.angle;
    }
}



void b2d_Object_xy_Spin_is (Object*   Self,
                            Real      Now)
{
  if (Self->body)
    {
      Self->body->SetTransform (Self->body->GetPosition(),
                                Now);
    }
  else
    {
      Self->bodyDef.angle = Now;
    }
}



Matrix_4x4
b2d_Object_Transform (Object*   Self)
{
  b2Transform     T;
  Matrix_4x4      M;

  if (Self->body)
    {
      T = Self->body->GetTransform();
    }
  else
    {
      T = b2Transform (Self->bodyDef.position,
	                     b2Rot (Self->bodyDef.angle));
    }

  b2Vec2     x_Axis = T.q.GetXAxis();
  b2Vec2     y_Axis = T.q.GetYAxis();

  M.m00 = x_Axis (0);   M.m01 = x_Axis (1);   M.m02 = 0.0;   M.m03 = 0.0;
  M.m10 = y_Axis (0);   M.m11 = y_Axis (1);   M.m12 = 0.0;   M.m13 = 0.0;
  M.m20 =        0.0;   M.m21 =        0.0;   M.m22 = 1.0;   M.m23 = 0.0;
  M.m30 =    T.p (0);   M.m31 =    T.p (1);   M.m32 = 0.0;   M.m33 = 1.0;

  return M;
}



void
b2d_Object_Transform_is (Object*       Self,
                         Matrix_4x4*   Now)
{
  b2Vec2     Pos   = b2Vec2 (Now->m30, Now->m31);
  float      Angle = atan2  (Now->m10, Now->m00);

  if (Self->body)
    {
      Self->body->SetTransform (Pos, Angle);
    }
  else
    {
      Self->bodyDef.position = Pos;
      Self->bodyDef.angle    = Angle;
    }
}



Vector_3
b2d_Object_Speed (Object*   Self)
{
  Vector_3     the_Speed;

  if (Self->body)
    {
      b2Vec2     b2d_Speed = Self->body->GetLinearVelocity();

      the_Speed.x = b2d_Speed.x;
      the_Speed.y = b2d_Speed.y;
      the_Speed.z = 0.0;
    }
  else
    {
      the_Speed.x = 0.0;
      the_Speed.y = 0.0;
      the_Speed.z = 0.0;
    }

  return the_Speed;
}



void
b2d_Object_Speed_is (Object*     Self,
                     Vector_3*   Now)
{
  if (Self->body)
    {
      Self->body->SetLinearVelocity (b2Vec2 (Now->x, Now->y));
    }
}



Vector_3
b2d_Object_Gyre (Object*   Self)
{
  Vector_3     the_Gyre;     // TODO

  printf ("TODO: b2d_Object_Gyre");
  return the_Gyre;
}



void
b2d_Object_Gyre_is (Object*     Self,
                    Vector_3*   Now)
{
  printf ("TODO: b2d_Object_Gyre_is");
}



void
b2d_Object_apply_Torque (Object*     Self,
                         Vector_3*   Torque)
{
  printf ("TODO: b2d_Object_apply_Torque");
}



void
b2d_Object_apply_Torque_impulse (Object*     Self,
                                 Vector_3*   Torque)
{
  printf ("TODO: b2d_Object_apply_Torque_impulse");
}



void
b2d_Object_apply_Force (Object*     Self,
                        Vector_3*   Force)
{
  Self->body->ApplyForceToCenter (b2Vec2 (Force->x, Force->y), 1);
}



void
b2d_dump (Object*   Self)
{
  Self->body->Dump();
}


} // end extern "C"
