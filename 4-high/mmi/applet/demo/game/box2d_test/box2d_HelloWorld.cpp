#include <box2d/box2d.h>
#include <stdio.h>

int main(int argc, char** argv)
{
  B2_NOT_USED(argc);
  B2_NOT_USED(argv);

  // Construct a world object, which will hold and simulate the rigid bodies.
  b2World world (b2Vec2 (0.0f, 0.0f));      // Does not bounce.
//  b2World world (b2Vec2 (0.0f, 0.017f));    // Bounces.

  // Define the dynamic ball body. We set its position and call the body factory.
  b2BodyDef bodyDef;
  bodyDef.type = b2_dynamicBody;
  bodyDef.position.Set(0.0f, 0.0f);
  b2Body* body = world.CreateBody(&bodyDef);

  // Define a ball shape for our dynamic body.
  b2CircleShape Ball;
  Ball.m_radius = 0.5f;

  // Define the dynamic body fixture.
  b2FixtureDef fixtureDef;

  fixtureDef.shape = &Ball;
  fixtureDef.density = 1.0f;
  fixtureDef.friction = 0.0f;
  fixtureDef.restitution = 1.0f;

  // Add the shape to the body.
  body->CreateFixture(&fixtureDef);


  // Define the statoc box body.

  float   stadium_Width     = 300.0;
  float   stadium_Height    = 10.0;
  float   Thickness         =  2.0;
  float   top_wall_Y_Offset = stadium_Height + (Thickness / 2.0);

  // Define the ground box shape.
  b2PolygonShape groundBox;
  groundBox.SetAsBox(stadium_Width, Thickness);

  b2BodyDef groundBodyDef;
  groundBodyDef.type = b2_staticBody;
  groundBodyDef.position.Set (0.0f, top_wall_Y_Offset);
  b2Body* groundBody = world.CreateBody(&groundBodyDef);

  b2FixtureDef GfixtureDef;
  GfixtureDef.shape = &groundBox;
  GfixtureDef.density = 0.0f;
  GfixtureDef.friction = 0.0f;
  GfixtureDef.restitution = 1.0f;
  groundBody->CreateFixture(&GfixtureDef);

  body->ApplyForceToCenter(b2Vec2 (45.0, 40.0), 1);

  float   timeStep = 1.0f / 60.0f;
  int32   velocityIterations = 6;
  int32   positionIterations = 2;

  for (int32 i = 0; i < 700; ++i)
    {
      world.Step(timeStep, velocityIterations, positionIterations);

      b2Vec2   position = body->GetPosition();
      float    angle    = body->GetAngle();

      printf("%d   %4.2f %4.2f    %4.2f\n", i, position.x, position.y, angle);
    }

  return 0;
}
