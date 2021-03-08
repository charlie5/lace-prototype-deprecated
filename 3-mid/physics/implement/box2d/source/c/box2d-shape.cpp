#include "box2d-shape.h"
#include "box2d-space.h"

#include <box2d/box2d.h>



extern "C"
{

/////////
//  Forge
//


void
b2d_free_Shape (Shape*       Self)
{
  b2Shape*   the_Shape = (b2Shape*) Self;

  delete (the_Shape);
}




Shape*
b2d_new_Circle (Real   Radius)
{
  b2CircleShape*   Self = new b2CircleShape();

  Self->m_radius = Radius;
  return (Shape*) Self;
}



Shape*
b2d_new_Polygon (Vector_2   Vertices[],
                 int        vertex_Count)
{
  b2PolygonShape*   Self = new b2PolygonShape();
  b2Vec2            Verts [vertex_Count];

  for (int i = 0;  i < vertex_Count;  i++)
    {
      Verts [i] = b2Vec2 (Vertices [i].x,
			  Vertices [i].y);
    }

    //  Self->Set (Verts, vertex_Count);
    Self->SetAsBox (Verts [2].x, Verts [2].y);

  return (Shape*) Self;
}




Shape*
b2d_new_Box (Vector_3*   half_Extents)
{
  return 0;
}



Shape*
b2d_new_Capsule (Vector_2*   Radii,
                 Real        Height)
{
  return 0;
}



Shape*
b2d_new_Cone (Real   Radius,
              Real   Height)
{
  return 0;
}



Shape*
b2d_new_convex_Hull (Vector_3     Points[],
                     int          point_Count)
{
  return 0;
}



Shape*
b2d_new_Cylinder (Vector_3*   half_Extents)
{
  return 0;
}




Shape*
b2d_new_Heightfield (int         Width,
                     int         Depth,
                     Real        Heights[],
                     Real        min_Height,
                     Real        max_Height,
                     Vector_3*   Scale)
{
  return 0;
}




Shape*
b2d_new_multiSphere (Vector_3*   Positions,
                     Real*       Radii,
                     int         sphere_Count)
{
  return 0;
}




Shape*
b2d_new_Plane (Vector_3*   Normal,
               Real       Offset)
{
  return 0;
}



Shape*
b2d_new_Sphere (Real   Radius)
{
  Shape*   Self = 0;
  return Self;
}




//////////////
//  Attributes
//

void
b2d_shape_Scale_is (Shape*   Self,   Vector_2   Now)
{
  return;

  b2Shape*   the_Shape = (b2Shape*) Self;

  if (the_Shape->GetType() == b2Shape::e_circle)
    {
       the_Shape->m_radius = Now.x / 2.0;
    }
  else if (the_Shape->GetType() == b2Shape::e_polygon)
    {
      // todo
    }
}




void*
b2d_Shape_user_Data      (Shape*   Self)
{
  return 0;
}


void
b2d_Shape_user_Data_is   (Shape*   Self,   void*   Now)
{
  // todo
}


} // extern "C"
