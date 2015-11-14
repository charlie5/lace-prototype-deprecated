#ifndef C_BOX2D_SHAPE_H
#define C_BOX2D_SHAPE_H

#include "box2d.h"



extern "C"
{
  struct Shape;

  Shape*     b2d_new_Circle         (Real         Radius);
  Shape*     b2d_new_Polygon        (Vector_2     Vertices[],
                                     int          vertex_Count);
  Shape*     b2d_new_Box            (Vector_3*    half_Extents);

  Shape*     b2d_new_Capsule        (Vector_2*    Radii,
                                     Real         Height);
  Shape*     b2d_new_Cone           (Real         Radius,
                                     Real         Height);
  Shape*     b2d_new_convex_Hull    (Vector_3     Points[],
                                     int          point_Count);
  Shape*     b2d_new_Cylinder       (Vector_3*    half_Extents);
  Shape*     b2d_new_Heightfield    (int          Width,
                                     int          Depth,
                                     Real*        Heights,
                                     Real         min_Height,
                                     Real         max_Height,
                                     Vector_3*    Scale);
  Shape*     b2d_new_multiSphere    (Vector_3     Positions[],
                                     Real*        Radii,
                                     int          sphere_Count);
  Shape*     b2d_new_Plane          (Vector_3*    Normal,
                                     Real         Offset);
  Shape*     b2d_new_Sphere         (Real         Radius);


  void       b2d_free_Shape         (Shape*       Self);


  void*      b2d_Shape_user_Data    (Shape*       Self);
  void       b2d_Shape_user_Data_is (Shape*       Self,   void*   Now);

  void       b2d_shape_Scale_is     (Shape*       Self,   Vector_2   Now);


} // extern "C"

#endif





