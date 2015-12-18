#ifndef C_BULLET_SHAPE_H
#define C_BULLET_SHAPE_H

#include "bullet.h"



extern "C"
{
  struct Shape;

  Shape*     b3d_new_Box           (Vector_3*    half_Extents);

  Shape*     b3d_new_Capsule       (Vector_2*    Radii,
                                    Real         Height);

  Shape*     b3d_new_Cone          (Real         Radius,
                                    Real         Height);

  Shape*     b3d_new_convex_Hull   (Vector_3     Points[],
                                    int          point_Count);

  Shape*     b3d_new_Mesh          (Vector_3     Points[],
				    int          point_Count,
				    Triangle     Triangles[],
                                    int          triangle_Count);

  Shape*     b3d_new_Cylinder      (Vector_3*    half_Extents);

  Shape*     b3d_new_Heightfield   (int          Width,
                                    int          Depth,
                                    Real*        Heights,
                                    Real         min_Height,
                                    Real         max_Height,
                                    Vector_3*    Scale);

  Shape*     b3d_new_multiSphere   (Vector_3     Positions[],
                                    Real*        Radii,
                                    int          sphere_Count);

  Shape*     b3d_new_Plane         (Vector_3*    Normal,
                                    Real         Offset);

  Shape*     b3d_new_Sphere        (Real         Radius);


  void*      b3d_Shape_user_Data      (Shape*   Self);
  void       b3d_Shape_user_Data_is   (Shape*   Self,   void*   Now);


}  // extern "C"


#endif
