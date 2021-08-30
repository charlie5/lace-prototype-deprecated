#ifndef C_MATH_H
#define C_MATH_H


//
/// Provides a simple C++ interface to math structures used by the simple Box2D and Bullet3D C interfaces.
//

extern "C"
{

  typedef float   Real;



  struct Vector_2
  {
    Vector_2 () {};
    Vector_2 (Real x, Real y) : x (x),  y (y)   {};


    Real   x, y;
  };


  struct Vector_3
  {
    Vector_3 () {};
    Vector_3 (Real x, Real y, Real z) : x (x),  y (y),  z (z)   {};


    Real   x, y, z;
  };


  typedef int   Index;

  struct Triangle
  {
    Triangle () {};
    Triangle (Real a, Real b, Real c) : a (a),  b (b),  c (c)   {};


    Index   a, b, c;
  };




  struct Matrix_3x3
  {
    Matrix_3x3 () {};
    Matrix_3x3 (Real*    First);
    Matrix_3x3 (Real     m00,  Real   m01,  Real   m02,
                Real     m10,  Real   m11,  Real   m12,
                Real     m20,  Real   m21,  Real   m22);

    Real   m00,  m01,  m02,
           m10,  m11,  m12,
           m20,  m21,  m22;
  };



  struct Matrix_4x4
  {
    Matrix_4x4 () {};
    Matrix_4x4 (Real*    First);

    Real   m00,  m01,  m02,  m03,
           m10,  m11,  m12,  m13,
           m20,  m21,  m22,  m23,
           m30,  m31,  m32,  m33;
  };

}

#endif
