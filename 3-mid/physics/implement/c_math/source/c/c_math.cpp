#include "c_math.h"


  Matrix_3x3::
  Matrix_3x3 (Real*    First) : m00 (First[0]),  m01 (First[1]),  m02 (First[2]),
                                m10 (First[3]),  m11 (First[4]),  m12 (First[5]),
                                m20 (First[6]),  m21 (First[7]),  m22 (First[8])
  {};


  Matrix_3x3::
  Matrix_3x3 (Real     m00,  Real   m01,  Real   m02,
              Real     m10,  Real   m11,  Real   m12,
              Real     m20,  Real   m21,  Real   m22) : m00 (m00),  m01 (m01),  m02 (m02),
                                                        m10 (m10),  m11 (m11),  m12 (m12),
                                                        m20 (m20),  m21 (m21),  m22 (m22)
  {};


  Matrix_4x4::
  Matrix_4x4 (Real*    First) : m00 (First[ 0]),  m01 (First[ 1]),  m02 (First[ 2]),  m03 (First[ 3]),
                                m10 (First[ 4]),  m11 (First[ 5]),  m12 (First[ 6]),  m13 (First[ 7]),
                                m20 (First[ 8]),  m21 (First[ 9]),  m22 (First[10]),  m23 (First[11]),
                                m30 (First[12]),  m31 (First[13]),  m32 (First[14]),  m33 (First[15])
  {};

