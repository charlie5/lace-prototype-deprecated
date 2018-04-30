/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 1.3.36
 * 
 * This file is not intended to be easily readable and contains a number of 
 * coding conventions designed to improve portability and efficiency. Do not make
 * changes to this file unless you know what you are doing--modify the SWIG 
 * interface file instead. 
 * ----------------------------------------------------------------------------- */


#ifdef __cplusplus
template<typename T> class SwigValueWrapper {
    T *tt;
public:
    SwigValueWrapper() : tt(0) { }
    SwigValueWrapper(const SwigValueWrapper<T>& rhs) : tt(new T(*rhs.tt)) { }
    SwigValueWrapper(const T& t) : tt(new T(t)) { }
    ~SwigValueWrapper() { delete tt; } 
    SwigValueWrapper& operator=(const T& t) { delete tt; tt = new T(t); return *this; }
    operator T&() const { return *tt; }
    T *operator&() { return tt; }
private:
    SwigValueWrapper& operator=(const SwigValueWrapper<T>& rhs);
};

template <typename T> T SwigValueInit() {
  return T();
}
#endif

/* -----------------------------------------------------------------------------
 *  This section contains generic SWIG labels for method/variable
 *  declarations/attributes, and other compiler dependent labels.
 * ----------------------------------------------------------------------------- */

/* template workaround for compilers that cannot correctly implement the C++ standard */
#ifndef SWIGTEMPLATEDISAMBIGUATOR
# if defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x560)
#  define SWIGTEMPLATEDISAMBIGUATOR template
# elif defined(__HP_aCC)
/* Needed even with `aCC -AA' when `aCC -V' reports HP ANSI C++ B3910B A.03.55 */
/* If we find a maximum version that requires this, the test would be __HP_aCC <= 35500 for A.03.55 */
#  define SWIGTEMPLATEDISAMBIGUATOR template
# else
#  define SWIGTEMPLATEDISAMBIGUATOR
# endif
#endif

/* inline attribute */
#ifndef SWIGINLINE
# if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#   define SWIGINLINE inline
# else
#   define SWIGINLINE
# endif
#endif

/* attribute recognised by some compilers to avoid 'unused' warnings */
#ifndef SWIGUNUSED
# if defined(__GNUC__)
#   if !(defined(__cplusplus)) || (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4))
#     define SWIGUNUSED __attribute__ ((__unused__)) 
#   else
#     define SWIGUNUSED
#   endif
# elif defined(__ICC)
#   define SWIGUNUSED __attribute__ ((__unused__)) 
# else
#   define SWIGUNUSED 
# endif
#endif

#ifndef SWIGUNUSEDPARM
# ifdef __cplusplus
#   define SWIGUNUSEDPARM(p)
# else
#   define SWIGUNUSEDPARM(p) p SWIGUNUSED 
# endif
#endif

/* internal SWIG method */
#ifndef SWIGINTERN
# define SWIGINTERN static SWIGUNUSED
#endif

/* internal inline SWIG method */
#ifndef SWIGINTERNINLINE
# define SWIGINTERNINLINE SWIGINTERN SWIGINLINE
#endif

/* exporting methods */
#if (__GNUC__ >= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#  ifndef GCC_HASCLASSVISIBILITY
#    define GCC_HASCLASSVISIBILITY
#  endif
#endif

#ifndef SWIGEXPORT
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   if defined(STATIC_LINKED)
#     define SWIGEXPORT
#   else
#     define SWIGEXPORT __declspec(dllexport)
#   endif
# else
#   if defined(__GNUC__) && defined(GCC_HASCLASSVISIBILITY)
#     define SWIGEXPORT __attribute__ ((visibility("default")))
#   else
#     define SWIGEXPORT
#   endif
# endif
#endif

/* calling conventions for Windows */
#ifndef SWIGSTDCALL
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   define SWIGSTDCALL __stdcall
# else
#   define SWIGSTDCALL
# endif 
#endif

/* Deal with Microsoft's attempt at deprecating C standard runtime functions */
#if !defined(SWIG_NO_CRT_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_CRT_SECURE_NO_DEPRECATE)
# define _CRT_SECURE_NO_DEPRECATE
#endif

/* Deal with Microsoft's attempt at deprecating methods in the standard C++ library */
#if !defined(SWIG_NO_SCL_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_SCL_SECURE_NO_DEPRECATE)
# define _SCL_SECURE_NO_DEPRECATE
#endif



#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#if defined(_WIN32) || defined(__CYGWIN32__)
#  define DllExport   __declspec( dllexport )
#  define SWIGSTDCALL __stdcall
#else
#  define DllExport  
#  define SWIGSTDCALL
#endif 

#ifdef __cplusplus
#  include <new>
#endif





#define protected public
#define private   public

extern "C"
{
   #include "../c/box2d-shape.h"
   #include "../c/box2d-object.h"
   #include "../c/box2d-joint.h"
   #include "../c/box2d-space.h"
}

   
      

#undef protected
#undef private
#ifdef __cplusplus 
extern "C" {
#endif
DllExport void * SWIGSTDCALL Ada_b2d_new_Circle (
  float jarg1
  )
{
  void * jresult ;
  Real arg1 ;
  Shape *result = 0 ;
  
  
  arg1 = (Real) jarg1; 
  
  
  result = (Shape *)b2d_new_Circle(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Polygon (
  void * jarg1
  ,
  
  int jarg2
  )
{
  void * jresult ;
  Vector_2 *arg1 ;
  int arg2 ;
  Shape *result = 0 ;
  
  arg1      = (Vector_2 *) jarg1; 
  
  
  arg2 = (int) jarg2; 
  
  
  result = (Shape *)b2d_new_Polygon(arg1,arg2);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Box (
  void * jarg1
  )
{
  void * jresult ;
  Vector_3 *arg1 = (Vector_3 *) 0 ;
  Shape *result = 0 ;
  
  arg1 = (Vector_3 *)jarg1; 
  
  result = (Shape *)b2d_new_Box(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Capsule (
  void * jarg1
  ,
  
  float jarg2
  )
{
  void * jresult ;
  Vector_2 *arg1 = (Vector_2 *) 0 ;
  Real arg2 ;
  Shape *result = 0 ;
  
  arg1 = (Vector_2 *)jarg1; 
  
  
  arg2 = (Real) jarg2; 
  
  
  result = (Shape *)b2d_new_Capsule(arg1,arg2);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Cone (
  float jarg1
  ,
  
  float jarg2
  )
{
  void * jresult ;
  Real arg1 ;
  Real arg2 ;
  Shape *result = 0 ;
  
  
  arg1 = (Real) jarg1; 
  
  
  
  arg2 = (Real) jarg2; 
  
  
  result = (Shape *)b2d_new_Cone(arg1,arg2);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_convex_Hull (
  void * jarg1
  ,
  
  int jarg2
  )
{
  void * jresult ;
  Vector_3 *arg1 ;
  int arg2 ;
  Shape *result = 0 ;
  
  arg1      = (Vector_3 *) jarg1; 
  
  
  arg2 = (int) jarg2; 
  
  
  result = (Shape *)b2d_new_convex_Hull(arg1,arg2);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Cylinder (
  void * jarg1
  )
{
  void * jresult ;
  Vector_3 *arg1 = (Vector_3 *) 0 ;
  Shape *result = 0 ;
  
  arg1 = (Vector_3 *)jarg1; 
  
  result = (Shape *)b2d_new_Cylinder(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Heightfield (
  int jarg1
  ,
  
  int jarg2
  ,
  
  float* jarg3
  ,
  
  float jarg4
  ,
  
  float jarg5
  ,
  
  void * jarg6
  )
{
  void * jresult ;
  int arg1 ;
  int arg2 ;
  Real *arg3 = (Real *) 0 ;
  Real arg4 ;
  Real arg5 ;
  Vector_3 *arg6 = (Vector_3 *) 0 ;
  Shape *result = 0 ;
  
  
  arg1 = (int) jarg1; 
  
  
  
  arg2 = (int) jarg2; 
  
  
  
  arg3 = (Real *) jarg3;
  
  
  
  arg4 = (Real) jarg4; 
  
  
  
  arg5 = (Real) jarg5; 
  
  
  arg6 = (Vector_3 *)jarg6; 
  
  result = (Shape *)b2d_new_Heightfield(arg1,arg2,arg3,arg4,arg5,arg6);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_multiSphere (
  void * jarg1
  ,
  
  float* jarg2
  ,
  
  int jarg3
  )
{
  void * jresult ;
  Vector_3 *arg1 ;
  Real *arg2 = (Real *) 0 ;
  int arg3 ;
  Shape *result = 0 ;
  
  arg1      = (Vector_3 *) jarg1; 
  
  
  arg2 = (Real *) jarg2;
  
  
  
  arg3 = (int) jarg3; 
  
  
  result = (Shape *)b2d_new_multiSphere(arg1,arg2,arg3);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Plane (
  void * jarg1
  ,
  
  float jarg2
  )
{
  void * jresult ;
  Vector_3 *arg1 = (Vector_3 *) 0 ;
  Real arg2 ;
  Shape *result = 0 ;
  
  arg1 = (Vector_3 *)jarg1; 
  
  
  arg2 = (Real) jarg2; 
  
  
  result = (Shape *)b2d_new_Plane(arg1,arg2);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Sphere (
  float jarg1
  )
{
  void * jresult ;
  Real arg1 ;
  Shape *result = 0 ;
  
  
  arg1 = (Real) jarg1; 
  
  
  result = (Shape *)b2d_new_Sphere(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_free_Shape (
  void * jarg1
  )
{
  Shape *arg1 = (Shape *) 0 ;
  
  arg1 = (Shape *)jarg1; 
  
  b2d_free_Shape(arg1);
  
  
}



DllExport void* SWIGSTDCALL Ada_b2d_Shape_user_Data (
  void * jarg1
  )
{
  void* jresult ;
  Shape *arg1 = (Shape *) 0 ;
  void *result = 0 ;
  
  arg1 = (Shape *)jarg1; 
  
  result = (void *)b2d_Shape_user_Data(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Shape_user_Data_is (
  void * jarg1
  ,
  
  void* jarg2
  )
{
  Shape *arg1 = (Shape *) 0 ;
  void *arg2 = (void *) 0 ;
  
  arg1 = (Shape *)jarg1; 
  
  arg2 = (void *)jarg2; 
  
  b2d_Shape_user_Data_is(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_shape_Scale_is (
  void * jarg1
  ,
  
  Vector_2 jarg2
  )
{
  Shape *arg1 = (Shape *) 0 ;
  Vector_2 arg2 ;
  Vector_2 *argp2 ;
  
  arg1 = (Shape *)jarg1; 
  
  
  argp2 = (Vector_2 *) &jarg2; 
  
  arg2 = *argp2; 
  
  
  b2d_shape_Scale_is(arg1,arg2);
  
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Object (
  void * jarg1
  ,
  
  float jarg2
  ,
  
  float jarg3
  ,
  
  float jarg4
  ,
  
  void * jarg5
  )
{
  void * jresult ;
  Vector_2 *arg1 = (Vector_2 *) 0 ;
  Real arg2 ;
  Real arg3 ;
  Real arg4 ;
  Shape *arg5 = (Shape *) 0 ;
  Object *result = 0 ;
  
  arg1 = (Vector_2 *)jarg1; 
  
  
  arg2 = (Real) jarg2; 
  
  
  
  arg3 = (Real) jarg3; 
  
  
  
  arg4 = (Real) jarg4; 
  
  
  arg5 = (Shape *)jarg5; 
  
  result = (Object *)b2d_new_Object(arg1,arg2,arg3,arg4,arg5);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_free_Object (
  void * jarg1
  )
{
  Object *arg1 = (Object *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  b2d_free_Object(arg1);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_Scale_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Vector_2 *arg2 = (Vector_2 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Vector_2 *)jarg2; 
  
  b2d_Object_Scale_is(arg1,arg2);
  
  
}



DllExport void * SWIGSTDCALL Ada_b2d_Object_Shape (
  void * jarg1
  )
{
  void * jresult ;
  Object *arg1 = (Object *) 0 ;
  Shape *result = 0 ;
  
  arg1 = (Object *)jarg1; 
  
  result = (Shape *)b2d_Object_Shape(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void* SWIGSTDCALL Ada_b2d_Object_user_Data (
  void * jarg1
  )
{
  void* jresult ;
  Object *arg1 = (Object *) 0 ;
  void *result = 0 ;
  
  arg1 = (Object *)jarg1; 
  
  result = (void *)b2d_Object_user_Data(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_user_Data_is (
  void * jarg1
  ,
  
  void* jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  void *arg2 = (void *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (void *)jarg2; 
  
  b2d_Object_user_Data_is(arg1,arg2);
  
  
}



DllExport float SWIGSTDCALL Ada_b2d_Object_Mass (
  void * jarg1
  )
{
  float jresult ;
  Object *arg1 = (Object *) 0 ;
  Real result;
  
  arg1 = (Object *)jarg1; 
  
  result = (Real)b2d_Object_Mass(arg1);
  jresult = result; 
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_Friction_is (
  void * jarg1
  ,
  
  float jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Real arg2 ;
  
  arg1 = (Object *)jarg1; 
  
  
  arg2 = (Real) jarg2; 
  
  
  b2d_Object_Friction_is(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_Restitution_is (
  void * jarg1
  ,
  
  float jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Real arg2 ;
  
  arg1 = (Object *)jarg1; 
  
  
  arg2 = (Real) jarg2; 
  
  
  b2d_Object_Restitution_is(arg1,arg2);
  
  
}



DllExport Vector_3 SWIGSTDCALL Ada_b2d_Object_Site (
  void * jarg1
  )
{
  Vector_3 jresult ;
  Object *arg1 = (Object *) 0 ;
  Vector_3 result;
  
  arg1 = (Object *)jarg1; 
  
  result = b2d_Object_Site(arg1);
  
  jresult = result; 
  //jresult = new Vector_3 ((Vector_3 &) result); 
  
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_Site_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Vector_3 *arg2 = (Vector_3 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Vector_3 *)jarg2; 
  
  b2d_Object_Site_is(arg1,arg2);
  
  
}



DllExport Matrix_3x3 SWIGSTDCALL Ada_b2d_Object_Spin (
  void * jarg1
  )
{
  Matrix_3x3 jresult ;
  Object *arg1 = (Object *) 0 ;
  Matrix_3x3 result;
  
  arg1 = (Object *)jarg1; 
  
  result = b2d_Object_Spin(arg1);
  
  jresult = result; 
  //jresult = new Matrix_3x3 ((Matrix_3x3 &) result); 
  
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_Spin_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Matrix_3x3 *arg2 = (Matrix_3x3 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Matrix_3x3 *)jarg2; 
  
  b2d_Object_Spin_is(arg1,arg2);
  
  
}



DllExport float SWIGSTDCALL Ada_b2d_Object_xy_Spin (
  void * jarg1
  )
{
  float jresult ;
  Object *arg1 = (Object *) 0 ;
  Real result;
  
  arg1 = (Object *)jarg1; 
  
  result = (Real)b2d_Object_xy_Spin(arg1);
  jresult = result; 
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_xy_Spin_is (
  void * jarg1
  ,
  
  float jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Real arg2 ;
  
  arg1 = (Object *)jarg1; 
  
  
  arg2 = (Real) jarg2; 
  
  
  b2d_Object_xy_Spin_is(arg1,arg2);
  
  
}



DllExport Matrix_4x4 SWIGSTDCALL Ada_b2d_Object_Transform (
  void * jarg1
  )
{
  Matrix_4x4 jresult ;
  Object *arg1 = (Object *) 0 ;
  Matrix_4x4 result;
  
  arg1 = (Object *)jarg1; 
  
  result = b2d_Object_Transform(arg1);
  
  jresult = result; 
  //jresult = new Matrix_4x4 ((Matrix_4x4 &) result); 
  
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_Transform_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Matrix_4x4 *arg2 = (Matrix_4x4 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Matrix_4x4 *)jarg2; 
  
  b2d_Object_Transform_is(arg1,arg2);
  
  
}



DllExport Vector_3 SWIGSTDCALL Ada_b2d_Object_Speed (
  void * jarg1
  )
{
  Vector_3 jresult ;
  Object *arg1 = (Object *) 0 ;
  Vector_3 result;
  
  arg1 = (Object *)jarg1; 
  
  result = b2d_Object_Speed(arg1);
  
  jresult = result; 
  //jresult = new Vector_3 ((Vector_3 &) result); 
  
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_Speed_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Vector_3 *arg2 = (Vector_3 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Vector_3 *)jarg2; 
  
  b2d_Object_Speed_is(arg1,arg2);
  
  
}



DllExport Vector_3 SWIGSTDCALL Ada_b2d_Object_Gyre (
  void * jarg1
  )
{
  Vector_3 jresult ;
  Object *arg1 = (Object *) 0 ;
  Vector_3 result;
  
  arg1 = (Object *)jarg1; 
  
  result = b2d_Object_Gyre(arg1);
  
  jresult = result; 
  //jresult = new Vector_3 ((Vector_3 &) result); 
  
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_Gyre_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Vector_3 *arg2 = (Vector_3 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Vector_3 *)jarg2; 
  
  b2d_Object_Gyre_is(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_apply_Force (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Vector_3 *arg2 = (Vector_3 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Vector_3 *)jarg2; 
  
  b2d_Object_apply_Force(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_apply_Torque (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Vector_3 *arg2 = (Vector_3 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Vector_3 *)jarg2; 
  
  b2d_Object_apply_Torque(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Object_apply_Torque_impulse (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Object *arg1 = (Object *) 0 ;
  Vector_3 *arg2 = (Vector_3 *) 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Vector_3 *)jarg2; 
  
  b2d_Object_apply_Torque_impulse(arg1,arg2);
  
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_hinge_Joint_with_local_anchors (
  void * jarg1
  ,
  
  void * jarg2
  ,
  
  void * jarg3
  ,
  
  void * jarg4
  ,
  
  void * jarg5
  ,
  
  float jarg6
  ,
  
  float jarg7
  ,
  
  unsigned int jarg8
  )
{
  void * jresult ;
  Space *arg1 = (Space *) 0 ;
  Object *arg2 = (Object *) 0 ;
  Object *arg3 = (Object *) 0 ;
  Vector_3 *arg4 = (Vector_3 *) 0 ;
  Vector_3 *arg5 = (Vector_3 *) 0 ;
  float arg6 ;
  float arg7 ;
  bool arg8 ;
  Joint *result = 0 ;
  
  arg1 = (Space *)jarg1; 
  
  arg2 = (Object *)jarg2; 
  
  arg3 = (Object *)jarg3; 
  
  arg4 = (Vector_3 *)jarg4; 
  
  arg5 = (Vector_3 *)jarg5; 
  
  
  arg6 = (float) jarg6; 
  
  
  
  arg7 = (float) jarg7; 
  
  
  
  arg8 = jarg8 ? true : false; 
  
  
  result = (Joint *)b2d_new_hinge_Joint_with_local_anchors(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_hinge_Joint (
  void * jarg1
  ,
  
  void * jarg2
  ,
  
  void * jarg3
  ,
  
  void * jarg4
  ,
  
  void * jarg5
  ,
  
  float jarg6
  ,
  
  float jarg7
  ,
  
  unsigned int jarg8
  )
{
  void * jresult ;
  Space *arg1 = (Space *) 0 ;
  Object *arg2 = (Object *) 0 ;
  Object *arg3 = (Object *) 0 ;
  Matrix_4x4 *arg4 = (Matrix_4x4 *) 0 ;
  Matrix_4x4 *arg5 = (Matrix_4x4 *) 0 ;
  float arg6 ;
  float arg7 ;
  bool arg8 ;
  Joint *result = 0 ;
  
  arg1 = (Space *)jarg1; 
  
  arg2 = (Object *)jarg2; 
  
  arg3 = (Object *)jarg3; 
  
  arg4 = (Matrix_4x4 *)jarg4; 
  
  arg5 = (Matrix_4x4 *)jarg5; 
  
  
  arg6 = (float) jarg6; 
  
  
  
  arg7 = (float) jarg7; 
  
  
  
  arg8 = jarg8 ? true : false; 
  
  
  result = (Joint *)b2d_new_hinge_Joint(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_free_hinge_Joint (
  void * jarg1
  )
{
  Joint *arg1 = (Joint *) 0 ;
  
  arg1 = (Joint *)jarg1; 
  
  b2d_free_hinge_Joint(arg1);
  
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_space_hinge_Joint (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  void * jresult ;
  Object *arg1 = (Object *) 0 ;
  Matrix_4x4 *arg2 = (Matrix_4x4 *) 0 ;
  Joint *result = 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Matrix_4x4 *)jarg2; 
  
  result = (Joint *)b2d_new_space_hinge_Joint(arg1,arg2);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_DoF6_Joint (
  void * jarg1
  ,
  
  void * jarg2
  ,
  
  void * jarg3
  ,
  
  void * jarg4
  )
{
  void * jresult ;
  Object *arg1 = (Object *) 0 ;
  Object *arg2 = (Object *) 0 ;
  Matrix_4x4 *arg3 = (Matrix_4x4 *) 0 ;
  Matrix_4x4 *arg4 = (Matrix_4x4 *) 0 ;
  Joint *result = 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Object *)jarg2; 
  
  arg3 = (Matrix_4x4 *)jarg3; 
  
  arg4 = (Matrix_4x4 *)jarg4; 
  
  result = (Joint *)b2d_new_DoF6_Joint(arg1,arg2,arg3,arg4);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_cone_twist_Joint (
  void * jarg1
  ,
  
  void * jarg2
  ,
  
  void * jarg3
  ,
  
  void * jarg4
  )
{
  void * jresult ;
  Object *arg1 = (Object *) 0 ;
  Object *arg2 = (Object *) 0 ;
  Matrix_4x4 *arg3 = (Matrix_4x4 *) 0 ;
  Matrix_4x4 *arg4 = (Matrix_4x4 *) 0 ;
  Joint *result = 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Object *)jarg2; 
  
  arg3 = (Matrix_4x4 *)jarg3; 
  
  arg4 = (Matrix_4x4 *)jarg4; 
  
  result = (Joint *)b2d_new_cone_twist_Joint(arg1,arg2,arg3,arg4);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_slider_Joint (
  void * jarg1
  ,
  
  void * jarg2
  ,
  
  void * jarg3
  ,
  
  void * jarg4
  )
{
  void * jresult ;
  Object *arg1 = (Object *) 0 ;
  Object *arg2 = (Object *) 0 ;
  Matrix_4x4 *arg3 = (Matrix_4x4 *) 0 ;
  Matrix_4x4 *arg4 = (Matrix_4x4 *) 0 ;
  Joint *result = 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Object *)jarg2; 
  
  arg3 = (Matrix_4x4 *)jarg3; 
  
  arg4 = (Matrix_4x4 *)jarg4; 
  
  result = (Joint *)b2d_new_slider_Joint(arg1,arg2,arg3,arg4);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_ball_Joint (
  void * jarg1
  ,
  
  void * jarg2
  ,
  
  void * jarg3
  ,
  
  void * jarg4
  )
{
  void * jresult ;
  Object *arg1 = (Object *) 0 ;
  Object *arg2 = (Object *) 0 ;
  Vector_3 *arg3 = (Vector_3 *) 0 ;
  Vector_3 *arg4 = (Vector_3 *) 0 ;
  Joint *result = 0 ;
  
  arg1 = (Object *)jarg1; 
  
  arg2 = (Object *)jarg2; 
  
  arg3 = (Vector_3 *)jarg3; 
  
  arg4 = (Vector_3 *)jarg4; 
  
  result = (Joint *)b2d_new_ball_Joint(arg1,arg2,arg3,arg4);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void* SWIGSTDCALL Ada_b2d_Joint_user_Data (
  void * jarg1
  )
{
  void* jresult ;
  Joint *arg1 = (Joint *) 0 ;
  void *result = 0 ;
  
  arg1 = (Joint *)jarg1; 
  
  result = (void *)b2d_Joint_user_Data(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Joint_user_Data_is (
  void * jarg1
  ,
  
  void* jarg2
  )
{
  Joint *arg1 = (Joint *) 0 ;
  void *arg2 = (void *) 0 ;
  
  arg1 = (Joint *)jarg1; 
  
  arg2 = (void *)jarg2; 
  
  b2d_Joint_user_Data_is(arg1,arg2);
  
  
}



DllExport void * SWIGSTDCALL Ada_b2d_Joint_Object_A (
  void * jarg1
  )
{
  void * jresult ;
  Joint *arg1 = (Joint *) 0 ;
  Object *result = 0 ;
  
  arg1 = (Joint *)jarg1; 
  
  result = (Object *)b2d_Joint_Object_A(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_b2d_Joint_Object_B (
  void * jarg1
  )
{
  void * jresult ;
  Joint *arg1 = (Joint *) 0 ;
  Object *result = 0 ;
  
  arg1 = (Joint *)jarg1; 
  
  result = (Object *)b2d_Joint_Object_B(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport Matrix_4x4 SWIGSTDCALL Ada_b2d_Joint_Frame_A (
  void * jarg1
  )
{
  Matrix_4x4 jresult ;
  Joint *arg1 = (Joint *) 0 ;
  Matrix_4x4 result;
  
  arg1 = (Joint *)jarg1; 
  
  result = b2d_Joint_Frame_A(arg1);
  
  jresult = result; 
  //jresult = new Matrix_4x4 ((Matrix_4x4 &) result); 
  
  
  
  
  return jresult;
  
}



DllExport Matrix_4x4 SWIGSTDCALL Ada_b2d_Joint_Frame_B (
  void * jarg1
  )
{
  Matrix_4x4 jresult ;
  Joint *arg1 = (Joint *) 0 ;
  Matrix_4x4 result;
  
  arg1 = (Joint *)jarg1; 
  
  result = b2d_Joint_Frame_B(arg1);
  
  jresult = result; 
  //jresult = new Matrix_4x4 ((Matrix_4x4 &) result); 
  
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Joint_Frame_A_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Joint *arg1 = (Joint *) 0 ;
  Matrix_4x4 *arg2 = (Matrix_4x4 *) 0 ;
  
  arg1 = (Joint *)jarg1; 
  
  arg2 = (Matrix_4x4 *)jarg2; 
  
  b2d_Joint_Frame_A_is(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Joint_Frame_B_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Joint *arg1 = (Joint *) 0 ;
  Matrix_4x4 *arg2 = (Matrix_4x4 *) 0 ;
  
  arg1 = (Joint *)jarg1; 
  
  arg2 = (Matrix_4x4 *)jarg2; 
  
  b2d_Joint_Frame_B_is(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Joint_set_local_Anchor (
  void * jarg1
  ,
  
  unsigned int jarg2
  ,
  
  void * jarg3
  )
{
  Joint *arg1 = (Joint *) 0 ;
  bool arg2 ;
  Vector_3 *arg3 = (Vector_3 *) 0 ;
  
  arg1 = (Joint *)jarg1; 
  
  
  arg2 = jarg2 ? true : false; 
  
  
  arg3 = (Vector_3 *)jarg3; 
  
  b2d_Joint_set_local_Anchor(arg1,arg2,arg3);
  
  
}



DllExport unsigned int SWIGSTDCALL Ada_b2d_Joint_is_Limited (
  void * jarg1
  ,
  
  int jarg2
  )
{
  unsigned int jresult ;
  Joint *arg1 = (Joint *) 0 ;
  int arg2 ;
  bool result;
  
  arg1 = (Joint *)jarg1; 
  
  
  arg2 = (int) jarg2; 
  
  
  result = (bool)b2d_Joint_is_Limited(arg1,arg2);
  jresult = result; 
  
  
  
  return jresult;
  
}



DllExport unsigned int SWIGSTDCALL Ada_b2d_Joint_Extent (
  void * jarg1
  ,
  
  int jarg2
  )
{
  unsigned int jresult ;
  Joint *arg1 = (Joint *) 0 ;
  int arg2 ;
  bool result;
  
  arg1 = (Joint *)jarg1; 
  
  
  arg2 = (int) jarg2; 
  
  
  result = (bool)b2d_Joint_Extent(arg1,arg2);
  jresult = result; 
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Joint_Velocity_is (
  void * jarg1
  ,
  
  int jarg2
  ,
  
  float jarg3
  )
{
  Joint *arg1 = (Joint *) 0 ;
  int arg2 ;
  Real arg3 ;
  
  arg1 = (Joint *)jarg1; 
  
  
  arg2 = (int) jarg2; 
  
  
  
  arg3 = (Real) jarg3; 
  
  
  b2d_Joint_Velocity_is(arg1,arg2,arg3);
  
  
}



DllExport Vector_3 SWIGSTDCALL Ada_b2d_Joint_reaction_Force (
  void * jarg1
  )
{
  Vector_3 jresult ;
  Joint *arg1 = (Joint *) 0 ;
  Vector_3 result;
  
  arg1 = (Joint *)jarg1; 
  
  result = b2d_Joint_reaction_Force(arg1);
  
  jresult = result; 
  //jresult = new Vector_3 ((Vector_3 &) result); 
  
  
  
  
  return jresult;
  
}



DllExport float SWIGSTDCALL Ada_b2d_Joint_reaction_Torque (
  void * jarg1
  )
{
  float jresult ;
  Joint *arg1 = (Joint *) 0 ;
  Real result;
  
  arg1 = (Joint *)jarg1; 
  
  result = (Real)b2d_Joint_reaction_Torque(arg1);
  jresult = result; 
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Joint_hinge_Limits_are (
  void * jarg1
  ,
  
  float jarg2
  ,
  
  float jarg3
  )
{
  Joint *arg1 = (Joint *) 0 ;
  Real arg2 ;
  Real arg3 ;
  
  arg1 = (Joint *)jarg1; 
  
  
  arg2 = (Real) jarg2; 
  
  
  
  arg3 = (Real) jarg3; 
  
  
  b2d_Joint_hinge_Limits_are(arg1,arg2,arg3);
  
  
}



DllExport void * SWIGSTDCALL Ada_b2d_new_Space (
  )
{
  void * jresult ;
  Space *result = 0 ;
  
  result = (Space *)b2d_new_Space();
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_free_Space (
  void * jarg1
  )
{
  Space *arg1 = (Space *) 0 ;
  
  arg1 = (Space *)jarg1; 
  
  b2d_free_Space(arg1);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Space_add_Object (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Space *arg1 = (Space *) 0 ;
  Object *arg2 = (Object *) 0 ;
  
  arg1 = (Space *)jarg1; 
  
  arg2 = (Object *)jarg2; 
  
  b2d_Space_add_Object(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Space_rid_Object (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Space *arg1 = (Space *) 0 ;
  Object *arg2 = (Object *) 0 ;
  
  arg1 = (Space *)jarg1; 
  
  arg2 = (Object *)jarg2; 
  
  b2d_Space_rid_Object(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Space_add_Joint (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Space *arg1 = (Space *) 0 ;
  Joint *arg2 = (Joint *) 0 ;
  
  arg1 = (Space *)jarg1; 
  
  arg2 = (Joint *)jarg2; 
  
  b2d_Space_add_Joint(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Space_rid_Joint (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Space *arg1 = (Space *) 0 ;
  Joint *arg2 = (Joint *) 0 ;
  
  arg1 = (Space *)jarg1; 
  
  arg2 = (Joint *)jarg2; 
  
  b2d_Space_rid_Joint(arg1,arg2);
  
  
}



DllExport void* SWIGSTDCALL Ada_b2d_b2Joint_user_Data (
  void * jarg1
  )
{
  void* jresult ;
  b2Joint *arg1 = (b2Joint *) 0 ;
  void *result = 0 ;
  
  arg1 = (b2Joint *)jarg1; 
  
  result = (void *)b2d_b2Joint_user_Data(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_new_joint_Cursor (
  )
{
  void * jresult ;
  joint_Cursor *result = 0 ;
  
  result = (joint_Cursor *)new joint_Cursor();
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_delete_joint_Cursor (
  void * jarg1
  )
{
  joint_Cursor *arg1 = (joint_Cursor *) 0 ;
  
  arg1 = (joint_Cursor *)jarg1; 
  
  delete arg1;
  
  
}



DllExport joint_Cursor SWIGSTDCALL Ada_b2d_Space_first_Joint (
  void * jarg1
  )
{
  joint_Cursor jresult ;
  Space *arg1 = (Space *) 0 ;
  joint_Cursor result;
  
  arg1 = (Space *)jarg1; 
  
  result = b2d_Space_first_Joint(arg1);
  
  jresult = result; 
  //jresult = new joint_Cursor ((joint_Cursor &) result); 
  
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Space_next_Joint (
  void * jarg1
  )
{
  joint_Cursor *arg1 = (joint_Cursor *) 0 ;
  
  arg1 = (joint_Cursor *)jarg1; 
  
  b2d_Space_next_Joint(arg1);
  
  
}



DllExport void * SWIGSTDCALL Ada_b2d_Space_joint_Element (
  void * jarg1
  )
{
  void * jresult ;
  joint_Cursor *arg1 = (joint_Cursor *) 0 ;
  b2Joint *result = 0 ;
  
  arg1 = (joint_Cursor *)jarg1; 
  
  result = (b2Joint *)b2d_Space_joint_Element(arg1);
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_b2d_Space_Gravity_is (
  void * jarg1
  ,
  
  void * jarg2
  )
{
  Space *arg1 = (Space *) 0 ;
  Vector_3 *arg2 = (Vector_3 *) 0 ;
  
  arg1 = (Space *)jarg1; 
  
  arg2 = (Vector_3 *)jarg2; 
  
  b2d_Space_Gravity_is(arg1,arg2);
  
  
}



DllExport void SWIGSTDCALL Ada_b2d_Space_evolve (
  void * jarg1
  ,
  
  float jarg2
  )
{
  Space *arg1 = (Space *) 0 ;
  float arg2 ;
  
  arg1 = (Space *)jarg1; 
  
  
  arg2 = (float) jarg2; 
  
  
  b2d_Space_evolve(arg1,arg2);
  
  
}



DllExport void * SWIGSTDCALL Ada_new_b2d_ray_Collision (
  )
{
  void * jresult ;
  b2d_ray_Collision *result = 0 ;
  
  result = (b2d_ray_Collision *)new b2d_ray_Collision();
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_delete_b2d_ray_Collision (
  void * jarg1
  )
{
  b2d_ray_Collision *arg1 = (b2d_ray_Collision *) 0 ;
  
  arg1 = (b2d_ray_Collision *)jarg1; 
  
  delete arg1;
  
  
}



DllExport b2d_ray_Collision SWIGSTDCALL Ada_b2d_Space_cast_Ray (
  void * jarg1
  ,
  
  void * jarg2
  ,
  
  void * jarg3
  )
{
  b2d_ray_Collision jresult ;
  Space *arg1 = (Space *) 0 ;
  Vector_3 *arg2 = (Vector_3 *) 0 ;
  Vector_3 *arg3 = (Vector_3 *) 0 ;
  b2d_ray_Collision result;
  
  arg1 = (Space *)jarg1; 
  
  arg2 = (Vector_3 *)jarg2; 
  
  arg3 = (Vector_3 *)jarg3; 
  
  result = b2d_Space_cast_Ray(arg1,arg2,arg3);
  
  jresult = result; 
  //jresult = new b2d_ray_Collision ((b2d_ray_Collision &) result); 
  
  
  
  
  return jresult;
  
}



DllExport void * SWIGSTDCALL Ada_new_b2d_Contact (
  )
{
  void * jresult ;
  b2d_Contact *result = 0 ;
  
  result = (b2d_Contact *)new b2d_Contact();
  jresult = (void *) result;      
  
  
  
  return jresult;
  
}



DllExport void SWIGSTDCALL Ada_delete_b2d_Contact (
  void * jarg1
  )
{
  b2d_Contact *arg1 = (b2d_Contact *) 0 ;
  
  arg1 = (b2d_Contact *)jarg1; 
  
  delete arg1;
  
  
}



DllExport int SWIGSTDCALL Ada_b2d_space_contact_Count (
  void * jarg1
  )
{
  int jresult ;
  Space *arg1 = (Space *) 0 ;
  int result;
  
  arg1 = (Space *)jarg1; 
  
  result = (int)b2d_space_contact_Count(arg1);
  jresult = result; 
  
  
  
  return jresult;
  
}



DllExport b2d_Contact SWIGSTDCALL Ada_b2d_space_Contact (
  void * jarg1
  ,
  
  int jarg2
  )
{
  b2d_Contact jresult ;
  Space *arg1 = (Space *) 0 ;
  int arg2 ;
  b2d_Contact result;
  
  arg1 = (Space *)jarg1; 
  
  
  arg2 = (int) jarg2; 
  
  
  result = b2d_space_Contact(arg1,arg2);
  
  jresult = result; 
  //jresult = new b2d_Contact ((b2d_Contact &) result); 
  
  
  
  
  return jresult;
  
}



#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
extern "C" {
#endif
extern joint_Cursor    gnat_new_joint_Cursor()
{
  return joint_Cursor();
}


extern b2d_ray_Collision    gnat_new_b2d_ray_Collision()
{
  return b2d_ray_Collision();
}


extern b2d_Contact    gnat_new_b2d_Contact()
{
  return b2d_Contact();
}


#ifdef __cplusplus
}
#endif

