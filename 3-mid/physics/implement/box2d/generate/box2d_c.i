//
//   SWIG interface defintion for 'Box2d_C' interface library binding.
//

%module box2d_c

%{
extern "C"
{
   #include "../c/box2d-shape.h"
   #include "../c/box2d-object.h"
   #include "../c/box2d-joint.h"
   #include "../c/box2d-space.h"
}
%}


%import "../../c_math/generate/c_math_c.i"

%include "../source/c/box2d.h"
%include "../source/c/box2d-shape.h"
%include "../source/c/box2d-object.h"
%include "../source/c/box2d-joint.h"
%include "../source/c/box2d-space.h"



// Tailoring
//

%inline 
%{   
      
%}






