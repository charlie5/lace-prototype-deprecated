//
//   SWIG interface defintion for 'Bullet_C' interface library binding.
//

%module bullet_c

%{
extern "C"
{
   #include "../c/bullet-shape.h"
   #include "../c/bullet-object.h"
   #include "../c/bullet-joint.h"
   #include "../c/bullet-space.h"
}
%}


%import "../../c_math/generate/c_math_c.i"

%include "../source/c/bullet.h"
%include "../source/c/bullet-shape.h"
%include "../source/c/bullet-object.h"
%include "../source/c/bullet-joint.h"
%include "../source/c/bullet-space.h"



// Tailoring
//

%inline 
%{   
      
%}






