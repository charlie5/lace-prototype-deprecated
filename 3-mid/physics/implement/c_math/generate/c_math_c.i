//
//   SWIG interface defintion for 'c_Math' interface library binding.
//

%module c_math_c

%{
extern "C"
{
   #include "../c/c_math.h"
}
%}


%include "../source/c/c_math.h"



// Tailoring
//

%inline 
%{   
      
%}






