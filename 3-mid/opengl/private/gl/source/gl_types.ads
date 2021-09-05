with
     Interfaces.C,
     System;

package GL_Types
--
--  Provides openGL types whose definitions may differ amongst platforms.
--
--  This file is generated by the 'generate_GL_types_Spec' tool.
--
is
   pragma Pure;
   use Interfaces;

   subtype GLenum     is C.unsigned;
   subtype GLboolean  is C.unsigned_char;
   subtype GLbitfield is C.unsigned;
   subtype GLvoid     is system.Address;
   subtype GLbyte     is C.signed_char;
   subtype GLshort    is C.short;
   subtype GLint      is C.int;
   subtype GLubyte    is C.unsigned_char;
   subtype GLushort   is C.unsigned_short;
   subtype GLuint     is C.unsigned;
   subtype GLsizei    is C.int;
   subtype GLfloat    is C.C_float;
   subtype GLclampf   is C.C_float;
   subtype GLdouble   is C.double;
   subtype GLclampd   is C.double;
   subtype GLchar     is C.char;
   subtype GLfixed    is Integer_32;

end GL_Types;
