with
     GL,
     System;

package GLU
   with Obsolescent
--
--  Provides a subset of the functions in GLU, tailored to be suitable for use with the openGL 'Embedded' profile.
--
--  Currently only 'gluScaleImage' is ported.
--
is
   use GL;

   procedure gluScaleImage (Format    : in GLenum;
                            WidthIn   : in GLsizei;
                            HeightIn  : in GLsizei;
                            TypeIn    : in GLenum;
                            DataIn    : in System.Address;
                            WidthOut  : in GLsizei;
                            HeightOut : in GLsizei;
                            TypeOut   : in GLenum;
                            DataOut   : in System.Address);
   GLU_INVALID_VALUE,
   GLU_INVALID_ENUM,
   GLU_INVALID_TYPE,
   GLU_INVALID_OPERATION,
   GLU_OUT_OF_MEMORY : exception;

end GLU;
