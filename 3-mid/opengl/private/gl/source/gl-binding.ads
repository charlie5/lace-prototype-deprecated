package gl.Binding
--
--  Provides functions common to all openGL profiles.
--
is

   procedure glActiveTexture (texture  : in     GLenum);
   procedure glBindTexture   (target   : in     GLenum;
                              texture  : in     GLuint);
   procedure glBlendFunc     (sfactor  : in     GLenum;
                              dfactor  : in     GLenum);
   procedure glClear         (mask     : in     GLbitfield);
   procedure glClearColor    (red      : in     GLclampf;
                              green    : in     GLclampf;
                              blue     : in     GLclampf;
                              alpha    : in     GLclampf);
   procedure glClearDepthf   (depth    : in     GLclampf);
   procedure glClearStencil  (s        : in     GLint);
   procedure glColorMask     (red      : in     GLboolean;
                              green    : in     GLboolean;
                              blue     : in     GLboolean;
                              alpha    : in     GLboolean);
   procedure glCullFace      (mode     : in     GLenum);
   procedure glDepthFunc     (func     : in     GLenum);
   procedure glDepthMask     (flag     : in     GLboolean);
   procedure glDepthRangef   (zNear    : in     GLclampf;
                              zFar     : in     GLclampf);
   procedure glDisable       (cap      : in     GLenum);
   procedure glDrawArrays    (mode     : in     GLenum;
                              first    : in     GLint;
                              count    : in     GLsizei);
   procedure glDrawElements  (mode     : in     GLenum;
                              count    : in     GLsizei;
                              the_type : in     GLenum;
                              indices  : access GLvoid);
   procedure glEnable        (cap      : in     GLenum);
   procedure glFinish;
   procedure glFlush;
   procedure glFrontFace     (mode     : in     GLenum);
   procedure glGenTextures   (n        : in     GLsizei;
                              textures : access GLuint);
   function  glGetError                                  return GLenum;
   procedure glGetBooleanv   (pname    : in     GLenum;
                              params   : access GLboolean);
   procedure glGetFloatv     (pname    : in     GLenum;
                              params   : access GLfloat);
   procedure glGetIntegerv   (pname    : in     GLenum;
                              params   : access GLint);
   function  glGetString     (name     : in     GLenum)  return access GLubyte;
   procedure glGetTexParameteriv
                             (target   : in     GLenum;
                              pname    : in     GLenum;
                              params   : access GLint);
   procedure glHint          (target   : in     GLenum;
                              mode     : in     GLenum);
   function  glIsEnabled     (cap      : in     GLenum)   return GLboolean;
   procedure glLineWidth     (width    : in     GLfloat);
   procedure glPixelStorei   (pname    : in     GLenum;
                              param    : in     GLint);
   procedure glPolygonOffset (factor   : in     GLfloat;
                              units    : in     GLfloat);
   procedure glReadPixels    (x        : in     GLint;
                              y        : in     GLint;
                              width    : in     GLsizei;
                              height   : in     GLsizei;
                              format   : in     GLenum;
                              the_type : in     GLenum;
                              pixels   : access GLvoid);
   procedure glScissor       (x        : in     GLint;
                              y        : in     GLint;
                              width    : in     GLsizei;
                              height   : in     GLsizei);
   procedure glStencilFunc   (func     : in     GLenum;
                              ref      : in     GLint;
                              mask     : in     GLuint);
   procedure glStencilMask   (mask     : in     GLuint);
   procedure glStencilOp     (fail     : in     GLenum;
                              zfail    : in     GLenum;
                              zpass    : in     GLenum);
   procedure glTexImage2D    (target   : in     GLenum;
                              level    : in     GLint;
                              internalformat
                                       : in     GLenum;
                              width    : in     GLsizei;
                              height   : in     GLsizei;
                              border   : in     GLint;
                              format   : in     GLenum;
                              the_type : in     GLenum;
                              pixels   : access GLvoid);
   procedure glTexSubImage2D (target   : in     GLenum;
                              level    : in     GLint;
                              xoffset  : in     GLint;
                              yoffset  : in     GLint;
                              width    : in     GLsizei;
                              height   : in     GLsizei;
                              format   : in     GLenum;
                              the_type : in     GLenum;
                              pixels   : access GLvoid);
   procedure glTexParameteri (target   : in     GLenum;
                              pname    : in     GLenum;
                              param    : in     GLint);
   procedure glViewport      (x        : in     GLint;
                              y        : in     GLint;
                              width    : in     GLsizei;
                              height   : in     GLsizei);



private

   pragma Import (StdCall, glActiveTexture, "glActiveTexture");
   pragma Import (Stdcall, glBindTexture,   "glBindTexture");
   pragma Import (Stdcall, glBlendFunc,     "glBlendFunc");
   pragma Import (Stdcall, glClear,         "glClear");
   pragma Import (Stdcall, glClearColor,    "glClearColor");
   pragma Import (Stdcall, glClearDepthf,   "glClearDepthf");
   pragma Import (Stdcall, glClearStencil,  "glClearStencil");
   pragma Import (Stdcall, glColorMask,     "glColorMask");
   pragma Import (Stdcall, glCullFace,      "glCullFace");
   pragma Import (Stdcall, glDepthFunc,     "glDepthFunc");
   pragma Import (Stdcall, glDepthMask,     "glDepthMask");
   pragma Import (Stdcall, glDepthRangef,   "glDepthRangef");
   pragma Import (Stdcall, glDisable,       "glDisable");
   pragma Import (Stdcall, glDrawArrays,    "glDrawArrays");
   pragma Import (Stdcall, glDrawElements,  "glDrawElements");
   pragma Import (Stdcall, glEnable,        "glEnable");
   pragma Import (Stdcall, glFinish,        "glFinish");
   pragma Import (Stdcall, glFlush,         "glFlush");
   pragma Import (Stdcall, glFrontFace,     "glFrontFace");
   pragma Import (Stdcall, glGenTextures,   "glGenTextures");
   pragma Import (Stdcall, glGetError,      "glGetError");
   pragma Import (StdCall, glGetBooleanv,   "glGetBooleanv");
   pragma Import (StdCall, glGetFloatv,     "glGetFloatv");
   pragma Import (StdCall, glGetIntegerv,   "glGetIntegerv");
   pragma Import (StdCall, glGetString,     "glGetString");
   pragma Import (StdCall, glGetTexParameteriv,
                                            "glGetTexParameteriv");
   pragma Import (Stdcall, glHint,          "glHint");
   pragma Import (Stdcall, glIsEnabled,     "glIsEnabled");
   pragma Import (Stdcall, glLineWidth,     "glLineWidth");
   pragma Import (Stdcall, glPixelStorei,   "glPixelStorei");
   pragma Import (Stdcall, glPolygonOffset, "glPolygonOffset");
   pragma Import (StdCall, glReadPixels,    "glReadPixels");
   pragma Import (Stdcall, glScissor,       "glScissor");
   pragma Import (Stdcall, glStencilFunc,   "glStencilFunc");
   pragma Import (Stdcall, glStencilMask,   "glStencilMask");
   pragma Import (Stdcall, glStencilOp,     "glStencilOp");
   pragma Import (StdCall, glTexImage2D,    "glTexImage2D");
   pragma Import (StdCall, glTexSubImage2D, "glTexSubImage2D");
   pragma Import (Stdcall, glTexParameteri, "glTexParameteri");
   pragma Import (Stdcall, glViewport,      "glViewport");

end gl.Binding;
