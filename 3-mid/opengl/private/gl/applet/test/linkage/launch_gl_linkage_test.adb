with
     GL.safe,
     GL.lean,
     GL.desk,
     interfaces.C,
     System;

procedure launch_GL_linkage_Test
--
--  This test is only intended to check that all GL functions link correctly.
--  It is not meant to be run.
--
--  todo: Add missing calls for each profile.
is
   use GL;
begin
   --  Make a call to each core function
   --
   declare
      Result : GLenum;
      Status : GLboolean;
   begin
      glActiveTexture (0);
      glBindTexture (0, 0);
      glBlendFunc (0, 0);
      glClear (0);
      glClearColor (0.0, 0.0, 0.0, 0.0);
      glClearDepthf (0.0);
      glClearStencil (0);
      glColorMask (0, 0, 0, 0);
      glCullFace (0);
      glDepthFunc (0);
      glDepthMask (0);
      glDepthRangef (0.0, 0.0);
      glDisable (0);
      glDrawArrays (0, 0, 0);
      glEnable (0);
      glFinish;
      glFlush;
      glFrontFace (0);
      Result := glGetError;
      glHint (0, 0);
      Status := glIsEnabled (0);
      glLineWidth (0.0);
      glPixelStorei (0, 0);
      glPolygonOffset (0.0, 0.0);
      glScissor (0, 0, 0, 0);
      glStencilFunc (0, 0, 0);
      glStencilMask (0);
      glStencilOp (0, 0, 0);
      glTexParameteri (0, 0, 0);
      glViewport (0, 0, 0, 0);
   end;


   --  Make a call to each 'Safe' function
   --
   declare
      use safe;
      Result : access GLubyte;
   begin
      Result := glGetString (0);
      glDrawElements  (0, 0, 0, null);
      glGenTextures   (0, null);
      glGetBooleanv   (0, null);
      glGetFloatv     (0, null);
      glGetIntegerv   (0, null);
      glGetTexParameteriv (0, 0, null);
      glReadPixels    (0, 0, 0, 0, 0, 0, null);
      glTexImage2D    (0, 0, 0, 0, 0, 0, 0, 0, null);
      glTexSubImage2D (0, 0, 0, 0, 0, 0, 0, 0, null);
   end;


   --  Make a call to each 'Lean' function
   --
   declare
      use lean, System;
      a_GLenum       : GLenum;
      a_GLuint       : GLuint;
      a_GLboolean    : GLboolean;
      a_C_int        : interfaces.C.int;
      GLubyte_access : access GLubyte;
   begin
      glAttachShader            (0, 0);
      glBindAttribLocation      (0, 0, null);
      glBindBuffer              (0, 0);
      glBindFramebuffer         (0, 0);
      glBindRenderbuffer        (0, 0);
      glBlendColor              (0.0, 0.0, 0.0, 0.0);
      glBlendEquation           (0);
      glBlendEquationSeparate   (0, 0);
      glBlendFuncSeparate       (0, 0, 0, 0);
      glBufferData              (0, 0, null, 0);
      glBufferSubData           (0, 0, 0, null);
      a_GLenum := glCheckFramebufferStatus (0);
      glCompileShader           (0);
      glCompressedTexImage2D    (0, 0, 0, 0, 0, 0, 0, null);
      glCompressedTexSubImage2D (0, 0, 0, 0, 0, 0, 0, 0, null);
      glCopyTexImage2D          (0, 0, 0, 0, 0, 0, 0, 0);
      glCopyTexSubImage2D       (0, 0, 0, 0, 0, 0, 0, 0);
      a_GLuint := glCreateProgram;
      a_GLuint := glCreateShader (0);
      glDeleteBuffers           (0, null);
      glDeleteFramebuffers      (0, null);
      glDeleteProgram           (0);
      glDeleteRenderbuffers     (0, null);
      glDeleteShader            (0);
      glDeleteTextures          (0, null);
      glDetachShader            (0, 0);
      glDisableVertexAttribArray(0);
      glDrawElements            (0, 0, 0, null);
      glEnableVertexAttribArray (0);
      glFramebufferRenderbuffer (0, 0, 0, 0);
      glFramebufferTexture2D    (0, 0, 0, 0, 0);
      glGenBuffers              (0, null);
      glGenFramebuffers         (0, null);
      glGenRenderbuffers        (0, null);
      glGenTextures             (0, null);
      glGenerateMipmap          (0);
      glGetActiveAttrib         (0, 0, 0, null, null, null, null);
      glGetActiveUniform        (0, 0, 0, null, null, null, null);
      glGetAttachedShaders      (0, 0, null, null);
      a_C_int := glGetAttribLocation (0, null);
      glGetBooleanv             (0, null);
      glGetBufferParameteriv    (0, 0, null);
      glGetFloatv               (0, null);
      glGetFramebufferAttachmentParameteriv
                                (0, 0, 0, null);
      glGetIntegerv             (0, null);
      glGetProgramiv            (0, 0, null);
      glGetProgramInfoLog       (0, 0, null, null);
      glGetRenderbufferParameteriv
                                (0, 0, null);
      glGetShaderiv             (0, 0, null);
      glGetShaderInfoLog        (0, 0, null, null);
      glGetShaderPrecisionFormat(0, 0, null, null);
      glGetShaderSource         (0, 0, null, null);
      GLubyte_access := glGetString(0);
      glGetTexParameterfv       (0, 0, null_Address);
      glGetTexParameteriv       (0, 0, null);
      glGetUniformfv            (0, 0, null_Address);
      glGetUniformiv            (0, 0, null);
      a_C_int := glGetUniformLocation (0, null);
      glGetVertexAttribfv       (0, 0, null_Address);
      glGetVertexAttribiv       (0, 0, null);
      glGetVertexAttribPointerv (0, 0, null);
      a_GLboolean := glIsBuffer      (0);
      a_GLboolean := glIsFramebuffer (0);
      a_GLboolean := glIsProgram     (0);
      a_GLboolean := glIsRenderbuffer(0);
      a_GLboolean := glIsShader      (0);
      a_GLboolean := glIsTexture     (0);
      glLinkProgram           (0);
      glReadPixels            (0, 0, 0, 0, 0, 0, null);
      glReleaseShaderCompiler;
      glRenderbufferStorage   (0, 0, 0, 0);
      glSampleCoverage        (0.0, 0);
      glShaderBinary          (0, null, 0, null, 0);
      glShaderSource          (0, 0, null, null);
      glStencilFuncSeparate   (0, 0, 0, 0);
      glStencilMaskSeparate   (0, 0);
      glStencilOpSeparate     (0, 0, 0, 0);
      glTexImage2D            (0, 0, 0, 0, 0, 0, 0, 0, null);
      glTexParameterf         (0, 0, 0.0);
      glTexParameterfv        (0, 0, null_Address);
      glTexParameteriv        (0, 0, null);
      glTexSubImage2D         (0, 0, 0, 0, 0, 0, 0, 0, null);
      glUniform1f             (0, 0.0);
      glUniform1fv            (0, 0, null_Address);
      glUniform1i             (0, 0);
      glUniform1iv            (0, 0, null);
      glUniform2f             (0, 0.0, 0.0);
      glUniform2fv            (0, 0, null_Address);
      glUniform2i             (0, 0, 0);
      glUniform2iv            (0, 0, null);
      glUniform3f             (0, 0.0, 0.0, 0.0);
      glUniform3fv            (0, 0, null_Address);
      glUniform3i             (0, 0, 0, 0);
      glUniform3iv            (0, 0, null);
      glUniform4f             (0, 0.0, 0.0, 0.0, 0.0);
      glUniform4fv            (0, 0, null_Address);
      glUniform4i             (0, 0, 0, 0, 0);
      glUniform4iv            (0, 0, null);
      glUniformMatrix2fv      (0, 0, 0, null_Address);
      glUniformMatrix3fv      (0, 0, 0, null_Address);
      glUniformMatrix4fv      (0, 0, 0, null_Address);
      glUseProgram            (0);
      glValidateProgram       (0);
      glVertexAttrib1f        (0, 0.0);
      glVertexAttrib1fv       (0, null_Address);
      glVertexAttrib2f        (0, 0.0, 0.0);
      glVertexAttrib2fv       (0, null_Address);
      glVertexAttrib3f        (0, 0.0, 0.0, 0.0);
      glVertexAttrib3fv       (0, null_Address);
      glVertexAttrib4f        (0, 0.0, 0.0, 0.0, 0.0);
      glVertexAttrib4fv       (0, null_Address);
      glVertexAttribPointer   (0, 0, 0, 0, 0, null);
   end;


   --  Make a call to each 'desk' function
   --
   declare
      use desk;
      a_GLboolean : GLboolean;
   begin
      glActiveTexture           (0);
      glBindTexture             (0, 0);
      glBlendColor              (0.0, 0.0, 0.0, 0.0);
      glBlendEquation           (0);
      glBlendEquationSeparate   (0, 0);
      glBlendFunc               (0, 0);
      glClearStencil            (0);
      glClearDepth              (0.0);
      glColorMask               (0, 0, 0, 0);
      glCompressedTexImage2D    (0, 0, 0, 0, 0, 0, 0, null);
      glCompressedTexSubImage2D (0, 0, 0, 0, 0, 0, 0, 0, null);
      glCopyTexImage2D          (0, 0, 0, 0, 0, 0, 0, 0);
      glCopyTexSubImage2D       (0, 0, 0, 0, 0, 0, 0, 0);
      glDeleteTextures          (0, null);
      glDepthMask               (0);
      glDisable                 (0);
      glDrawArrays              (0, 0, 0);
      glGetBooleanv             (0, null);
      glGetFloatv               (0, null);
      glGetIntegerv             (0, null);
      glGetTexParameterfv       (0, 0, null);
      glGetTexParameteriv       (0, 0, null);
      a_GLboolean := glIsTexture(0);
      glLineWidth               (0.0);
      glPixelStorei             (0, 0);
      glPolygonOffset           (0.0, 0.0);
      glReadPixels              (0, 0, 0, 0, 0, 0, null);
      glSampleCoverage          (0.0, 0);
      glStencilMask             (0);
      glStencilOp               (0, 0, 0);
      glTexImage2D              (0, 0, 0, 0, 0, 0, 0, 0, null);
      glTexParameterf           (0, 0, 0.0);
      glTexParameterfv          (0, 0, null);
      glTexParameteri           (0, 0, 0);
      glTexParameteriv          (0, 0, null);
      glTexSubImage2D           (0, 0, 0, 0, 0, 0, 0, 0, null);
   end;

end launch_GL_linkage_Test;
