with
     eGL.Binding,
     Swig,
     interfaces.C.Strings,
     System;

procedure launch_egl_linkage_Test
--
--  Tests linkage to eGL functions.
--  Is not meant to be run.
--
is
   use eGL,
       eGL.Binding,
       System;

   an_EGLint     : EGLint;
   an_EGLdisplay : EGLdisplay;
   an_EGLboolean : EGLboolean;
   an_EGLsurface : EGLsurface;
   an_EGLcontext : EGLcontext;

   a_chars_ptr   : interfaces.C.strings.chars_ptr;
   a_void_ptr    : swig.void_ptr;

   an_EGLdisplay_pointer : access EGLdisplay;

begin
   an_EGLint     := eglGetError;
   an_EGLdisplay := eglGetDisplay        (null);
   an_EGLboolean := eglInitialize        (null_Address, null, null);
   an_EGLboolean := eglTerminate         (null_Address);
   a_chars_ptr   := eglQueryString       (null_Address, 0);

   an_EGLboolean := eglGetConfigs        (null_Address, null, 0, null);
   an_EGLboolean := eglChooseConfig      (null_Address, null, null, 0, null);
   an_EGLboolean := eglGetConfigAttrib   (null_Address, null_Address, 0, null);

   an_EGLsurface := eglCreateWindowSurface  (null_Address, null_Address, 0, null);
   an_EGLsurface := eglCreatePbufferSurface (null_Address, null_Address, null);
   an_EGLsurface := eglCreatePixmapSurface  (null_Address, null_Address, 0, null);

   an_EGLboolean := eglDestroySurface    (null_Address, null_Address);
   an_EGLboolean := eglQuerySurface      (null_Address, null_Address, 0, null);
   an_EGLboolean := eglBindAPI           (0);
   an_EGLboolean := eglQueryAPI;
   an_EGLboolean := eglWaitClient;
   an_EGLboolean := eglReleaseThread;
   an_EGLsurface := eglCreatePbufferFromClientBuffer
                                         (null_Address, 0, null_Address, null_Address, null);
   an_EGLboolean := eglSurfaceAttrib     (null_Address, null_Address, 0, 0);
   an_EGLboolean := eglBindTexImage      (null_Address, null_Address, 0);
   an_EGLboolean := eglReleaseTexImage   (null_Address, null_Address, 0);
   an_EGLboolean := eglSwapInterval      (null_Address, 0);
   an_EGLcontext := eglCreateContext     (null_Address, null_Address, null_Address, null);
   an_EGLboolean := eglDestroyContext    (null_Address, null_Address);
   an_EGLboolean := eglMakeCurrent       (null_Address, null_Address, null_Address, null_Address);

   an_EGLcontext := eglGetCurrentContext;
   an_EGLsurface := eglGetCurrentSurface (0);
   an_EGLdisplay := eglGetCurrentDisplay;
   an_EGLboolean := eglQueryContext      (null_Address, null_Address, 0, null);
   an_EGLboolean := eglWaitGL;
   an_EGLboolean := eglWaitNative        (0);
   an_EGLboolean := eglSwapBuffers       (null_Address, null_Address);
   an_EGLboolean := eglCopyBuffers       (null_Address, null_Address, 0);
   a_void_ptr    := eglGetProcAddress    (Interfaces.C.Strings.null_ptr);
   an_EGLdisplay_pointer
                 := egl_DEFAULT_DISPLAY;
   an_EGLcontext := egl_NO_CONTEXT;
   an_EGLdisplay := egl_NO_DISPLAY;
   an_EGLsurface := egl_NO_SURFACE;
   an_EGLint     := egl_DONT_CARE;
end launch_egl_linkage_Test;
