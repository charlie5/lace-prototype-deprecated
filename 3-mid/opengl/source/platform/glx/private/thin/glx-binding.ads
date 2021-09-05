with
     glx.Pointers,
     interfaces.C;

package glx.Binding
is
   function  getCurrentContext  return access ContextRec;
   function  getCurrentDrawable return        Drawable;

   procedure waitGL;
   procedure waitX;

   procedure useXFont (Font  : in GLX.Font;
                       First : in C.int;
                       Count : in C.int;
                       List  : in C.int);

   function  getCurrentReadDrawable return Drawable;

   function  get_visualid (Self : in Pointers.XVisualInfo_Pointer) return VisualID;



private

   pragma Import (C, getCurrentContext,      "glXGetCurrentContext");
   pragma Import (C, getCurrentDrawable,     "glXGetCurrentDrawable");
   pragma Import (C, waitGL,                 "glXWaitGL");
   pragma Import (C, waitX,                  "glXWaitX");
   pragma Import (C, useXFont,               "glXUseXFont");
   pragma Import (C, getCurrentReadDrawable, "glXGetCurrentReadDrawable");
   pragma Import (C, get_visualid,           "Ada_get_visualid");

end glx.Binding;
