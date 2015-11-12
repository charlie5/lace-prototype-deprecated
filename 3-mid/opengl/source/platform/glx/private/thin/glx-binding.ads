with
     glx.Pointers,
     Interfaces.C;


package GLX.Binding
is


   function  glXGetCurrentContext  return access GLX.GLXcontextRec;
   function  glXGetCurrentDrawable return  GLX.GLXDrawable;

   procedure glXWaitGL;
   procedure glXWaitX;

   procedure glXUseXFont (font  : in GLX.Font;
                          first : in Interfaces.C.int;
                          count : in Interfaces.C.int;
                          list  : in Interfaces.C.int);


   function glXGetCurrentReadDrawable return  GLX.GLXDrawable;


   function get_visualid (Self : in GLX.Pointers.XVisualInfo_Pointer) return GLX.VisualID;



private

   pragma Import (C, glXGetCurrentContext,      "glXGetCurrentContext");
   pragma Import (C, glXGetCurrentDrawable,     "glXGetCurrentDrawable");
   pragma Import (C, glXWaitGL,                 "glXWaitGL");
   pragma Import (C, glXWaitX,                  "glXWaitX");
   pragma Import (C, glXUseXFont,               "glXUseXFont");
   pragma Import (C, glXGetCurrentReadDrawable, "glXGetCurrentReadDrawable");
   pragma Import (C, get_visualid,              "Ada_get_visualid");

end GLX.Binding;
