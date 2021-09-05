with
     glx.Pointers;

package GLX.Pointer_Pointers
is
   use glx.Pointers;

   type VisualID_Pointer_Pointer    is access all VisualID_Pointer;
   type XVisualInfo_Pointer_Pointer is access all XVisualInfo_Pointer;
   type Pixmap_Pointer_Pointer      is access all Pixmap_Pointer;
   type Font_Pointer_Pointer        is access all Font_Pointer;
   type Window_Pointer_Pointer      is access all Window_Pointer;
   type Bool_Pointer_Pointer        is access all Bool_Pointer;
   type ContextRec_Pointer_Pointer  is access all ContextRec_Pointer;
   type XID_Pointer_Pointer         is access all XID_Pointer;
   type GLXPixmap_Pointer_Pointer   is access all GLXPixmap_Pointer;
   type Drawable_Pointer_Pointer    is access all Drawable_Pointer;
   type FBConfig_Pointer_Pointer    is access all FBConfig_Pointer;
   type FBConfigID_Pointer_Pointer  is access all FBConfigID_Pointer;
   type ContextID_Pointer_Pointer   is access all ContextID_Pointer;
   type GLXWindow_Pointer_Pointer   is access all Window_Pointer;
   type PBuffer_Pointer_Pointer     is access all PBuffer_Pointer;

end GLX.Pointer_Pointers;
