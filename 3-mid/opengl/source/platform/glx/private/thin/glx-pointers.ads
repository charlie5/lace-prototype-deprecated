package GLX.Pointers
is
   --  VisualID_Pointer
   --
   type VisualID_Pointer  is access all VisualID;
   type VisualID_Pointers is array (C.size_t range <>) of aliased VisualID_Pointer;

   --  XVisualInfo_Pointer
   --
   type XVisualInfo_Pointer is access all XVisualInfo;
   type XVisualInfo_Pointers is array (C.size_t range <>) of aliased XVisualInfo_Pointer;

   --  Pixmap_Pointer
   --
   type Pixmap_Pointer  is access all Pixmap;
   type Pixmap_Pointers is array (C.size_t range <>) of aliased Pixmap_Pointer;

   --  Font_Pointer
   --
   type Font_Pointer  is access all Font;
   type Font_Pointers is array (C.size_t range <>) of aliased Font_Pointer;

   --  Window_Pointer
   --
   type Window_Pointer  is access all Window;
   type Window_Pointers is array (C.size_t range <>) of aliased Window_Pointer;

   --  Bool_Pointer
   --
   type Bool_Pointer  is access all Bool;
   type Bool_Pointers is array (C.size_t range <>) of aliased Bool_Pointer;

   --  ContextRec_Pointer
   --
   type ContextRec_Pointer  is access all ContextRec;
   type ContextRec_Pointers is array (C.size_t range <>) of aliased ContextRec_Pointer;

   --  XID_Pointer
   --
   type XID_Pointer  is access all XID;
   type XID_Pointers is array (C.size_t range <>) of aliased XID_Pointer;

   --  GLXPixmap_Pointer
   --
   type GLXPixmap_Pointer  is access all GLXPixmap;
   type GLXPixmap_Pointers is array (C.size_t range <>) of aliased GLXPixmap_Pointer;

   --  Drawable_Pointer
   --
   type Drawable_Pointer  is access all Drawable;
   type Drawable_Pointers is array (C.size_t range <>) of aliased Drawable_Pointer;

   --  FBConfig_Pointer
   --
   type FBConfig_Pointer  is access all FBConfig;
   type FBConfig_Pointers is array (C.size_t range <>) of aliased FBConfig_Pointer;

   --  GLXFBConfigID_Pointer
   --
   type FBConfigID_Pointer  is access all FBConfigID;
   type FBConfigID_Pointers is array (C.size_t range <>) of aliased FBConfigID_Pointer;

   --  GLXContextID_Pointer
   --
   type ContextID_Pointer  is access all ContextID;
   type ContextID_Pointers is array (C.size_t range <>) of aliased ContextID_Pointer;

   --  GLXWindow_Pointer
   --
   type GLXWindow_Pointer  is access all GLXWindow;
   type GLXWindow_Pointers is array (C.size_t range <>) of aliased GLXWindow_Pointer;

   --  PBuffer_Pointer
   --
   type PBuffer_Pointer  is access all PBuffer;
   type PBuffer_Pointers is array (C.size_t range <>) of aliased PBuffer_Pointer;

end GLX.Pointers;
