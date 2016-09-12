with
     openGL.Context,
     interfaces.C;


package body openGL.Surface
is
   use -- Glx,
       Interfaces;


--     visual_attribs : array (Positive range <>) of aliased C.int := (GLX_X_RENDERABLE,    1,
--                                                                     GLX_DRAWABLE_TYPE,   GLX_WINDOW_BIT,
--                                                                     GLX_RENDER_TYPE,     GLX_RGBA_BIT,
--                                                                     GLX_X_VISUAL_TYPE,   GLX_TRUE_COLOR,
--                                                                     GLX_RED_SIZE,        8,
--                                                                     GLX_GREEN_SIZE,      8,
--                                                                     GLX_BLUE_SIZE,       8,
--                                                                     GLX_ALPHA_SIZE,      8,
--                                                                     GLX_DEPTH_SIZE,      24,
--                                                                     GLX_STENCIL_SIZE,    8,
--                                                                     GLX_DOUBLEBUFFER,    1,
--                                                                     -- GLX_SAMPLE_BUFFERS  , 1,
--                                                                     -- GLX_SAMPLES         , 4,
--                                                                     0
--                                                                    );


   procedure define (Self : in out Item;   surface_Profile : in openGL.surface_Profile.item'Class;
                                           Display         : in openGL.Display.Item;
                                           Window_Id       : in Natural)
   is
      pragma Unreferenced (Window_Id);

      the_surface_Profile : constant openGL.surface_Profile.item'Class := surface_Profile;
   begin
      Self.Display := Display;
   end define;



   --  Operations
   --

   procedure swap_Buffers (Self : in Item)
   is
   begin
      null;
   end swap_Buffers;


end openGL.Surface;
