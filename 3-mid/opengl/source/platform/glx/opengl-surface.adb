with
     openGL.Context,
     interfaces.C;

package body openGL.Surface     -- TODO: Finish this package.
is
   use Glx,
       Interfaces;

   visual_Attributes : array (Positive range <>) of aliased C.int := (GLX_X_RENDERABLE,    1,
                                                                      GLX_DRAWABLE_TYPE,   GLX_WINDOW_BIT,
                                                                      GLX_RENDER_TYPE,     GLX_RGBA_BIT,
                                                                      GLX_X_VISUAL_TYPE,   GLX_TRUE_COLOR,
                                                                      GLX_RED_SIZE,        8,
                                                                      GLX_GREEN_SIZE,      8,
                                                                      GLX_BLUE_SIZE,       8,
                                                                      GLX_ALPHA_SIZE,      8,
                                                                      GLX_DEPTH_SIZE,      24,
                                                                      GLX_STENCIL_SIZE,    8,
                                                                      GLX_DOUBLEBUFFER,    1,
                                                                      0);


   procedure define (Self : in out Item;   Profile   : in surface_Profile.item'Class;
                                           Window_Id : in Natural)
   is
      pragma Unreferenced (Window_Id);
--        the_Profile : constant surface_Profile.item'Class := Profile;
   begin
      null;
   end define;



   --  Operations
   --

   procedure swap_Buffers (Self : in Item)
   is
   begin
      null;
   end swap_Buffers;


end openGL.Surface;
