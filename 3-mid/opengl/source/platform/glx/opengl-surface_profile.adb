with
     interfaces.C,
     ada.unchecked_Conversion;

package body openGL.surface_Profile     -- TODO: Finish this package.
is
   use Interfaces,
       GLX;

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
                                                                      -- GLX_SAMPLE_BUFFERS  , 1,
                                                                      -- GLX_SAMPLES         , 4,
                                                                      0);


   procedure define (Self : in out Item;   Screen  : access openGL.Screen.item'Class;
                                           Desired : in     Qualities := default_Qualities)
   is
      pragma Unreferenced (Desired);

--        num_fb_configs : aliased C.int := 0;
--        visual_Id      : aliased C.int;
      Unused : C.int with Unreferenced;
   begin
      null;
   end define;



   function get_Visual (Self : in Item) return access glx.XVisualInfo
   is
   begin
      return Self.Visual;
   end get_Visual;



   function fetch_All return surface_Profile.items
   is
   begin
      raise Program_Error with "TBD";
      return (1 .. 0 =>  <>);
   end fetch_All;



   function Quality (Self : in Item) return Qualities
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "TBD";
      return (others => <>);
   end Quality;



   function value_Image (Value : in Natural) return String
   is
   begin
      if Value = Irrelevant then
         return "Irrelevant";
      else
         return Natural'Image (Value);
      end if;
   end value_Image;



   function Image (Self : in color_Buffer) return String
   is
   begin
      return "("
        &      "Bits_red =>"        & value_Image (Self.Bits_red)
        &    ", Bits_green =>"      & value_Image (Self.Bits_green)
        &    ", Bits_blue =>"       & value_Image (Self.Bits_blue)
        &    ", Bits_luminence =>"  & value_Image (Self.Bits_luminence)
        &    ", Bits_alpha =>"      & value_Image (Self.Bits_alpha)
        &    ", Bits_alpha_mask =>" & value_Image (Self.Bits_alpha_mask)
        &    ")";
   end Image;



   function Image (Self : in Qualities) return String
   is
   begin
      return "("
        &  Image (Self.color_Buffer)
        & ", depth_buffer_Bits =>"    & value_Image (Self.  depth_buffer_Bits)
        & ", stencil_buffer_Bits => " & value_Image (Self.stencil_buffer_Bits)
        & ")";
   end Image;


end openGL.surface_Profile;
