package body openGL.Surface.privvy
is

   function to_GLX (Self : in Surface.item'Class) return glx.Drawable
   is
   begin
      return Self.glx_Surface;
   end to_GLX;

end openGL.Surface.privvy;
