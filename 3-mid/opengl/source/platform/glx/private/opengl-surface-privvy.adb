package body openGL.Surface.privvy
is

   function to_glx (Self : in Surface.item'Class) return glx.GLXDrawable
   is
   begin
      return Self.glx_Surface;
   end to_glx;

end openGL.Surface.privvy;
