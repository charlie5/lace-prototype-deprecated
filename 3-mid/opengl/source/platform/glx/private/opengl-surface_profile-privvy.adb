package body openGL.surface_Profile.privvy
is

   function to_glx (Self : in Item'Class) return GLX.GLXFBConfig
   is
   begin
      return Self.glx_Config;
   end to_glx;

end openGL.surface_Profile.privvy;
