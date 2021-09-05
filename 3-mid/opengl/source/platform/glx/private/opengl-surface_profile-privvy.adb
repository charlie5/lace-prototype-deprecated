package body openGL.surface_Profile.privvy
is

   function to_GLX (Self : in Item'Class) return glx.FBConfig
   is
   begin
      return Self.glx_Config;
   end to_GLX;

end openGL.surface_Profile.privvy;
