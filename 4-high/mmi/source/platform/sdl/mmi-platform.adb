with mmi.Display.sdl;



package body mmi.Platform
--
--
--
is

   function Display return mmi.Display.view
   is
   begin
      return mmi.Display.sdl.forge.new_Display.all'access;
   end;

end mmi.Platform;
