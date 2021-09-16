with
     mmi.Applet.gui_world,
     mmi.Forge,
     mmi.Window.setup,     -- This makes MMI use the GL window system selected by the 'opengl_platform' scenario variable.
     Ada.Text_IO,
     Ada.Exceptions;

pragma Unreferenced (mmi.Window.setup);


procedure launch_hello_MMI
--
--  Opens an MMI window.
--
is
   use mmi.Applet.gui_world, Ada.Text_IO;

   the_Applet : mmi.Applet.gui_World.view := mmi.Forge.new_gui_Applet ("Hello MMI");

begin
   while the_Applet.is_open
   loop
      the_Applet.gui_World.evolve (by => 1.0 / 60.0);     -- Evolve the world.
      the_Applet.freshen;                                 -- Handle any new events and update the screen.
   end loop;

   free (the_Applet);

exception
   when E : others =>
      put_Line ("Exception in Environment task");
      put_Line (ada.Exceptions.Exception_Information (E));
end launch_hello_MMI;
