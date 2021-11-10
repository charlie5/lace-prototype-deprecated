with
     gel.Applet.gui_world,
     gel.Forge,
     gel.Window.setup,

     ada.Text_IO,
     ada.Exceptions;

pragma unreferenced (gel.Window.setup);


procedure launch_hello_GEL
--
--  Opens a GEL window.
--
is
   use gel.Applet.gui_world,
       ada.Text_IO;

   the_Applet : gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("Hello GEL");

begin
   while the_Applet.is_open
   loop
      the_Applet.gui_World.evolve;     -- Evolve the world.
      the_Applet.freshen;              -- Handle any new events and update the screen.
   end loop;

   free (the_Applet);

exception
   when E : others =>
      put_Line ("Exception in Environment task");
      put_Line (ada.Exceptions.Exception_Information (E));
end launch_hello_GEL;
