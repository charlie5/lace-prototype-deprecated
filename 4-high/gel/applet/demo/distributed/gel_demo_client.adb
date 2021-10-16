with
     gel_demo_Services,

     gel.Window.lumen,
     gel.Applet.gui_World,
     gel.World,
     gel.Forge,
     gel.Camera,
     gel.Sprite,

     float_Math,

     ada.Text_IO,
     ada.Exceptions;


package body gel_demo_Client
is
   use ada.Text_IO;

   package Math renames float_Math;



   task body Item
   is
      use gel.Applet;

      the_Applet : gel.Applet.gui_World.view;
   begin
      accept start;

      the_Applet := gel.Forge.new_gui_Applet ("distributed Demo ~ Client", 1920, 1200);
--        the_Applet.gui_World.start;

      -- Register the client world as a mirror of the server world.
      --
      the_Applet.gui_World.Gravity_is ((0.0, 0.0, 0.0));
      the_Applet.gui_World.is_a_Mirror (of_world => gel_demo_Services.World);

      gel_demo_Services.World.register (the_mirror         => the_Applet.gui_World.all'Access,
                                        mirror_as_observer => the_Applet.gui_World.all'Access);
      --  Setup.
      --
      the_Applet.gui_Camera.Site_is ((0.0, 0.0, 20.0));
      the_Applet.enable_simple_Dolly (1);

      --  Begin processing.
      --
      while the_Applet.is_open
      loop
         the_Applet.gui_World.evolve (by => 1.0/60.0);
         the_Applet.freshen;
      end loop;

      --  Close.
      --
      gel_demo_services.World.deregister (the_mirror => the_Applet.gui_World.all'access);
      the_Applet.destroy;
      --           Client_is_running := False;

      put_Line ("Client done.");


   exception
      when E : others =>
         put_Line ("Client unhandled exception ...");
         put_Line (ada.exceptions.Exception_Information (E));
         put_Line ("Client has terminated !");
   end Item;


end gel_demo_Client;
