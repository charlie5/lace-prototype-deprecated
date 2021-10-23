with
     gel_demo_Services,

     gel.Window.lumen,
     -- gel.Window.sdl,
     gel.Applet.client_World,
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

      the_Applet : gel.Applet.client_World.view;
   begin
      accept start;

      the_Applet := gel.Forge.new_client_Applet ("distributed Demo ~ Client", 1920, 1200);
--        the_Applet.gui_World.start;

      -- Register the client world as a mirror of the server world.
      --
      the_Applet.client_World.is_a_Mirror (of_World => gel_demo_Services.World);

      gel_demo_Services.World.register (the_Mirror         => the_Applet.client_World.all'Access,
                                        Mirror_as_observer => the_Applet.client_World.all'Access);

      --  Setup.
      --
      the_Applet.client_Camera.Site_is ((0.0, 0.0, 20.0));
      the_Applet.enable_simple_Dolly (1);

      --  Begin processing.
      --
      while the_Applet.is_open
      loop
         --  the_Applet.client_World.evolve (by => 1.0/60.0);
         the_Applet.freshen;
      end loop;

      --  Close.
      --
      --  gel_demo_services.World.deregister (the_mirror => the_Applet.client_World.all'access);
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
