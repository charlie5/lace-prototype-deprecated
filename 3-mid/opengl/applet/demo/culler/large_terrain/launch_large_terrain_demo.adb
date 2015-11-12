with
     openGL.Visual,
     openGL.Terrain,

     openGL.Demo,

     ada.Text_IO,
     ada.Exceptions;


procedure launch_large_Terrain_Demo
--
--  Exercise the culler with a large terrain grid.
--
is
   use openGL,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

begin
   openGL.Demo.define ("openGL 'large Terrin' Demo");

   --  Setup the camera.
   --
   Demo.Camera.Position_is ((0.0, 0.0, 5.0),
--     the_Camera.Position_is ((0.0, 0.0, 350.0),
                            y_Rotation_from (to_Radians (0.0)));


   declare
      use openGL.Math.Functions;

--        heights_File     : constant String := "assets/kidwelly-terrain-255x255.png";
      heights_File     : constant String             := "assets/kidwelly-terrain-510x510.png";
--        heights_File     : constant String := "assets/kidwelly-terrain-1000x1000.png";
--        heights_File     : constant String := "assets/kidwelly-terrain-2000x2000.png";
--        heights_File     : constant String := "assets/kidwelly-terrain-4000x4000.png";

      texture_File     : constant String             := "assets/kidwelly-terrain-texture-255x255.png";

      the_terrain_Grid : constant openGL.Visual.Grid := openGL.Terrain.new_Terrain (heights_File => heights_File,
                                                                                    texture_File => texture_File,
                                                                                    Scale        => (1.0, 20.0, 1.0));
      num_Sprites      : constant Positive           :=   the_terrain_Grid'Length (1)
                                                        * the_terrain_Grid'Length (2);
      the_Sprites      :          openGL.Visual.views (1 .. num_Sprites); --  := (others => new_Sprite (the_ground_Model));
      sprites_Last     :          Natural            := 0;

   begin
      for Row in the_terrain_Grid'Range (1)
      loop
         for Col in the_terrain_Grid'Range (2)
         loop
            sprites_Last               := sprites_Last + 1;
            the_Sprites (sprites_Last) := the_terrain_Grid (Row, Col);
         end loop;
      end loop;


      --  Main loop.
      --
      while not Demo.Done
      loop
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

         declare
            Command : Character;
            Avail   : Boolean;
         begin
            Demo.Dolly.get_last_Character (Command, Avail);

            if Avail
            then
               case Command
               is
               when others => null;
               end case;
            end if;
         end;

         Demo.Camera.render (the_Sprites (1 .. sprites_Last));

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.
      end loop;
   end;

   Demo.destroy;
   new_Line;


exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main task !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_large_Terrain_Demo;
