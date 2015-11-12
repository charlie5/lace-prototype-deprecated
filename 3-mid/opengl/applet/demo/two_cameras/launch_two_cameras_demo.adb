with
     openGL.Camera,
     openGL.Palette,
     openGL.Texture,
     openGL.Model.box .lit_colored_textured,
     openGL.Model.sphere.lit_colored_textured,
     openGL.Visual,
     openGL.Demo,

     ada.Text_IO,
     ada.Exceptions;


procedure launch_two_Cameras_Demo
--
--  Exercise the culler with two cameras.
--
is
   use openGL,
       openGL.Model,
       openGL.Model.box,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

   the_Camera_2 : aliased  openGL.Camera.item;


begin
   openGL.Demo.define ("openGL 'two Cameras' Demo");


   --  Setup the extra camera.
   --
   the_Camera_2.define;
   the_Camera_2.Renderer_is (Demo.Renderer'unchecked_Access);

   the_Camera_2.Position_is ((0.0, 20.0, 0.0),
                             y_Rotation_from (to_Radians (0.0)));

   the_Camera_2.Viewport_is (width  => 1000,
                             height => 1000);


   -- Create the sprites.
   --
   declare
      use openGL.Math.Functions;

      --  The Models.
      --
      the_box_Model : constant openGL.Model.box.lit_colored_textured.view
        := openGL.Model.box.lit_colored_textured.forge.new_Box
             (scale         => (0.5, 0.5, 0.5),
              faces         => (front => (colors         => (others => (White,     Opaque)),
                                          texture_name   => to_Asset ("assets/mmi/Face1.bmp"),
                                          texture_object => Texture.null_Object),
                                rear  => (colors         => (others => (Blue,     Opaque)),
                                          texture_name   => to_Asset ("assets/mmi/Face1.bmp"),
                                          texture_object => Texture.null_Object),
                                upper => (colors         => (others => (Green,    Opaque)),
                                          texture_name   => to_Asset ("assets/mmi/Face1.bmp"),
                                          texture_object => Texture.null_Object),
                                lower => (colors         => (others => (Green,    Opaque)),
                                          texture_name   => to_Asset ("assets/mmi/Face1.bmp"),
                                          texture_object => Texture.null_Object),
                                left  => (colors         => (others => (Dark_Red, Opaque)),
                                          texture_name   => to_Asset ("assets/mmi/Face1.bmp"),
                                          texture_object => Texture.null_Object),
                                right => (colors         => (others => (Red,      Opaque)),
                                          texture_name   => to_Asset ("assets/mmi/Face1.bmp"),
                                          texture_object => Texture.null_Object)));

      the_ball_Model : constant openGL.Model.sphere.lit_colored_textured.view
        := openGL.Model.sphere.lit_colored_textured.forge.new_sphere (radius => 0.5);

      --  The Sprites.
      --
      the_Sprites   : constant openGL.Visual.views (1 .. 4_000) := (others => openGL.Visual.Forge.new_Visual (openGL.Model.view (the_box_Model)));
      the_Sprites_2 : constant openGL.Visual.views (1 .. 4_000) := (others => openGL.Visual.Forge.new_Visual (openGL.Model.view (the_ball_Model)));

      grid_Size     : constant openGL.Real                      := SqRt (openGL.Real (the_Sprites'Length));
      x             :          openGL.Real                      := -grid_Size / 2.0;
      z             :          openGL.Real                      := 0.0;

   begin
      Demo.Dolly.Speed_is (0.1);

      for i in the_Sprites'Range
      loop
         x := x + 1.0;

         if i mod Integer (SqRt (openGL.Real (the_Sprites'Length))) = 0
         then
            z := z - 1.0;
            x := -grid_Size / 2.0;
         end if;

         the_Sprites (i).Site_is ((x, 0.0, z));
      end loop;


      for i in the_Sprites_2'Range
      loop
         x := x + 1.2;

         if i mod Integer (SqRt (openGL.Real (the_Sprites_2'Length))) = 0
         then
            z := z - 1.0;
            x := -grid_Size / 2.0;
         end if;

         the_Sprites_2 (i).Site_is ((x, 0.0, z));
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

         Demo.Camera .render (the_Sprites);
         the_Camera_2.render (the_Sprites_2);

         while not (    Demo.Camera .cull_Completed
                    and the_Camera_2.cull_Completed)
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.

--           while the_Renderer.is_Busy
--           loop
--              delay Duration'Small; -- 1.0 / (60.0 * 1000.0);
--           end loop;
      end loop;
   end;

   Demo.destroy;
   the_Camera_2.destroy;
   new_Line;

exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_two_Cameras_Demo;
