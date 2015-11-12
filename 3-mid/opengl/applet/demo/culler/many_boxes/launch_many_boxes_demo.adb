with
     openGL.Palette,
     openGL.Texture,
     openGL.Model.box.lit_colored_textured,
     openGL.Visual,

     openGL.Demo,

     ada.Text_IO,
     ada.Exceptions;


procedure launch_many_boxes_Demo
--
--  Exercise the culler with many boxes.
--
is
   use openGL,
       openGL.Model,
       openGL.Model.box,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

begin
   openGL.Demo.define ("openGL 'many Boxes' Demo");

   --  Setup the camera.
   --
   Demo.Camera.Position_is ((0.0, 0.0, 5.0),
                            y_Rotation_from (to_Radians (0.0)));

   declare
      use openGL.Math.Functions;

      the_box_Model : constant openGL.Model.box.lit_colored_textured.view
        := openGL.Model.box.lit_colored_textured.forge.new_Box
          (scale         => (0.5, 0.5, 0.5),
           faces         => (front => (colors         => (others => (White,     Opaque)),
                                       texture_name   => to_Asset ("assets/Face1.bmp"),
                                       texture_object => Texture.null_Object),
                             rear  => (colors         => (others => (Blue,     Opaque)),
                                       texture_name   => to_Asset ("assets/Face1.bmp"),
                                       texture_object => Texture.null_Object),
                             upper => (colors         => (others => (Green,    Opaque)),
                                       texture_name   => to_Asset ("assets/Face1.bmp"),
                                       texture_object => Texture.null_Object),
                             lower => (colors         => (others => (Green,    Opaque)),
                                       texture_name   => to_Asset ("assets/Face1.bmp"),
                                       texture_object => Texture.null_Object),
                             left  => (colors         => (others => (Dark_Red, Opaque)),
                                       texture_name   => to_Asset ("assets/Face1.bmp"),
                                       texture_object => Texture.null_Object),
                             right => (colors         => (others => (Red,      Opaque)),
                                       texture_name   => to_Asset ("assets/Face1.bmp"),
                                       texture_object => Texture.null_Object)));

      --  The Sprites.
      --
--        the_Sprites : openGL.Sprite.views (1 .. 1) := (others => new_Sprite (the_box_Model_2));
--        the_Sprites : openGL.Sprite.views (1 .. 5_000) := (others => new_Sprite (the_box_Model_2));
      the_Sprites : constant openGL.Visual.views (1 .. 5_000) := (others => openGL.Visual.Forge.new_Visual (openGL.Model.view (the_box_Model)));
--        the_Sprites : openGL.Sprite.views (1 .. 10_000) := (others => new_Sprite (the_box_Model_2));

      grid_Size   : constant openGL.Real :=  SqRt (openGL.Real (the_Sprites'Length));
      x           :          openGL.Real := -grid_Size / 2.0;
      z           :          openGL.Real :=  0.0;

   begin
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

         Demo.Camera.render (the_Sprites);

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


   new_Line;

exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_many_boxes_Demo;
