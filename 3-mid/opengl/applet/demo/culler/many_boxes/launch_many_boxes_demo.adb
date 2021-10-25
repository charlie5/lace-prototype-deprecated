with
     openGL.Palette,
     openGL.Model.Box.lit_colored_textured,
     openGL.Visual,

     openGL.Demo;


procedure launch_many_Boxes_Demo
--
--  Exercise the culler with many boxes.
--
is
   use openGL,
       openGL.Model,
       openGL.Model.box,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d;

begin
   Demo.print_Usage;
   Demo.define ("openGL 'many Boxes' Demo");

   --  Setup the camera.
   --
   Demo.Camera.Position_is ((0.0, 0.0, 5.0),
                            y_Rotation_from (to_Radians (0.0)));

   declare
      Face          : constant asset_Name := to_Asset ("assets/Face1.bmp");

      the_box_Model : constant Box.lit_colored_textured.view
        := Box.lit_colored_textured.new_Box
          (size          => (0.5, 0.5, 0.5),
           faces         => (front => (colors => (others => (White,    Opaque)), texture_name => Face),
                             rear  => (colors => (others => (Blue,     Opaque)), texture_name => Face),
                             upper => (colors => (others => (Green,    Opaque)), texture_name => Face),
                             lower => (colors => (others => (Green,    Opaque)), texture_name => Face),
                             left  => (colors => (others => (Dark_Red, Opaque)), texture_name => Face),
                             right => (colors => (others => (Red,      Opaque)), texture_name => Face)));

      Size : constant Integer     :=  70;
      x    :          openGL.Real := -openGL.Real (Size) / 2.0;
      z    :          openGL.Real :=  0.0;

      Sprites : constant Visual.views (1 .. Size * Size) := (others => Visual.Forge.new_Visual (Model.view (the_box_Model)));

   begin
      for i in Sprites'Range
      loop
         x := x + 1.0;

         if i mod Size = 0
         then
            z := z - 1.0;
            x := -openGL.Real (Size) / 2.0;
         end if;

         Sprites (i).Site_is ((x, 0.0, z));
      end loop;


      --  Main loop.
      --
      while not Demo.Done
      loop
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

         Demo.Camera.render (Sprites);

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.
      end loop;
   end;

   Demo.destroy;
end launch_many_Boxes_Demo;
