with
     openGL.Palette;


package body openGL.Demo
is


   procedure my_context_Setter
   is
   begin
      Lumen.Window.make_Current (Window);
   end my_context_Setter;


   procedure my_Swapper
   is
   begin
      Lumen.Window.swap (Window);
   end my_Swapper;



   procedure define (Name   : in String;
                     Width  : in Positive := 1000;
                     Height : in Positive := 1000)
   is
      use openGL.Palette,
          linear_Algebra_3d;
   begin
      Lumen.Window.Create (Window,
                           width    => Width,
                           height   => Height,
                           name     => Name,
                           animated => True);

--        put_Line ("openGL Server: " & openGL.Server.Version);

      Renderer.define; --  := openGL.Renderer.lean.forge.to_Renderer;

      Renderer.Background_is (Grey);
      Renderer.Swapper_is    (my_Swapper'unrestricted_Access);

      Lumen.Window.Make_Non_Current (Window);

      Renderer.Context_Setter_is (my_context_Setter'unrestricted_Access);
      Renderer.start_Engine;


      Camera.define;
      Camera.Renderer_is (Renderer'unchecked_Access);

      Camera.Position_is ((0.0, 0.0, 5.0),
                          y_Rotation_from (to_Radians (0.0)));

      Camera.Viewport_is (width  => Width,
                          height => Height);

   end define;


   procedure destroy
   is
   begin
      Camera  .destroy;
      Renderer.stop_Engine;
   end destroy;





end openGL.Demo;
