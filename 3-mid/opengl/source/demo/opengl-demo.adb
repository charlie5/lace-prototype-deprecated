with
     openGL.Palette,
     openGL.Texture,
     openGL.Model.arrow    .colored,
     openGL.Model.billboard.        textured,
     openGL.Model.billboard.colored_textured,
     openGL.Model.box      .colored,
     openGL.Model.box  .lit_colored_textured,
     openGL.Model.capsule.lit_colored_textured,
     openGL.Model.grid,
     openGL.Model.hexagon       .lit_colored_textured,
     openGL.Model.hexagon_Column.lit_colored_textured_faceted,
     openGL.Model.hexagon_Column.lit_colored_textured_rounded;

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



   function Models return openGL.Model.views
   is
      use openGL,
          openGL.Model,
          openGL.Palette;

      the_Texture : constant openGL.asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");

      the_arrow_Model : constant openGL.Model.arrow.colored.view
        := openGL.Model.arrow.colored.Forge.new_Arrow (End_2 => (0.0, 1.0, 0.0));

      the_billboard_Model : constant openGL.Model.billboard.textured.view
        := openGL.Model.billboard.textured.forge.new_Billboard (scale   => (1.0, 1.0, 1.0),
                                                                plane   => Billboard.xy,
                                                                texture => the_Texture);

      the_colored_billboard_Model : constant openGL.Model.billboard.colored_textured.view
        := openGL.Model.billboard.colored_textured.forge.new_Billboard (scale   => (1.0, 1.0, 1.0),
                                                                        plane   => Billboard.xy,
                                                                        color   => (Green, Opaque),
                                                                        texture => the_Texture);
      use openGL.Model.box;

      the_box_1_Model : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.forge.new_Box
             (scale => (1.0, 2.0, 3.0),
              faces => (front => (colors => (others => (Blue,     Opaque))),
                        rear  => (colors => (others => (Blue,     Opaque))),
                        upper => (colors => (others => (Green,    Opaque))),
                        lower => (colors => (others => (Green,    Opaque))),
                        left  => (colors => (others => (Dark_Red, Opaque))),
                        right => (colors => (others => (Red,      Opaque)))));

      the_box_2_Model : constant openGL.Model.box.lit_colored_textured.view
        := openGL.Model.box.lit_colored_textured.forge.new_Box
             (scale => (1.0, 2.0, 3.0),
              faces => (front => (colors         => (others => (Blue,     Opaque)),
                                  texture_name   => the_Texture,
                                  texture_object => Texture.null_Object),
                        rear  => (colors         => (others => (Blue,     Opaque)),
                                  texture_name   => the_Texture,
                                  texture_object => Texture.null_Object),
                        upper => (colors         => (others => (Green,    Opaque)),
                                  texture_name   => the_Texture,
                                  texture_object => Texture.null_Object),
                        lower => (colors         => (others => (Green,    Opaque)),
                                  texture_name   => the_Texture,
                                  texture_object => Texture.null_Object),
                        left  => (colors         => (others => (Dark_Red, Opaque)),
                                  texture_name   => the_Texture,
                                  texture_object => Texture.null_Object),
                        right => (colors         => (others => (Red,      Opaque)),
                                  texture_name   => the_Texture,
                                  texture_object => Texture.null_Object)));

      the_capsule_Model : constant openGL.Model.capsule.lit_colored_textured.view
        := openGL.Model.capsule.lit_colored_textured.forge.new_Capsule (radius => 1.0,
                                                                        height => 2.0,
                                                                        image  => the_Texture);
      the_grid_Model       : constant openGL.Model.grid.view
        := openGL.Model.grid.new_grid_Model (color  => Red,
                                             width  => 3,
                                             height => 3);

      the_hexagon_Model    : constant openGL.Model.hexagon.lit_colored_textured.view
        := openGL.Model.hexagon.lit_colored_textured.forge.new_Hexagon (radius => 0.25,
                                                                        face   => (center_Color => (Green, Opaque),
                                                                                   colors       => (others => (Red, Opaque)),
                                                                                   texture      => <>)); --openGL.io.to_Texture ("assets/mmi/Face1.bmp")));

      the_faceted_hexagon_column_Model : constant openGL.Model.hexagon_Column.lit_colored_textured_faceted.view
        := openGL.Model.hexagon_Column.lit_colored_textured_faceted.forge.new_hexagon_Column
             (radius => 0.25,
              height => 1.0,
              upper => (center_Color => (Green, Opaque),
                        colors       => (others => (Red, Opaque)),
                        texture      => <>),
              lower => (center_Color => (Green, Opaque),
                        colors       => (others => (Red, Opaque)),
                        texture      => <>),
              Shaft => (color        => (Green, Opaque),
                        texture      => <>));

      the_rounded_hexagon_column_Model : constant openGL.Model.hexagon_Column.lit_colored_textured_rounded.view
        := openGL.Model.hexagon_Column.lit_colored_textured_rounded.forge.new_hexagon_Column
             (radius => 0.25,
              height => 1.0,
              upper => (center_Color => (Green, Opaque),
                        colors       => (others => (Red, Opaque)),
                        texture      => <>),
              lower => (center_Color => (Green, Opaque),
                        colors       => (others => (Red, Opaque)),
                        texture      => <>),
              Shaft => (color        => (Green, Opaque),
                        texture      => <>));

   begin
      return (            the_arrow_Model.all'Access,
                      the_billboard_Model.all'Access,
              the_colored_billboard_Model.all'Access,
                          the_box_1_Model.all'Access,
                          the_box_2_Model.all'Access,
                        the_capsule_Model.all'Access,
                           the_grid_Model.all'Access,
                             the_hexagon_Model.all'Access,
              the_faceted_hexagon_column_Model.all'Access,
              the_rounded_hexagon_column_Model.all'Access);
   end Models;


end openGL.Demo;
