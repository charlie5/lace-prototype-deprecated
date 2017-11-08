with
     openGL.Palette,
     openGL.Texture,
     openGL.Font,
     openGL.IO,
     openGL.Model.arrow    .colored,
     openGL.Model.billboard.        textured,
     openGL.Model.billboard.colored_textured,
     openGL.Model.box      .colored,
     openGL.Model.box  .lit_colored_textured,
     openGL.Model.capsule.lit_colored_textured,
     openGL.Model.grid,
     openGL.Model.hexagon       .lit_colored_textured,
     openGL.Model.hexagon_Column.lit_colored_textured_faceted,
     openGL.Model.hexagon_Column.lit_colored_textured_rounded,
     openGL.Model.line          .colored,
     openGL.Model.open_gl,
     openGL.Model.polygon       .lit_colored,
     openGL.Model.segment_line,
     openGL.Model.sphere        .colored,
     openGL.Model.sphere        .lit_colored,
     openGL.Model.sphere        .lit_colored_textured,
     openGL.Model.Text          .lit_colored_textured,
     openGL.Model.terrain;

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

      the_Texture : constant openGL.asset_Name   :=  to_Asset ("assets/opengl/texture/Face1.bmp");
      the_font_Id : constant openGL.Font.font_Id := (to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"),
                                                     24);

      the_arrow_Model : constant openGL.Model.arrow.colored.view
        := openGL.Model.arrow.colored.Forge.new_Arrow (End_2 => (0.0, 1.0, 0.0));

      the_ball_1_Model : constant openGL.Model.sphere.colored.view
        := openGL.Model.sphere.colored.forge.new_Sphere (Radius => 0.5, color => (Red, Opaque));

      the_ball_2_Model : constant openGL.Model.sphere.lit_colored.view
        := openGL.Model.sphere.lit_colored.forge.new_Sphere (Radius => 1.0, color => (Green, Opaque));

      the_ball_3_Model : constant openGL.Model.sphere.lit_colored_textured.view
        := openGL.Model.sphere.lit_colored_textured.forge.new_Sphere (Radius => 1.0, image => the_Texture);

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

      the_line_Model       : constant openGL.Model.line.colored.view
        := openGL.Model.line.colored.new_line_Model (color  => Red,
                                                     End_1 => (0.0, 0.0, 0.0),
                                                     End_2 => (5.0, 5.0, 0.0));

      the_collada_Model    : constant openGL.Model.open_gl.view   -- tbd: broken for human models
        := openGL.Model.open_gl.forge.new_Model (scale            => (1.0, 1.0, 1.0),
                                                 model => to_Asset ("assets/opengl/model/duck.dae"),
--                                                   model => to_Asset ("assets/opengl/model/deer.dae"),
                                                 math_model => null,
                                                 Texture          => the_Texture,
                                                 Texture_is_lucid => False);

      the_wavefront_Model  : constant openGL.Model.open_gl.view
        := openGL.Model.open_gl.forge.new_Model (scale => (1.0, 1.0, 1.0),
                                                 model => to_Asset ("assets/opengl/model/human.obj"),
                                                 math_model => null,
                                                 Texture          => the_Texture,
                                                 Texture_is_lucid => False);

      the_polygon_Model    : constant openGL.Model.polygon.lit_colored.view
        := openGL.Model.polygon.lit_colored.forge.new_Polygon (vertices => (Origin_2d, (1.0, 0.0), (1.0, 1.0), (-1.0, 0.5)),
                                                               color    => (Red, Opaque));

      the_text_Model       : constant openGL.Model.Text.lit_colored_textured.view
        := openGL.Model.Text.lit_colored_textured.forge.new_Text (scale => (1.0, 1.0, 1.0),
                                                                  text     => "Howdy",
                                                                  Font     => the_font_Id,
                                                                  Color    => (Green, Opaque),
                                                                  Centered => False);

      the_segment_line_Model : constant openGL.Model.segment_line.view
        := openGL.Model.segment_line.new_segment_line_Model (scale => (1.0, 1.0, 1.0),
                                                             color => Green);

      heights_File : constant String := "assets/opengl/terrain/kidwelly-terrain.png";
      texture_File : constant String := "assets/opengl/terrain/kidwelly-terrain-texture.png";

      the_Region   : constant opengl.io.height_Map_view   := opengl.io.to_height_Map (heights_File, 10.0);
      Tiling       : constant opengl.texture_Transform_2d := (s => (0.0, 1.0),
                                                              t => (0.0, 1.0));
      the_ground_Model : constant access openGL.Model.terrain.item
        := new openGL.Model.terrain.item' (openGL.Model.item with
                                           heights_asset => to_Asset (heights_File),
                                           row           => 1,
                                           col           => 1,
                                           heights       => the_Region.all'Access,
                                           color_map     => to_Asset (texture_File),
                                           tiling        => Tiling);
   begin
      Demo.Renderer.add_Font (the_font_Id);

      the_segment_line_Model.add_1st_Segment (start_Site => (0.0, 0.0, 0.0),
                                              end_Site   => (1.0, 1.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (0.0, 1.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (2.0, 2.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (0.0, 2.0, 0.0));

      return (            the_arrow_Model.all'Access,
                         the_ball_1_Model.all'Access,
                         the_ball_2_Model.all'Access,
                         the_ball_3_Model.all'Access,

                      the_billboard_Model.all'Access,
              the_colored_billboard_Model.all'Access,
                          the_box_1_Model.all'Access,
                          the_box_2_Model.all'Access,

                        the_capsule_Model.all'Access,
                           the_grid_Model.all'Access,

                             the_hexagon_Model.all'Access,
              the_faceted_hexagon_column_Model.all'Access,
              the_rounded_hexagon_column_Model.all'Access,

                                the_line_Model.all'Access,
                             the_collada_Model.all'Access,
                           the_wavefront_Model.all'Access,

                             the_polygon_Model.all'Access,
                                the_text_Model.all'Access,
                        the_segment_line_Model.all'Access,

                              the_ground_Model.all'Access);

                          end Models;


   procedure layout (the_Visuals : in openGL.Visual.views)
   is
      initial_X : constant openGL.Real := -6.0;
      initial_Y : constant openGL.Real :=  6.0;

      X    :          openGL.Real := initial_X;
      Y    :          openGL.Real := initial_Y;
      Pad  : constant openGL.Real := 3.0;

   begin
      the_Visuals (1).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (2).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (3).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (4).Site_is ((X, Y, 0.0));

      X := initial_X;
      Y := Y - Pad;

      the_Visuals (5).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (6).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (7).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (8).Site_is ((X, Y, 0.0));

      X := initial_X;
      Y := Y - Pad;

      the_Visuals  (9).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (10).Site_is ((X, Y, 0.0));   X := X + Pad;

      X := initial_X;
      Y := Y - Pad;

      the_Visuals (11).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (12).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (13).Site_is ((X, Y, 0.0));   X := X + Pad;

      X := initial_X;
      Y := Y - Pad;

      the_Visuals (14).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (15).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (16).Site_is ((X, Y, 0.0));   X := X + Pad;

      X := initial_X;
      Y := Y - Pad;

      the_Visuals (17).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (18).Site_is ((X, Y, 0.0));   X := X + Pad;
      the_Visuals (19).Site_is ((X, Y, 0.0));   X := X + Pad;

      X := initial_X;
      Y := Y - Pad;

      the_Visuals (20).Site_is ((X, Y, 0.0));   X := X + Pad;

   end layout;


end openGL.Demo;
