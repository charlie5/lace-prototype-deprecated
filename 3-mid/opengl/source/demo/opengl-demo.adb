with
     openGL.Palette,
     openGL.Font,
     openGL.IO,
     openGL.Model.arrow    .colored,
     openGL.Model.billboard.        textured,
     openGL.Model.billboard.colored_textured,
     openGL.Model.box      .colored,
     openGL.Model.box      .textured,
     openGL.Model.box      .lit_colored_textured,
     openGL.Model.capsule  .lit_colored_textured,
     openGL.Model.grid,
     openGL.Model.hexagon       .lit_colored_textured,
     openGL.Model.hexagon_Column.lit_colored_textured_faceted,
     openGL.Model.hexagon_Column.lit_colored_textured_rounded,
     openGL.Model.line          .colored,
     openGL.Model.any,
     openGL.Model.polygon       .lit_colored,
     openGL.Model.segment_line,
     openGL.Model.sphere        .colored,
     openGL.Model.sphere        .lit_colored,
     openGL.Model.sphere        .lit_colored_textured,
     openGL.Model.Text          .lit_colored_textured,
     openGL.Model.terrain,
     ada.Text_IO;

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
      use Palette,
          linear_Algebra_3d;
   begin
      Lumen.Window.Create (Window,
                           width    => Width,
                           height   => Height,
                           name     => Name,
                           animated => True);

      Renderer.define;
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
      ada.Text_IO.new_Line;
   end destroy;


   function Models return openGL.Model.views
   is
      use Model,
          Palette;

      the_Texture : constant asset_Name   :=  to_Asset ("assets/opengl/texture/Face1.bmp");
      the_font_Id : constant Font.font_Id := (to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"),
                                              Size => 24);

      the_arrow_Model : constant Model.arrow.colored.view
        := Model.arrow.colored.new_Arrow (End_2 => (0.0, 1.0, 0.0));

      the_ball_1_Model : constant Model.sphere.colored.view
        := Model.sphere.colored.new_Sphere (Radius => 0.5, Color => (Red, Opaque));

      the_ball_2_Model : constant Model.sphere.lit_colored.view
        := Model.sphere.lit_colored.new_Sphere (Radius => 1.0, Color => (Green, Opaque));

      the_ball_3_Model : constant Model.sphere.lit_colored_textured.view
        := Model.sphere.lit_colored_textured.new_Sphere (Radius => 1.0, Image => the_Texture);

      the_billboard_Model : constant Model.billboard.textured.view
        := Model.billboard.textured.forge.new_Billboard (Scale   => (1.0, 1.0, 1.0),
                                                         Plane   => Billboard.xy,
                                                         Texture => the_Texture);

      the_colored_billboard_Model : constant Model.billboard.colored_textured.view
        := Model.billboard.colored_textured.new_Billboard (Scale   => (1.0, 1.0, 1.0),
                                                           Plane   => Billboard.xy,
                                                           Color   => (Green, Opaque),
                                                           Texture => the_Texture);
      use Model.box;

      the_box_1_Model : constant Model.box.colored.view
        := Model.box.colored.new_Box
             (Size => (1.0, 2.0, 3.0),
              Faces => (Front => (Colors => (others => (Blue,     Opaque))),
                        Rear  => (Colors => (others => (Blue,     Opaque))),
                        Upper => (Colors => (others => (Green,    Opaque))),
                        Lower => (Colors => (others => (Green,    Opaque))),
                        Left  => (Colors => (others => (Dark_Red, Opaque))),
                        Right => (Colors => (others => (Red,      Opaque)))));

      the_box_2_Model : constant Model.box.lit_colored_textured.view
        := Model.box.lit_colored_textured.new_Box
             (Size => (1.0, 2.0, 1.0),
              Faces => (Front => (Colors => (others => (Blue,     Opaque)),  texture_Name => the_Texture),
                        Rear  => (Colors => (others => (Blue,     Opaque)),  texture_Name => the_Texture),
                        Upper => (Colors => (others => (Green,    Opaque)),  texture_Name => the_Texture),
                        Lower => (Colors => (others => (Green,    Opaque)),  texture_Name => the_Texture),
                        Left  => (Colors => (others => (Dark_Red, Opaque)),  texture_Name => the_Texture),
                        Right => (Colors => (others => (Red,      Opaque)),  texture_Name => the_Texture)));

      the_box_3_Model : constant Model.box.textured.view
        := Model.box.textured.new_Box
             (Size => (1.0, 2.0, 3.0),
              Faces => (Front => (texture_Name => the_Texture),
                        Rear  => (texture_Name => the_Texture),
                        Upper => (texture_Name => the_Texture),
                        Lower => (texture_Name => the_Texture),
                        Left  => (texture_Name => the_Texture),
                        Right => (texture_Name => the_Texture)));

      the_capsule_Model : constant Model.capsule.lit_colored_textured.view
        := Model.capsule.lit_colored_textured.new_Capsule (Radius => 0.5,
                                                           Height => 2.0,
                                                           Color  => (White, Opaque),
                                                           Image  => the_Texture);
      the_grid_Model    : constant Model.grid.view
        := Model.grid.new_grid_Model (Color  => Red,
                                      Width  => 3,
                                      Height => 3);

      the_hexagon_Model : constant Model.hexagon.lit_colored_textured.view
        := Model.hexagon.lit_colored_textured.new_Hexagon (Radius => 0.25,
                                                           Face   => (center_Color => (Green, Opaque),
                                                                      Colors       => (others => (Red, Opaque)),
                                                                      Texture      => <>));

      the_faceted_hexagon_column_Model : constant Model.hexagon_Column.lit_colored_textured_faceted.view
        := Model.hexagon_Column.lit_colored_textured_faceted.new_hexagon_Column
             (Radius => 0.25,
              Height => 1.0,
              Upper => (center_Color => (Green, Opaque),
                        Colors       => (others => (Red, Opaque)),
                        Texture      => <>),
              Lower => (center_Color => (Green, Opaque),
                        Colors       => (others => (Red, Opaque)),
                        Texture      => <>),
              Shaft => (Color        => (Green, Opaque),
                        Texture      => the_Texture));

      the_rounded_hexagon_column_Model : constant Model.hexagon_Column.lit_colored_textured_rounded.view
        := Model.hexagon_Column.lit_colored_textured_rounded.new_hexagon_Column
             (Radius => 0.25,
              Height => 1.0,
              Upper => (center_Color => (Green, Opaque),
                        Colors       => (others => (Red, Opaque)),
                        Texture      => <>),
              Lower => (center_Color => (Green, Opaque),
                        Colors       => (others => (Red, Opaque)),
                        Texture      => <>),
              Shaft => (Color        => (White, Opaque),
                        Texture      => the_Texture));

      the_line_Model : constant Model.line.colored.view
        := Model.line.colored.new_line_Model (Color  => Red,
                                              End_1 => (0.0, 0.0, 0.0),
                                              End_2 => (5.0, 5.0, 0.0));

      the_collada_Model : constant Model.any.view
        := Model.any.new_Model (Scale            => (1.0, 1.0, 1.0),
                                Model            => to_Asset ("assets/opengl/model/human.dae"),
                                Texture          => the_Texture,
                                Texture_is_lucid => False);

      the_wavefront_Model : constant Model.any.view
        := Model.any.new_Model (Scale            => (1.0, 1.0, 1.0),
                                Model            => to_Asset ("assets/opengl/model/human.obj"),
                                Texture          => the_Texture,
                                Texture_is_lucid => False);

      the_polygon_Model : constant Model.polygon.lit_colored.view
        := Model.polygon.lit_colored.new_Polygon (Vertices => (Origin_2D, (1.0, 0.0), (1.0, 1.0), (-1.0, 0.5)),
                                                  Color    => (Red, Opaque));

      the_text_Model : constant Model.Text.lit_colored_textured.view
        := Model.Text.lit_colored_textured.new_Text (Scale => (1.0, 1.0, 1.0),
                                                     Text     => "Howdy",
                                                     Font     => the_font_Id,
                                                     Color    => (Green, Opaque),
                                                     Centered => False);

      the_segment_line_Model : constant Model.segment_line.view
        := Model.segment_line.new_segment_line_Model (Scale => (1.0, 1.0, 1.0),
                                                      Color => Green);

      -- Terrain
      --
      heights_File : constant asset_Name := to_Asset ("assets/opengl/terrain/kidwelly-terrain.png");
      texture_File : constant asset_Name := to_Asset ("assets/opengl/terrain/kidwelly-terrain-texture.png");

      the_Region   : constant IO.height_Map_view   := IO.to_height_Map (heights_File, 10.0);
      Tiling       : constant texture_Transform_2d := (S => (0.0, 1.0),
                                                       T => (0.0, 1.0));
      the_ground_Model : constant access Model.terrain.item
        := new Model.terrain.item' (openGL.Model.item with
                                    heights_Asset => heights_File,
                                    Row           => 1,
                                    Col           => 1,
                                    Heights       => the_Region.all'Access,
                                    Color_Map     => texture_File,
                                    Tiling        => Tiling);
   begin
      Demo.Renderer.add_Font (the_font_Id);

      the_segment_line_Model.add_1st_Segment (start_Site => (0.0, 0.0, 0.0),
                                              end_Site   => (1.0, 1.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (0.0, 1.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (2.0, 2.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (0.0, 2.0, 0.0));

      return (           the_ground_Model.all'Access,
                        the_polygon_Model.all'Access,
                           the_text_Model.all'Access,
                          the_arrow_Model.all'Access,
                         the_ball_1_Model.all'Access,
                         the_ball_2_Model.all'Access,
                         the_ball_3_Model.all'Access,

                      the_billboard_Model.all'Access,
              the_colored_billboard_Model.all'Access,
                          the_box_1_Model.all'Access,
                          the_box_2_Model.all'Access,
                          the_box_3_Model.all'Access,

                        the_capsule_Model.all'Access,
                           the_grid_Model.all'Access,

                             the_hexagon_Model.all'Access,
              the_faceted_hexagon_column_Model.all'Access,
              the_rounded_hexagon_column_Model.all'Access,

                                the_line_Model.all'Access,
                             the_collada_Model.all'Access,
                           the_wavefront_Model.all'Access,

                        the_segment_line_Model.all'Access);
   end Models;


   procedure layout (the_Visuals : in Visual.views)
   is
      initial_X : constant openGL.Real := -6.0;
      initial_Y : constant openGL.Real :=  6.0;

      X    :          openGL.Real := initial_X;
      Y    :          openGL.Real := initial_Y;
      Pad  : constant openGL.Real := 3.0;
      i    :          Positive    := 1;

      procedure set_next_Visual_Site
      is
      begin
         the_Visuals (i).Site_is ((X, Y, 0.0));

         i := i + 1;
         X := X + Pad;
      end set_next_Visual_Site;

      procedure new_Line
      is
      begin
         X := initial_X;
         Y := Y - Pad;
      end new_Line;

   begin
      set_next_Visual_Site;
      set_next_Visual_Site;
      set_next_Visual_Site;
      set_next_Visual_Site;

      new_Line;
      set_next_Visual_Site;
      set_next_Visual_Site;
      set_next_Visual_Site;
      set_next_Visual_Site;
      set_next_Visual_Site;

      new_Line;
      set_next_Visual_Site;
      set_next_Visual_Site;

      new_Line;
      set_next_Visual_Site;
      set_next_Visual_Site;
      set_next_Visual_Site;

      new_Line;
      set_next_Visual_Site;
      set_next_Visual_Site;
      set_next_Visual_Site;

      new_Line;
      set_next_Visual_Site;
      set_next_Visual_Site;
      set_next_Visual_Site;

      new_Line;
      set_next_Visual_Site;
   end layout;


   procedure print_Usage (append_Message : in String := "")
   is
      use ada.Text_IO;
   begin
      put_Line ("Camera controls: 'w' => Move forward");
      put_Line ("                 'z' => Move backward");
      put_Line ("                 'a' => Move left");
      put_Line ("                 's' => Move right");
      put_Line ("                 'e' => Move up");
      put_Line ("                 'd' => Move down");
      put_Line ("                 'W' => Rotate clockwise         about Z-axis");
      put_Line ("                 'Z' => Rotate counter-clockwise about Z-axis");
      put_Line ("                 'A' => Orbit  clockwise about   about Y-Axis");
      put_Line ("                 'S' => Orbit  counter clockwise about Y-axis");
      put_Line ("                 'E' => Rotate clockwise         about X-axis");
      put_Line ("                 'D' => Rotate counter clockwise about X-axis");
      put_Line ("                 'q' => Quit");

      new_Line;

      if append_Message /= ""
      then
         put_Line (append_Message);
         new_Line;
      end if;
   end print_Usage;


end openGL.Demo;
