with
     openGL.Visual,
     openGL.Palette,
     openGL.Texture,
     openGL.IO,
     openGL.Font,

     openGL.Model.arrow.colored,
     openGL.Model.billboard.textured,
     openGL.Model.box      .colored,
     openGL.Model.box      .lit_colored_textured,
     openGL.Model.capsule  .lit_colored_textured,
     openGL.Model.grid,
     openGL.Model.hexagon  .lit_colored_textured,
     openGL.Model.hexagon_Column.lit_colored_textured_faceted,
     openGL.Model.hexagon_Column.lit_colored_textured_rounded,
     openGL.Model.line     .colored,
     openGL.Model.open_gl,
     openGL.Model.polygon  .lit_colored,
     openGL.Model.segment_line,
     openGL.Model.sphere   .colored,
     openGL.Model.sphere   .lit_colored,
     openGL.Model.sphere   .lit_colored_textured,
     openGL.Model.Text     .lit_colored_textured,
     openGL.Model.terrain,

     openGL.Demo,

     ada.Text_IO,
     ada.Unchecked_Deallocation,
     ada.Exceptions;


procedure launch_render_Models
--
--  Exercise the renderer with a set of models.
--
is
   use openGL,
       openGL.Model,
       openGL.Model.box,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

   the_font_Id : constant openGL.Font.font_Id := (to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"), 24);
   the_Texture : constant openGL.asset_Name   :=  to_Asset ("assets/opengl/texture/Face1.bmp");

begin
   openGL.Demo.define ("openGL 'render Models' Demo");

   --  Setup the camera.
   --
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));

--     the_Font := openGL.Font.texture.new_Font_texture ("assets/opengl/font/LiberationMono-Regular.ttf");
--     Success  := the_Font.FaceSize (51,  78, 95);

   Demo.Renderer.add_Font (the_font_Id);

   declare
      --  The Models.
      --
      the_arrow_Model : constant openGL.Model.arrow.colored.view
        := openGL.Model.arrow.colored.Forge.new_Arrow (Color      => Radical_Red,
                                                       line_Width => 2.0,
                                                       End_1      => (0.0, 0.0, 0.0),
                                                       End_2      => (5.0, 0.0, 0.0));

      the_billboard_Model : constant openGL.Model.billboard.textured.view
        := openGL.Model.billboard.textured.forge.new_Billboard (scale => (1.0, 1.0, 1.0),
                                                                plane   => Billboard.xy,
                                                                texture => the_Texture);

      the_box_Model_1     : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.forge.new_Box
             (scale => (1.0, 2.0, 3.0),
              faces => (front => (colors => (others => (Blue,     Opaque))),
                        rear  => (colors => (others => (Blue,     Opaque))),
                        upper => (colors => (others => (Green,    Opaque))),
                        lower => (colors => (others => (Green,    Opaque))),
                        left  => (colors => (others => (Dark_Red, Opaque))),
                        right => (colors => (others => (Red,      Opaque)))));

      the_box_Model_2     : constant openGL.Model.box.lit_colored_textured.view
        := openGL.Model.box.lit_colored_textured.forge.new_Box
             (scale => (1.0, 2.0, 3.0),
--            (scale         => (1.0, 1.0, 1.0),
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

      the_ball_Model_1     : constant openGL.Model.sphere.colored.view
        := openGL.Model.sphere.colored.forge.new_Sphere (Radius => 0.5, color => (Red, Opaque));

      the_ball_Model_2     : constant openGL.Model.sphere.lit_colored.view
        := openGL.Model.sphere.lit_colored.forge.new_Sphere (Radius => 1.0, color => (Green, Opaque));

      the_ball_Model_3     : constant openGL.Model.sphere.lit_colored_textured.view
        := openGL.Model.sphere.lit_colored_textured.forge.new_Sphere (Radius => 1.0, image => the_Texture);


      the_capsule_Model    : constant openGL.Model.capsule.lit_colored_textured.view
        := openGL.Model.capsule.lit_colored_textured.forge.new_Capsule (radius => 1.0,
                                                                        height => 3.0,
                                                                        image  => the_Texture);
      the_grid_Model       : constant openGL.Model.grid.view
        := openGL.Model.grid.new_grid_Model (color  => Red,
                                             width  => 5,
                                             height => 5);

      the_hexagon_Model    : constant openGL.Model.hexagon.lit_colored_textured.view
        := openGL.Model.hexagon.lit_colored_textured.forge.new_Hexagon (scale => (1.0, 1.0, 1.0),
                                                                        face => (center_Color => (Green, Opaque),
                                                                                 colors       => (others => (Red, Opaque)),
                                                                                 texture      => <>)); --openGL.io.to_Texture ("assets/mmi/Face1.bmp")));

      the_faceted_hexagon_column_Model : constant openGL.Model.hexagon_Column.lit_colored_textured_faceted.view
        := openGL.Model.hexagon_Column.lit_colored_textured_faceted.forge.new_hexagon_Column
             (scale => (1.0, 1.0, 1.0),
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
             (scale => (1.0, 1.0, 1.0),
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
                                                 model => to_Asset ("assets/duck.dae"),
--                                                   model            => to_Asset ("assets/human-default.dae"),  -- tbd: Models with weights fail due to skinning and not setting opengl program parameters.
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
                                                                  Color    => (Red, Opaque),
                                                                  Centered => False);

      the_segment_line_Model : constant openGL.Model.segment_line.view
        := openGL.Model.segment_line.new_segment_line_Model (scale => (1.0, 1.0, 1.0),
                                                             color => Green);

      heights_File : constant String := "assets/kidwelly-terrain.png";
      texture_File : constant String := "assets/kidwelly-terrain-texture.png";

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
                                           tiling        => Tiling,
                                           bounds        => (ball => 1.0,
                                                             box  => <>));
      --  The Sprites.
      --
      use openGL.Visual.Forge;

      the_Sprites : constant openGL.Visual.views
        := (new_Visual (the_arrow_Model       .all'Access),
            new_Visual (the_box_Model_1       .all'Access),
            new_Visual (the_box_Model_2       .all'Access),
            new_Visual (the_ball_Model_1      .all'Access),
            new_Visual (the_ball_Model_2      .all'Access),
            new_Visual (the_ball_Model_3      .all'Access),
            new_Visual (the_billboard_Model   .all'Access),
            new_Visual (the_capsule_Model     .all'Access),
            new_Visual (the_grid_Model        .all'Access),
            new_Visual (the_hexagon_Model     .all'Access),
            new_Visual (the_faceted_hexagon_column_Model.all'Access),
            new_Visual (the_rounded_hexagon_column_Model.all'Access),
            new_Visual (the_line_Model        .all'Access),
--              new_Visual (the_collada_Model     .all'Access),
            new_Visual (the_wavefront_Model   .all'Access),
            new_Visual (the_polygon_Model     .all'Access),
            new_Visual (the_text_Model        .all'Access),
            new_Visual (the_segment_line_Model.all'Access));
--              new_Visual (the_ground_Model      .all'Access));

      Current     :          Integer := the_Sprites'First;


      procedure flip (Self : opengl.io.height_Map_view)
      is
         procedure free is new ada.Unchecked_Deallocation (opengl.height_Map,
                                                           opengl.IO.height_Map_view);
--           use type opengl.Index_t;
         Pad : opengl.io.height_Map_view := new opengl.height_Map' (Self.all);
      begin
         for Row in Self'Range (1)
         loop
            for Col in Self'Range (2)
            loop
               Self (Row, Col) := Pad (Self'Last (1) - Row + 1,  Col);
            end loop;
         end loop;

         free (Pad);
      end flip;


   begin
      flip (the_Region);

      the_segment_line_Model.add_1st_Segment (start_Site => (0.0, 0.0, 0.0),
                                              end_Site   => (1.0, 1.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (0.0, 1.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (2.0, 2.0, 0.0));
      the_segment_line_Model.add_Segment     (end_Site   => (0.0, 2.0, 0.0));


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
               when ' ' =>
                  if     Current  = the_Sprites'Last
                  then   Current := the_Sprites'First;
                  else   Current := Current + 1;
                  end if;

               when others =>
                  null;
               end case;
            end if;
         end;

         --  Render all sprites.
         --
         Demo.Camera.render ((1 => the_Sprites (Current)));

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
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_render_Models;
