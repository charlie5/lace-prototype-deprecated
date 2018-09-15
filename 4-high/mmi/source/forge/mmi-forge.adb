with
     openGL.Model.text     .lit_colored_textured,
     openGL.Model.sphere   .lit_colored_textured,
     openGL.Model.sphere   .lit_colored,
     openGL.Model.polygon  .lit_colored,
     openGL.Model.box      .colored,
     openGL.Model.box      .textured,
     openGL.Model.billboard.textured,
     openGL.Model.billboard.colored_textured,
     openGL.Model.arrow    .colored,
     openGL.Model.line     .colored,
     openGL.Model.segment_line,

     physics.Model,
     mmi.Window;


package body mmi.Forge
is

   ------------
   --- Applets
   --

   function new_gui_Applet (Named         : in String;
                            window_Width  : in Positive                 := 500;
                            window_Height : in Positive                 := 500;
                            space_Kind    : in physics.space_Kind := physics.Bullet) return mmi.Applet.gui_world.view
   is
      the_Window : constant mmi.Window.view
        := mmi.Window.Forge.new_Window (Named,
                                        window_Width,
                                        window_Height);

      the_Applet : constant mmi.Applet.gui_world.view
        := mmi.Applet.gui_World.forge.new_Applet ("Applet." & Named,
                                                  the_Window,
                                                  space_Kind);
   begin
      return the_Applet;
   end new_gui_Applet;



   function new_gui_and_sim_Applet (Named         : in String;
                                    window_Width  : in Positive                 := 500;
                                    window_Height : in Positive                 := 500;
                                    space_Kind    : in physics.space_Kind := physics.Bullet) return mmi.Applet.gui_and_sim_World.view
   is
      the_Window : constant mmi.Window.view
        := mmi.Window.Forge.new_Window ("Window." & Named,
                                        window_Width,
                                        window_Height);

      the_Applet : constant mmi.Applet.gui_and_sim_World.view
        := mmi.Applet.gui_and_sim_World.forge.new_Applet ("Applet." & Named,
                                                          the_Window);
   begin
      return the_Applet;
   end new_gui_and_sim_Applet;




   ------------
   --- Sprites
   --

   --- 2D
   --

   function new_circle_Sprite (in_World : in mmi.World.view;
                               Site     : in math.Vector_2 := math.Origin_2d;
                               Mass     : in math.Real     := 1.0;
                               Friction : in math.Real     := 0.5;
                               Bounce   : in math.Real     := 0.5;
                               Radius   : in math.Real     := 0.5;
                               Color    : in openGL.Color      := opengl.Palette.White;
                               Texture  : in openGL.asset_Name := openGL.null_Asset) return mmi.Sprite.view
   is
      use openGL;

      the_graphics_Model : openGL.Model.sphere.view;

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info  => (physics.Model.Circle, Radius),
                                                      Mass        => Mass,
                                                      Friction    => Friction,
                                                      Restitution => Bounce);
   begin
      if Texture = openGL.null_Asset
      then
         the_graphics_Model := openGL.Model.sphere.lit_colored.new_Sphere (Radius,
                                                                           (Color, openGL.Opaque)).all'Access;
      else
         the_graphics_Model := openGL.Model.sphere.lit_colored_textured.new_Sphere (Radius,
                                                                                    Texture).all'Access;
      end if;

      return mmi.Sprite.Forge.new_Sprite ("circle_Sprite",
                                          in_World,
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_graphics => True,
                                          owns_physics  => True,
                                          is_Kinematic  => False);
   end new_circle_Sprite;




   function new_polygon_Sprite (in_World : in mmi.World.view;
                                Site     : in math.Vector_2    := math.Origin_2d;
                                Mass     : in math.Real        := 1.0;
                                Friction : in math.Real        := 0.5;
                                Bounce   : in math.Real        := 0.5;
                                Vertices : in Geometry_2d.Sites;
                                Color    : in openGL.Color     := opengl.Palette.White) return mmi.Sprite.view
   is
      use Math;
      use type Geometry_2d.Sites;

      the_graphics_Model : constant openGL.Model.polygon.lit_colored.view
        := openGL.Model.polygon.lit_colored.new_Polygon (openGL.Vector_2_array (Vertices),
                                                         (Color, openGL.Opaque));

      Padding            : constant Geometry_2d.Sites (1 .. 8 - Vertices'Length) := (others => <>);

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info  => (physics.Model.Polygon,
                                                                      vertex_count => Vertices'Length,
                                                                      vertices     => Vertices & Padding),
                                                      Mass        => Mass,
                                                      Friction    => Friction,
                                                      Restitution => Bounce);
   begin
      return mmi.Sprite.Forge.new_Sprite ("polygon_Sprite",
                                          in_World,
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_graphics => True,
                                          owns_physics  => True,
                                          is_Kinematic  => False,
                                          Site          => Vector_3 (Site & 0.0));
   end new_polygon_Sprite;




   function new_rectangle_Sprite (in_World : in mmi.World.view;
                                  Site     : in math.Vector_2 := math.Origin_2d;
                                  Mass     : in math.Real     := 1.0;
                                  Friction : in math.Real     := 0.5;
                                  Bounce   : in math.Real     := 0.5;
                                  Width,
                                  Height   : in math.Real;
                                  Color    : in openGL.Color  := opengl.Palette.White) return mmi.Sprite.view
   is
      use Math;

      half_Width   : constant Real                       := Width  / 2.0;
      half_Height  : constant Real                       := Height / 2.0;
      the_Vertices : constant Geometry_2d.Sites (1 .. 4) := ((-half_Width, -half_Height),
                                                             ( half_Width, -half_Height),
                                                             ( half_Width,  half_Height),
                                                             (-half_Width,  half_Height));
   begin
      return new_polygon_Sprite (in_World, Site, Mass, Friction, Bounce, the_Vertices, Color);
   end new_rectangle_Sprite;





   --- 3D
   --

   function new_ball_Sprite (in_World : in mmi.World.view;
                             Mass     : in math.Real     := 1.0;
                             Radius   : in math.Real     := 0.5;
                             Color    : in openGL.Color  := opengl.Palette.White) return mmi.Sprite.view
   is
      the_graphics_Model : constant openGL.Model.sphere.lit_colored.view
        := openGL.Model.sphere.lit_colored.new_Sphere (Radius, (Color, openGL.Opaque));

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (physics.Model.a_Sphere, Radius / 2.0),
                                                      mass       => Mass);
   begin
      return mmi.Sprite.Forge.new_Sprite ("ball_Sprite",
                                          in_World,
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_graphics => True,
                                          owns_physics  => True,
                                          is_Kinematic  => False);
   end new_ball_Sprite;




   function new_box_Sprite (in_World     : in mmi.World.view;
                            Mass         : in math.Real     := 1.0;
                            Size         : in math.Vector_3 := (1.0, 1.0, 1.0);
                            Colors       : in box_Colors    := (others => opengl.Palette.random_Color);
                            is_Kinematic : in Boolean       := False) return mmi.Sprite.view
   is
      use openGL.Model.box,
          openGL, openGL.Palette,
          Math;

      the_box_Model         : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.new_Box (size => Size,
                                             faces => (front => (colors => (others => (Colors (1), Opaque))),
                                                       rear  => (colors => (others => (Colors (2), Opaque))),
                                                       upper => (colors => (others => (Colors (3), Opaque))),
                                                       lower => (colors => (others => (Colors (4), Opaque))),
                                                       left  => (colors => (others => (Colors (5), Opaque))),
                                                       right => (colors => (others => (Colors (6), Opaque)))));
      the_box_physics_Model : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                     half_extents => the_box_Model.Scale / 2.0),
                                                      mass       => Mass);
      the_Box               : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("demo.Box",
                                        in_World,
                                        the_box_Model.all'Access,
                                        the_box_physics_Model,
                                        owns_graphics => True,
                                        owns_physics  => True,
                                        is_Kinematic  => is_Kinematic);
   begin
      return the_Box;
   end new_box_Sprite;



   function new_box_Sprite (in_World : in mmi.World.view;
                            Mass     : in math.Real        := 1.0;
                            Size     : in math.Vector_3    := (1.0, 1.0, 1.0);
                            Texture  : in openGL.asset_Name) return mmi.Sprite.view
   is
      use openGL.Model.box,
          openGL, openGL.Palette,
          Math;

      the_box_Model         : constant openGL.Model.box.textured.view
        := openGL.Model.box.textured.new_Box (size => Size,
                                              faces => (front => (texture_Name => Texture),
                                                        rear  => (texture_Name => Texture),
                                                        upper => (texture_Name => Texture),
                                                        lower => (texture_Name => Texture),
                                                        left  => (texture_Name => Texture),
                                                        right => (texture_Name => Texture)));
      the_box_physics_Model : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                     half_extents => the_box_Model.Scale / 2.0),
                                                      mass       => Mass);
      the_Box               : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("demo.Box",
                                        in_World,
                                        the_box_Model.all'Access,
                                        the_box_physics_Model,
                                        owns_graphics => True,
                                        owns_physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Box;
   end new_box_Sprite;




   function new_billboard_Sprite (in_World : in mmi.World.view;
                                  Mass     : in math.Real         := 1.0;
                                  Size     : in math.Vector_3     := (1.0, 1.0, 1.0);
                                  Texture  : in openGL.asset_Name := openGL.null_Asset) return mmi.Sprite.view
   is
      use openGL.Model.box,
          openGL, openGL.Palette,
          Math;

      the_billboard_Model         : constant openGL.Model.billboard.textured.view
        := openGL.Model.billboard.textured.forge.new_Billboard (scale   => Size,
                                                                plane   => openGL.Model.Billboard.xy,
                                                                texture => Texture);

      the_billboard_physics_Model : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                     half_extents => the_billboard_Model.Scale / 2.0),
                                                      mass       => Mass);

      the_Billboard               : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("Billboard",
                                        in_World,
                                        the_billboard_Model.all'Access,
                                        the_billboard_physics_Model,
                                        owns_graphics => True,
                                        owns_physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Billboard;
   end new_billboard_Sprite;



   function new_billboard_Sprite (in_World : in mmi.World.view;
                                  Color    : in openGL.lucid_Color;
                                  Mass     : in math.Real          := 1.0;
                                  Size     : in math.Vector_3      := (1.0, 1.0, 1.0);
                                  Texture  : in openGL.asset_Name  := openGL.null_Asset) return mmi.Sprite.view
   is
      use openGL.Model.box,
          openGL, openGL.Palette,
          Math;

      the_billboard_Model         : constant openGL.Model.billboard.colored_textured.view
        := openGL.Model.billboard.colored_textured.new_Billboard (scale   => Size,
                                                                  plane   => openGL.Model.Billboard.xy,
                                                                  texture => Texture,
                                                                  color   => Color);
      the_billboard_physics_Model : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                     half_extents => the_billboard_Model.Scale / 2.0),
                                                      mass       => Mass);

      the_Billboard               : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("Billboard",
                                        in_World,
                                        the_billboard_Model.all'Access,
                                        the_billboard_physics_Model,
                                        owns_graphics => True,
                                        owns_physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Billboard;
   end new_billboard_Sprite;




   function new_arrow_Sprite (in_World   : in mmi.World.view;
                              Mass       : in math.Real          := 0.0;
                              Size       : in math.Vector_3      := (1.0, 1.0, 1.0);
                              Texture    : in openGL.asset_Name  := openGL.null_Asset;
                              Color      : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                              line_Width : in openGL.Real        := openGL.Primitive.unused_line_Width) return mmi.Sprite.view
   is
      use openGL.Model.box,
          openGL, openGL.Palette,
          Math;

      the_graphics_Model : constant openGL.Model.arrow.colored.view
        := openGL.Model.arrow.colored.new_Arrow (color      => Color.primary,
                                                 line_Width => line_Width);

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                     half_extents => the_graphics_Model.Scale / 2.0),
                                                      mass       => Mass);
      the_Arrow          : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("Arrow",
                                        in_World,
                                        the_graphics_Model.all'Access,
                                        the_physics_Model,
                                        owns_graphics => True,
                                        owns_physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Arrow;
   end new_arrow_Sprite;



   function new_line_Sprite  (in_World   : in mmi.World.view;
                              Mass       : in math.Real          := 0.0;
                              Size       : in math.Vector_3      := (1.0, 1.0, 1.0);
                              Texture    : in openGL.asset_Name  := openGL.null_Asset;
                              Color      : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                              line_Width : in openGL.Real        := openGL.Primitive.unused_line_Width) return mmi.Sprite.view
   is
      use openGL.Model.box,
          openGL, openGL.Palette,
          Math;

      the_graphics_Model : constant openGL.Model.line.colored.view
        := openGL.Model.line.colored. new_line_Model (color      => Color.primary);

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                     half_extents => the_graphics_Model.Scale / 2.0),
                                                      mass       => Mass);
      the_Line           : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("Line",
                                        in_World,
                                        the_graphics_Model.all'Access,
                                        the_physics_Model,
                                        owns_graphics => True,
                                        owns_physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Line;
   end new_line_Sprite;



   function new_segment_line_Sprite  (in_World   : in mmi.World.view;
                                      Mass       : in math.Real          := 0.0;
                                      Size       : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Texture    : in openGL.asset_Name  := openGL.null_Asset;
                                      Color      : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                                      line_Width : in openGL.Real        := openGL.Primitive.unused_line_Width) return mmi.Sprite.view
   is
      use openGL.Model.box,
          openGL,
          openGL.Palette,
          Math;

      the_graphics_Model : constant openGL.Model.segment_line.view
        := openGL.Model.segment_line.new_segment_line_Model (scale => (1.0, 1.0, 1.0),
                                                             color => Color.primary);

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                     half_extents => the_graphics_Model.Scale / 2.0),
                                                      mass       => Mass);
      the_Line           : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("Line",
                                        in_World,
                                        the_graphics_Model.all'Access,
                                        the_physics_Model,
                                        owns_graphics => True,
                                        owns_physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Line;
   end new_segment_line_Sprite;





   -- Text
   --

   function new_text_Sprite (in_World : in mmi.World.view;
                             Text     : in String;
                             Font     : in openGL.Font.font_Id;
                             Color    : in openGL.Color       := opengl.Palette.Black;
                             Scale    : in math.Vector_3      := (1.0, 1.0, 1.0);
                             Centered : in Boolean            := True) return mmi.Sprite.view
   is
      the_graphics_Model  : constant openGL.Model.text.lit_colored_textured.View
        := openGL.Model.text.lit_colored_textured.new_Text (scale    => Scale,
                                                            text     => Text,
                                                            font     => Font,
                                                            color    => (Color, openGL.Opaque),
                                                            centered => Centered);
      the_physics_Model   : physics.Model.view;
      use type Physics.space_Kind;
   begin
      if in_World.space_Kind = Physics.Box2d
      then
         declare
            use Math;

            half_Width   : constant Real                       := Scale (1) / 2.0;
            half_Height  : constant Real                       := Scale (2) / 2.0;
            the_Vertices : constant Geometry_2d.Sites (1 .. 8) := ((-half_Width, -half_Height),
                                                                   ( half_Width, -half_Height),
                                                                   ( half_Width,  half_Height),
                                                                   (-half_Width,  half_Height),
                                                                   others => (0.0, 0.0));
         begin
            the_physics_Model := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Polygon,
                                                                                           vertices     => the_Vertices,
                                                                                           vertex_Count => 4));
         end;
      else
         the_physics_Model := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                                        half_extents => the_graphics_Model.Scale));
      end if;

      return mmi.Sprite.Forge.new_Sprite ("text_Sprite",
                                          in_World,
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_graphics => True,
                                          owns_physics  => True,
                                          is_Kinematic  => False);
   end new_text_Sprite;


end mmi.Forge;
