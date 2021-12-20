with
     openGL.Model.text     .lit_colored,

     openGL.Model.sphere   .lit_colored_textured,
     openGL.Model.sphere   .lit_colored,
     openGL.Model.sphere   .textured,
     openGL.Model.sphere   .colored,

     openGL.Model.polygon  .lit_colored,

     openGL.Model.box      .colored,
     openGL.Model.box      .textured,

     openGL.Model.billboard.textured,
     openGL.Model.billboard.colored_textured,

     openGL.Model.arrow    .colored,
     openGL.Model.line     .colored,
     openGL.Model.segment_line,

     physics.Model,
     gel.Window;


package body gel.Forge
is

   -----------
   --- Applets
   --

   function new_gui_Applet (Named         : in String;
                            window_Width  : in Positive := 500;
                            window_Height : in Positive := 500;
                            space_Kind    : in physics.space_Kind := physics.Bullet) return gel.Applet.gui_world.view
   is
      the_Window : constant gel.Window.view
        := gel.Window.Forge.new_Window (Named,
                                        window_Width,
                                        window_Height);

      the_Applet : constant gel.Applet.gui_world.view
        := gel.Applet.gui_World.forge.new_Applet ("Applet." & Named,
                                                  the_Window,
                                                  space_Kind);
   begin
      return the_Applet;
   end new_gui_Applet;



   function new_gui_and_sim_Applet (Named         : in String;
                                    window_Width  : in Positive := 500;
                                    window_Height : in Positive := 500;
                                    space_Kind    : in physics.space_Kind := physics.Bullet) return gel.Applet.gui_and_sim_World.view
   is
      pragma Unreferenced (space_Kind);
      the_Window : constant gel.Window.view
        := gel.Window.Forge.new_Window ("Window." & Named,
                                        window_Width,
                                        window_Height);

      the_Applet : constant gel.Applet.gui_and_sim_World.view
        := gel.Applet.gui_and_sim_World.forge.new_Applet ("Applet." & Named,
                                                          the_Window);
   begin
      return the_Applet;
   end new_gui_and_sim_Applet;



   function new_server_Applet (Named         : in String;
                               window_Width  : in Positive := 500;
                               window_Height : in Positive := 500;
                               space_Kind    : in physics.space_Kind := physics.Bullet) return gel.Applet.server_world.view
   is
      the_Window : constant gel.Window.view
        := gel.Window.Forge.new_Window (Named,
                                        window_Width,
                                        window_Height);

      the_Applet : constant gel.Applet.server_world.view
        := gel.Applet.server_World.forge.new_Applet ("Applet." & Named,
                                                     the_Window,
                                                     space_Kind);
   begin
      return the_Applet;
   end new_server_Applet;



   function new_client_Applet (Named         : in String;
                               window_Width  : in Positive := 500;
                               window_Height : in Positive := 500;
                               space_Kind    : in physics.space_Kind := physics.Bullet) return gel.Applet.client_world.view
   is
      the_Window : constant gel.Window.view
        := gel.Window.Forge.new_Window (Named,
                                        window_Width,
                                        window_Height);

      the_Applet : constant gel.Applet.client_world.view
        := gel.Applet.client_World.forge.new_Applet ("Applet." & Named,
                                                     the_Window,
                                                     space_Kind);
   begin
      return the_Applet;
   end new_client_Applet;


   -----------
   --- Sprites
   --

   -- 2D
   --

   function new_circle_Sprite (in_World : in gel.World.view;
                               Site     : in math.Vector_2     := math.Origin_2D;
                               Mass     : in math.Real         := 1.0;
                               Friction : in math.Real         := 0.5;
                               Bounce   : in math.Real         := 0.5;
                               Radius   : in math.Real         := 0.5;
                               Color    : in openGL.Color      := opengl.Palette.White;
                               Texture  : in openGL.asset_Name := openGL.null_Asset) return gel.Sprite.view
   is
      use openGL;
      use type Vector_2;

      the_graphics_Model : openGL.Model.sphere.view;

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info  => (physics.Model.Circle, Radius),
                                                  Mass        => Mass,
                                                  Friction    => Friction,
                                                  Restitution => Bounce);
                                                  --  Site        => Vector_3 (Site & 0.0));
   begin
      if Texture = openGL.null_Asset
      then
         the_graphics_Model := openGL.Model.sphere.lit_colored.new_Sphere (Radius,
                                                                           Color => (Color, openGL.Opaque)).all'Access;
      else
         the_graphics_Model := openGL.Model.sphere.lit_colored_textured.new_Sphere (Radius,
                                                                                    Image => Texture).all'Access;
      end if;

      return gel.Sprite.Forge.new_Sprite ("circle_Sprite",
                                          sprite.World_view (in_World),
                                          Vector_3 (Site & 0.0),
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_graphics => True,
                                          owns_physics  => True,
                                          is_Kinematic  => False);
   end new_circle_Sprite;



   function new_polygon_Sprite (in_World : in gel.World.view;
                                Site     : in math.Vector_2 := math.Origin_2D;
                                Mass     : in math.Real     := 1.0;
                                Friction : in math.Real     := 0.5;
                                Bounce   : in math.Real     := 0.5;
                                Vertices : in Geometry_2d.Sites;
                                Color    : in openGL.Color  := opengl.Palette.White) return gel.Sprite.view
   is
      use Math;
      use type Geometry_2d.Sites;

      the_graphics_Model : constant openGL.Model.polygon.lit_colored.view
        := openGL.Model.polygon.lit_colored.new_Polygon (openGL.Vector_2_array (Vertices),
                                                         (Color, openGL.Opaque));

      Padding            : constant Geometry_2d.Sites (1 .. 8 - Vertices'Length) := (others => <>);

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info  => (physics.Model.Polygon,
                                                                  vertex_Count => Vertices'Length,
                                                                  Vertices     => Vertices & Padding),
                                                  --  Site        => Vector_3 (Site & 0.0),
                                                  Mass        => Mass,
                                                  Friction    => Friction,
                                                  Restitution => Bounce);
   begin
      return gel.Sprite.Forge.new_Sprite ("polygon_Sprite",
                                          sprite.World_view (in_World),
                                          Vector_3 (Site & 0.0),
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_graphics => True,
                                          owns_physics  => True,
                                          is_Kinematic  => False);
   end new_polygon_Sprite;



   function new_rectangle_Sprite (in_World : in gel.World.view;
                                  Site     : in math.Vector_2 := math.Origin_2D;
                                  Mass     : in math.Real     := 1.0;
                                  Friction : in math.Real     := 0.5;
                                  Bounce   : in math.Real     := 0.5;
                                  Width,
                                  Height   : in math.Real;
                                  Color    : in openGL.Color  := opengl.Palette.White) return gel.Sprite.view
   is
      use Math;

      half_Width   : constant Real := Width  / 2.0;
      half_Height  : constant Real := Height / 2.0;

      the_Vertices : constant Geometry_2d.Sites (1 .. 4) := ((-half_Width, -half_Height),
                                                             ( half_Width, -half_Height),
                                                             ( half_Width,  half_Height),
                                                             (-half_Width,  half_Height));
   begin
      return new_polygon_Sprite (in_World, Site, Mass, Friction, Bounce, the_Vertices, Color);
   end new_rectangle_Sprite;



   -- 3D
   --

   function new_ball_Sprite (in_World   : in gel.World.view;
                             Site       : in math.Vector_3      := math.Origin_3D;
                             Mass       : in math.Real          := 1.0;
                             Radius     : in math.Real          := 0.5;
                             lat_Count  : in Positive           := openGL.Model.sphere.default_latitude_Count;
                             long_Count : in Positive           := openGL.Model.sphere.default_longitude_Count;
                             is_Lit     : in Boolean            := True;
                             Color      : in openGL.lucid_Color := opengl.no_lucid_Color;
                             Texture    : in openGL.asset_Name  := openGL.null_Asset) return gel.Sprite.view
   is
      use type openGL.lucid_Color;

      the_graphics_Model : openGL.Model.sphere.view;

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (physics.Model.a_Sphere, Radius),
                                                  Mass       => Mass);
   begin
      if is_Lit     -- TODO: Remaining combinations.
      then
         the_graphics_Model := openGL.Model.sphere.lit_colored_textured.new_Sphere (Radius,
                                                                                    lat_Count  => lat_Count,
                                                                                    long_Count => long_Count,
                                                                                    Image      => Texture).all'Access;
      else
         if Color /= openGL.no_lucid_Color
         then
            the_graphics_Model := openGL.Model.sphere.colored.new_Sphere (Radius,
                                                                          lat_Count  => lat_Count,
                                                                          long_Count => long_Count,
                                                                          Color      => Color).all'Access;
         else
            the_graphics_Model := openGL.Model.sphere.textured.new_Sphere (Radius,
                                                                           lat_Count  => lat_Count,
                                                                           long_Count => long_Count,
                                                                           Image      => Texture).all'Access;
         end if;
      end if;

      return gel.Sprite.Forge.new_Sprite ("ball_Sprite",
                                          sprite.World_view (in_World),
                                          Site,
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_Graphics => True,
                                          owns_Physics  => True,
                                          is_Kinematic  => False);
   end new_ball_Sprite;



   function new_skysphere_Sprite (in_World : in gel.World.view;
                                  Site     : in math.Vector_3 := math.Origin_3D;
                                  Radius   : in math.Real     := 1_000_000.0;
                                  Texture  : in openGL.asset_Name) return gel.Sprite.view
   is
      the_graphics_Model : openGL.Model.sphere.view;

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (physics.Model.a_Sphere, Radius),
                                                  Mass       => 0.0);
   begin
      the_graphics_Model := openGL.Model.sphere.textured.new_Sphere (Radius,
                                                                     lat_Count    => 180,
                                                                     Image        => Texture,
                                                                     is_Skysphere => True).all'Access;
      return gel.Sprite.Forge.new_Sprite ("skysphere_Sprite",
                                          sprite.World_view (in_World),
                                          Site,
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_Graphics => True,
                                          owns_Physics  => True,
                                          is_Kinematic  => False);
   end new_skysphere_Sprite;



   function new_box_Sprite (in_World     : in gel.World.view;
                            Site         : in math.Vector_3 := math.Origin_3D;
                            Mass         : in math.Real     := 1.0;
                            Size         : in math.Vector_3 := (1.0, 1.0, 1.0);
                            Colors       : in box_Colors    := (others => opengl.Palette.random_Color);
                            is_Kinematic : in Boolean       := False) return gel.Sprite.view
   is
      use openGL.Model.box,
          openGL,
          Math;

      the_box_Model : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.new_Box (Size => Size,
                                             Faces => (Front => (Colors => (others => (Colors (1), Opaque))),
                                                       Rear  => (Colors => (others => (Colors (2), Opaque))),
                                                       Upper => (Colors => (others => (Colors (3), Opaque))),
                                                       Lower => (Colors => (others => (Colors (4), Opaque))),
                                                       Left  => (Colors => (others => (Colors (5), Opaque))),
                                                       Right => (Colors => (others => (Colors (6), Opaque)))));
      the_box_physics_Model : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                 half_Extents => Size / 2.0),
                                                                 --  half_Extents => the_box_Model.Scale / 2.0),
                                                  Mass       => Mass);
      the_Box : constant gel.Sprite.view
        := gel.Sprite.Forge.new_Sprite ("demo.Box",
                                        sprite.World_view (in_World),
                                        Site,
                                        the_box_Model.all'Access,
                                        the_box_physics_Model,
                                        owns_Graphics => True,
                                        owns_Physics  => True,
                                        is_Kinematic  => is_Kinematic);
   begin
      return the_Box;
   end new_box_Sprite;



   function new_box_Sprite (in_World : in gel.World.view;
                            Site     : in math.Vector_3 := math.Origin_3D;
                            Mass     : in math.Real     := 1.0;
                            Size     : in math.Vector_3 := (1.0, 1.0, 1.0);
                            Texture  : in openGL.asset_Name) return gel.Sprite.view
   is
      use openGL.Model.box,
          Math;

      the_box_Model : constant openGL.Model.box.textured.view
        := openGL.Model.box.textured.new_Box (Size => Size,
                                              Faces => (Front => (texture_Name => Texture),
                                                        Rear  => (texture_Name => Texture),
                                                        Upper => (texture_Name => Texture),
                                                        Lower => (texture_Name => Texture),
                                                        Left  => (texture_Name => Texture),
                                                        Right => (texture_Name => Texture)));
      the_box_physics_Model : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                 half_Extents => Size / 2.0),
                                                                 --  half_Extents => the_box_Model.Scale / 2.0),
                                                  Mass       => Mass);
      the_Box : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("demo.Box",
                                        sprite.World_view (in_World),
                                        Site,
                                        the_box_Model.all'Access,
                                        the_box_physics_Model,
                                        owns_graphics => True,
                                        owns_physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Box;
   end new_box_Sprite;



   function new_billboard_Sprite (in_World : in gel.World.view;
                                  Site     : in math.Vector_3     := math.Origin_3D;
                                  Mass     : in math.Real         := 1.0;
                                  Size     : in math.Vector_3     := (1.0, 1.0, 1.0);
                                  Texture  : in openGL.asset_Name := openGL.null_Asset) return gel.Sprite.view
   is
      use Math;

      the_billboard_Model : constant openGL.Model.billboard.textured.view
        := openGL.Model.billboard.textured.forge.new_Billboard (Size    => (Width  => Size (1),
                                                                            Height => Size (2)),
                                                                Plane   => openGL.Model.Billboard.xy,
                                                                Texture => Texture);

      the_billboard_physics_Model : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                 half_Extents => Size / 2.0),
                                                                 --  half_Extents => the_billboard_Model.Scale / 2.0),
                                                  Mass       => Mass);

      the_Billboard : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("Billboard",
                                        sprite.World_view (in_World),
                                        Site,
                                        the_billboard_Model.all'Access,
                                        the_billboard_physics_Model,
                                        owns_Graphics => True,
                                        owns_Physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Billboard;
   end new_billboard_Sprite;



   function new_billboard_Sprite (in_World : in gel.World.view;
                                  Site     : in math.Vector_3 := math.Origin_3D;
                                  Color    : in openGL.lucid_Color;
                                  Mass     : in math.Real         := 1.0;
                                  Size     : in math.Vector_3     := (1.0, 1.0, 1.0);
                                  Texture  : in openGL.asset_Name := openGL.null_Asset) return gel.Sprite.view
   is
      use Math;

      the_billboard_Model : constant openGL.Model.billboard.colored_textured.view
        := openGL.Model.billboard.colored_textured.new_Billboard (Size    => (Width  => Size (1),
                                                                              Height => Size (2)),
                                                                  Plane   => openGL.Model.Billboard.xy,
                                                                  Texture => Texture,
                                                                  Color   => Color);
      the_billboard_physics_Model : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                 half_Extents => Size / 2.0),
                                                                 --  half_Extents => the_billboard_Model.Scale / 2.0),
                                                  Mass       => Mass);
      the_Billboard : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("Billboard",
                                        sprite.World_view (in_World),
                                        Site,
                                        the_billboard_Model.all'Access,
                                        the_billboard_physics_Model,
                                        owns_Graphics => True,
                                        owns_Physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Billboard;
   end new_billboard_Sprite;



   function new_arrow_Sprite (in_World   : in gel.World.view;
                              Site       : in math.Vector_3      := math.Origin_3D;
                              Mass       : in math.Real          := 0.0;
                              Size       : in math.Vector_3      := (1.0, 1.0, 1.0);
                              Texture    : in openGL.asset_Name  := openGL.null_Asset;
                              Color      : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                              line_Width : in openGL.Real        := openGL.Primitive.unused_line_Width) return gel.Sprite.view
   is
      pragma Unreferenced (Texture);
      use Math;

      the_graphics_Model : constant openGL.Model.arrow.colored.view
        := openGL.Model.arrow.colored.new_Arrow (Color      => Color.primary,
                                                 line_Width => line_Width);

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                 half_Extents => Size / 2.0),
                                                                 --  half_Extents => the_graphics_Model.Scale / 2.0),
                                                  Mass       => Mass);
      the_Arrow : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("Arrow",
                                        sprite.World_view (in_World),
                                        Site,
                                        the_graphics_Model.all'Access,
                                        the_physics_Model,
                                        owns_Graphics => True,
                                        owns_Physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Arrow;
   end new_arrow_Sprite;



   function new_line_Sprite  (in_World   : in gel.World.view;
                              Site       : in math.Vector_3      := math.Origin_3D;
                              Mass       : in math.Real          := 0.0;
                              Size       : in math.Vector_3      := (1.0, 1.0, 1.0);
                              Texture    : in openGL.asset_Name  := openGL.null_Asset;
                              Color      : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                              line_Width : in openGL.Real        := openGL.Primitive.unused_line_Width) return gel.Sprite.view
   is
      pragma Unreferenced (Texture, line_Width);
      use Math;

      the_graphics_Model : constant openGL.Model.line.colored.view
        := openGL.Model.line.colored.new_line_Model (Color => Color.primary);

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                 half_Extents => Size / 2.0),
                                                                 --  half_Extents => the_graphics_Model.Scale / 2.0),
                                                  Mass       => Mass);
      the_Line : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("Line",
                                        sprite.World_view (in_World),
                                        Site,
                                        the_graphics_Model.all'Access,
                                        the_physics_Model,
                                        owns_Graphics => True,
                                        owns_Physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Line;
   end new_line_Sprite;



   function new_segment_line_Sprite  (in_World   : in gel.World.view;
                                      Site       : in math.Vector_3      := math.Origin_3D;
                                      Mass       : in math.Real          := 0.0;
                                      Size       : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Texture    : in openGL.asset_Name  := openGL.null_Asset;
                                      Color      : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                                      line_Width : in openGL.Real        := openGL.Primitive.unused_line_Width) return gel.Sprite.view
   is
      pragma Unreferenced (Texture, line_Width);
      use Math;

      the_graphics_Model : constant openGL.Model.segment_line.view
        := openGL.Model.segment_line.new_segment_line_Model (Color => Color.primary);

      the_physics_Model  : constant physics.Model.view
        := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                 half_Extents => Size / 2.0),
                                                  Mass       => Mass);
      the_Line : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("Line",
                                        sprite.World_view (in_World),
                                        Site,
                                        the_graphics_Model.all'Access,
                                        the_physics_Model,
                                        owns_Graphics => True,
                                        owns_Physics  => True,
                                        is_Kinematic  => False);
   begin
      return the_Line;
   end new_segment_line_Sprite;



   -- Text
   --

   function new_text_Sprite (in_World : in gel.World.view;
                             Site     : in math.Vector_3 := math.Origin_3D;
                             Text     : in String;
                             Font     : in openGL.Font.font_Id;
                             Color    : in openGL.Color  := opengl.Palette.Black;
                             Size     : in math.Vector_3 := (1.0, 1.0, 1.0);
                             Centered : in Boolean       := True) return gel.Sprite.view
   is
      use Math;
      use type Physics.space_Kind;

      the_graphics_Model : constant openGL.Model.text.lit_colored.view
        := openGL.Model.text.lit_colored.new_Text (Text     => Text,
                                                   Font     => Font,
                                                   Color    => (Color, openGL.Opaque),
                                                   Centered => Centered);
      the_physics_Model  : physics.Model.view;
   begin
      if in_World.space_Kind = Physics.Box2d
      then
         declare
            half_Width   : constant Real                       := Size (1) / 2.0;
            half_Height  : constant Real                       := Size (2) / 2.0;
            the_Vertices : constant Geometry_2d.Sites (1 .. 8) := ((-half_Width, -half_Height),
                                                                   ( half_Width, -half_Height),
                                                                   ( half_Width,  half_Height),
                                                                   (-half_Width,  half_Height),
                                                                   others => (0.0, 0.0));
         begin
            the_physics_Model := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Polygon,
                                                                                       Vertices     => the_Vertices,
                                                                                       vertex_Count => 4));
         end;
      else
         the_physics_Model := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                                    half_Extents => Size / 2.0));
                                                                                    --  half_Extents => the_graphics_Model.Scale));
      end if;

      return gel.Sprite.Forge.new_Sprite ("text_Sprite",
                                          sprite.World_view (in_World),
                                          Site,
                                          the_graphics_Model,
                                          the_physics_Model,
                                          owns_Graphics => True,
                                          owns_Physics  => True,
                                          is_Kinematic  => False);
   end new_text_Sprite;


end gel.Forge;
