with
     gel.Window.setup,
     gel.Applet.gui_and_sim_world,
     gel.Forge,
     gel.Sprite,

     physics.Model,

     openGL.Model.box    .colored,
     openGL.Model.sphere .textured,
     openGL.Model.capsule.textured,
     openGL.Model.any,
     openGL.Model.terrain,
     openGL.IO,
     openGL.Light,
     openGL.Palette;

pragma unreferenced (gel.Window.setup);


procedure launch_full_Demo
--
--  Drops a variety of shapes onto a terrain.
--
is
   use gel.Math,
       openGL,
       openGL.Model.box,
       opengl.Palette;

   the_Applet : constant gel.Applet.gui_and_sim_world.view := gel.Forge.new_gui_and_sim_Applet ("mixed Shapes", 1536, 864);


   text_line_Site : gel.Math.Vector_3 := (-10.0, 50.0, -60.0);

   procedure put_Line (Message : in String)
   is
      Text : constant gel.Sprite.view := gel.Forge.new_text_Sprite (the_Applet.gui_World,
                                                                    text_line_Site,
                                                                    Message,
                                                                    the_Applet.Font,
                                                                    White);
   begin
      the_Applet.gui_World.add (Text);
      text_line_Site (2) := text_line_Site (2) - 2.0;
   end put_Line;


   x : math.Real :=  0.0;
   y : math.Real := 10.0;

   -----------
   --  Terrain
   --

   --  Heightfield
   --
   function to_Heightfield (From : in openGL.height_Map) return physics.Heightfield
   is
      Result : physics.Heightfield (1 .. Integer (From'Last (1)),
                                    1 .. Integer (From'Last (2)));
      Last_i : constant Index_t := From'Last (1);
   begin
      for i in Result'Range (1)
      loop
         for j in Result'Range (1)
         loop
            Result (i, j) := math.Real (From (Last_i - Index_t (i) + 1,
                                              Index_t (j)));
         end loop;
      end loop;

      return Result;
   end to_Heightfield;


   terrain_Heights : constant openGL.asset_Name := to_Asset ("assets/gel/kidwelly-terrain.png");
   terrain_Texture : constant openGL.asset_Name := to_Asset ("assets/gel/kidwelly-terrain-texture.png");

   hs : constant := 1.0;

   gl_Heights : constant openGL.IO.height_Map_view := openGL.IO.to_height_Map (image_Filename => terrain_Heights,
                                                                               Scale          => 10.0);

   the_terrain_Model : constant openGL.Model.terrain.view
     := openGL.Model.terrain.new_Terrain (heights_Asset => terrain_Heights,
                                          Row           => 1,
                                          Col           => 1,
                                          Heights       => openGL.Model.terrain.height_Map_view (gl_Heights),
                                          color_Map     => terrain_Texture,
                                          Tiling        => (s => (0.0, 1.0),
                                                            t => (0.0, 1.0)));

   the_terrain_physics_Model : constant physics.Model.view
     := physics.Model.forge.new_physics_Model (shape_Info => (Kind         => physics.Model.heightfield,
                                                              Heights      => new physics.Heightfield' (to_Heightfield (gl_Heights.all)),
                                                              height_Range => (0.0, 200.0)),
                                               Scale      =>  (hs, 1.0, hs));
   the_Terrain : constant gel.Sprite.view
     := gel.Sprite.forge.new_Sprite ("demo.Terrain",
                                     the_Applet.gui_World.all'Access,
                                     Origin_3D,
                                     the_terrain_Model,
                                     the_terrain_physics_Model);
begin
   -- Setup the applet.
   --
   the_Applet.gui_Camera.Site_is ((0.0, 4.0, 30.0));                         -- Position the camera.
   the_Applet.sim_Camera.Site_is ((0.0, 4.0, 30.0));                         -- Position the camera.

   the_Applet.enable_simple_Dolly (in_World => the_Applet.sim_World.Id);     -- Enable user camera control via keyboard.
   the_Applet.Dolly.Speed_is (0.1);

   the_Applet.Renderer.Background_is (sky_Blue);

   the_Applet.sim_World.Gravity_is ((0.0, -9.8, 0.0));


   -- Camera controls text.
   --
   put_Line ("Camera Controls: - Use arrow and PgUp/PgDn keys to move.");
   put_Line ("         - Use Shift key to move faster.");
   put_Line ("                    - Use Ctrl  key to rotate instead of move.");
   put_Line ("                    - Use Alt   key to orbit  instead of move.");


   -- Set the lights position.
   --
   declare
      use openGL.Light;
      Light : openGL.Light.item := the_Applet.Renderer.new_Light;
   begin
      Light.Kind_is (Diffuse);
      Light.Site_is ((0.0, 100.0, 100.0));
      Light.ambient_Coefficient_is (0.2);

      the_Applet.Renderer.set (Light);
   end;


   -- Terrain.
   --
   the_Applet.sim_World.add (the_Terrain);     -- Add the terrain.


   -- Add several sprites of each shape.
   --
   for i in 1 .. 5
   loop
      declare
         --  Box
         --
         the_box_Model : constant openGL.Model.box.colored.view
           := openGL.Model.box.colored.new_Box (Size => (1.0, 1.0, 1.0),
                                                Faces => (Front => (Colors => (others => (Red,     Opaque))),
                                                          Rear  => (Colors => (others => (Green,   Opaque))),
                                                          Upper => (Colors => (others => (Violet,  Opaque))),
                                                          Lower => (Colors => (others => (Yellow,  Opaque))),
                                                          Left  => (Colors => (others => (Cyan,    Opaque))),
                                                          Right => (Colors => (others => (Magenta, Opaque)))));
         the_box_physics_Model : constant physics.Model.view
           := physics.Model.forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                                    half_Extents => the_box_Model.Size / 2.0),
                                                     Mass       => 1.0);

         the_Box : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Box",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3D,
                                           the_box_Model.all'Access,
                                           the_box_physics_Model);

         --  Ball
         --
         the_ball_physics_Model : constant physics.Model.view
           := physics.Model.forge.new_physics_Model (shape_Info => (Kind          => physics.Model.a_sphere,
                                                                    sphere_Radius => 0.5),
                                                     Mass       => 1.0);

         the_ball_Model : constant openGL.Model.sphere.textured.view
           := openGL.Model.sphere.textured.new_Sphere (Radius => 0.5,
                                                       Image  => openGL.to_Asset ("assets/gel/texture/earth_map.bmp"));
         the_Ball : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Ball",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3D,
                                           the_ball_Model,
                                           the_ball_physics_Model);

         --  Cone
         --
         the_cone_Model : constant openGL.Model.any.view
           := openGL.Model.any.new_Model (Model            => openGL.to_Asset ("assets/gel/model/unit_cone.obj"),
                                          Texture          => openGL.to_Asset ("assets/gel/Face1.bmp"),
                                          Texture_is_lucid => False);

         the_cone_physics_Model : constant physics.Model.view
           := physics.Model.forge.new_physics_Model (shape_Info => (Kind => physics.Model.cone),
                                                     Mass       => 1.0);
         the_Cone : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Cone",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3D,
                                           the_cone_Model.all'Access,
                                           the_cone_physics_Model);
         --  Capsule
         --
         the_capsule_Model : constant openGL.Model.capsule.textured.view
           := openGL.Model.capsule.textured.new_Capsule (Radius => 0.5,
                                                         Height => 1.0,
                                                         Image  => openGL.to_Asset ("assets/gel/Face1.bmp"));

         the_capsule_physics_Model : constant physics.Model.view
           := physics.Model.forge.new_physics_Model (shape_Info => (Kind         => physics.Model.a_Capsule,
                                                                    lower_Radius => 0.5,
                                                                    upper_Radius => 0.5,
                                                                    Height       => 1.0),
                                                     Mass       => 1.0);
         the_Capsule : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Capsule",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3D,
                                           the_capsule_Model.all'Access,
                                           the_capsule_physics_Model);

         --  multi_Sphere
         --
         the_multi_Sphere_Model : constant openGL.Model.capsule.textured.view
           := openGL.Model.capsule.textured.new_Capsule (Radius => 0.5,
                                                         Height => 0.0,
                                                         Image  => openGL.to_Asset ("assets/gel/golf_green-16x16.tga"));

         the_multi_Sphere_physics_Model : constant physics.Model.view
           := physics.Model.forge.new_physics_Model (shape_Info => (Kind  => physics.Model.multi_Sphere,
                                                                    Sites => new physics.Vector_3_array' ((-0.5, 0.0, 0.0),
                                                                                                          ( 0.5, 0.0, 0.0)),
                                                                    Radii => new gel.math.Vector' (1 => 0.5,
                                                                                                   2 => 0.5)),
                                                     Mass       => 1.0);

         the_multi_Sphere : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.multi_Sphere",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3D,
                                           the_multi_Sphere_Model.all'Access,
                                           the_multi_Sphere_physics_Model);

         --  Hull
         --
         s              : constant := 0.5;
         the_hull_Model : constant openGL.Model.box.colored.view
           := openGL.Model.box.colored.new_Box (Size  => (s * 2.0,  s * 2.0,  s * 2.0),
                                                Faces => (others => (others => (others => (Palette.random_Color, Opaque)))));

         the_hull_physics_Model : constant physics.Model.view
           := physics.Model.forge.new_physics_Model (shape_Info => (Kind   => physics.Model.hull,
                                                                    Points => new physics.Vector_3_array' ((-s, -s,  s),
                                                                                                           ( s, -s,  s),
                                                                                                           ( s,  s,  s),
                                                                                                           (-s,  s,  s),

                                                                                                           (-s, -s, -s),
                                                                                                           ( s, -s, -s),
                                                                                                           ( s,  s, -s),
                                                                                                           (-s,  s, -s))),
                                                     Mass       => 1.0);
         the_Hull : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Hull",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3D,
                                           the_hull_Model.all'Access,
                                           the_hull_physics_Model);
      begin
         the_Applet.sim_World.add (the_Ball);
         the_Applet.sim_World.add (the_Box);
         the_Applet.sim_World.add (the_Cone);
         the_Applet.sim_World.add (the_Capsule);
         the_Applet.sim_World.add (the_multi_Sphere);
         the_Applet.sim_World.add (the_Hull);

         the_Ball        .Site_is (( x,        y,      0.0));
         the_Box         .Site_is (( 0.0,      y,     -2.5));
         the_Cone        .Site_is (( 0.0,      y,      0.0));
         the_Capsule     .Site_is (( 0.0 + X,  y,  0.0 + x));
         the_multi_Sphere.Site_is ((-4.0,      y,      4.4));
         the_Hull        .Site_is (( 4.0,      y,      4.4));

         x := x + 2.0;
         y := y + 2.0;
      end;

   end loop;


   -- Main loop.
   --
   while the_Applet.is_open
   loop
      the_Applet.freshen;     -- Handle any new events, evolve physics and update the screen.
   end loop;


   the_Applet.destroy;
end launch_full_Demo;
