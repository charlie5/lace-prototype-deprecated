with
     gel.Window.sdl,
     gel.Applet.gui_world,
     gel.Forge,
     gel.Sprite,
     --  gel.Terrain,

     physics.Model,

     openGL.Model.box.colored,
     openGL.Model.sphere.lit_colored_textured,
     openGL.Model.capsule.lit_colored_textured,
     openGL.Model.any,
     openGL.Model.terrain,
     openGL.IO,
     openGL.Light,
     openGL.Palette;

pragma unreferenced (gel.Window.sdl);


procedure launch_mixed_Shapes
--
--  Drops a variety of shapes onto a terrain.
--
is
   use gel.Math,
       openGL,
       openGL.Model.box,
       opengl.Palette;

   the_Applet : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("mixed Shapes", 1536, 864);

   x : math.Real := 0.0;
   y : math.Real := 2.0;

   -----------
   --  Terrain
   --

   --  Plane
   --
   --  the_plane_Model : constant openGL.Model.box.colored.view
   --    := openGL.Model.box.colored.new_Box (size => (1000.0, 0.05, 1000.0),
   --                                         faces => (front => (colors => (others => (Red,     Opaque))),
   --                                                   rear  => (colors => (others => (Blue,    Opaque))),
   --                                                   upper => (colors => (others => (Green,   Opaque))),
   --                                                   lower => (colors => (others => (Yellow,  Opaque))),
   --                                                   left  => (colors => (others => (Cyan,    Opaque))),
   --                                                   right => (colors => (others => (Magenta, Opaque)))));
   --  the_plane_physics_Model : constant physics.Model.view
   --    := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Plane,
   --                                                             plane_Normal => (0.00, 1.0, 0.00),
   --                                                             plane_Offset =>  0.0));
   --  the_Plane : constant gel.Sprite.view
   --    := gel.Sprite.forge.new_Sprite ("demo.Plane",
   --                                    the_Applet.gui_World.all'Access,
   --                                    math.Origin_3d,
   --                                    the_plane_Model.all'Access,
   --                                    the_plane_physics_Model);


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
                                                                               Scale          => 2.0);

   the_heightfield_Model : constant openGL.Model.terrain.view
     := openGL.Model.terrain.new_Terrain (heights_Asset => terrain_Heights,
                                          Row           => 1,
                                          Col           => 1,
                                          Heights       => openGL.Model.terrain.height_Map_view (gl_Heights),
                                          color_Map     => terrain_Texture,
                                          Tiling        => (s => (0.0, 1.0),
                                                            t => (0.0, 1.0)));

   the_heightfield_physics_Model : constant physics.Model.view
     := physics.Model.forge.new_physics_Model (shape_Info => (Kind         => physics.Model.heightfield,
                                                              Heights      => new physics.Heightfield' (to_Heightfield (gl_Heights.all)),
                                                              height_Range => (0.0, 200.0)),
                                               Scale      =>  (hs, 1.0, hs));
   the_Heightfield : constant gel.Sprite.view
     := gel.Sprite.forge.new_Sprite ("demo.Hull",
                                     the_Applet.gui_World.all'Access,
                                     Origin_3D,
                                     the_Heightfield_Model,
                                     the_Heightfield_physics_Model);
begin
   -- Applet.
   --
   the_Applet.gui_Camera.Site_is ((0.0, 4.0, 30.0));                         -- Position the camera.

   the_Applet.enable_simple_Dolly (in_World => the_Applet.gui_World.Id);     -- Enable user camera control via keyboard.
   the_Applet.Dolly.Speed_is (0.1);

   the_Applet.Renderer.Background_is (Blue);

   -- Set the lights position.
   --
   declare
      Light : openGL.Light.item := the_Applet.Renderer.new_Light;
   begin
      Light.Site_is ((0.0, 1000.0, 0.0));
      the_Applet.Renderer.set (Light);
   end;

   -- Terrain.
   --
   the_Applet.gui_World.add (the_heightfield);     -- Add heightfield.

   --  Add several of each shape.
   --
   for i in 1 .. 5
   loop
      declare
         --  Box
         --
         the_box_Model : constant openGL.Model.box.colored.view
           := openGL.Model.box.colored.new_Box (Size => (1.0, 1.0, 1.0),
                                                Faces => (Front => (Colors => (others => (Red,     Opaque))),
                                                          Rear  => (Colors => (others => (Blue,    Opaque))),
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
                                                                    sphere_Radius => 1.0),
                                                     Mass       => 1.0);

         the_ball_Model : constant openGL.Model.sphere.lit_colored_textured.view
           := openGL.Model.sphere.lit_colored_textured.new_Sphere (Radius => 1.0,
                                                                   Image  => openGL.to_Asset ("assets/gel/golf_green-16x16.tga"));
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
                                          Texture          => openGL.null_Asset,
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
         --  Cylinder
         --
         the_cylinder_Model : constant openGL.Model.any.view
           := openGL.Model.any.new_Model (Model            => openGL.to_Asset ("assets/gel/model/unit_cylinder.obj"),
                                          Texture          => openGL.null_Asset,
                                          Texture_is_lucid => False);

         the_cylinder_physics_Model : constant physics.Model.view
           := physics.Model.forge.new_physics_Model (shape_Info => (Kind         => physics.Model.cylinder,
                                                                    half_Extents => (1.0, 1.0, 1.0) / 2.0),
                                                     Mass       => 1.0);

         the_Cylinder : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Cylinder",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3D,
                                           the_cylinder_Model.all'Access,
                                           the_cylinder_physics_Model);
         --  Capsule
         --
         the_capsule_Model : constant openGL.Model.capsule.lit_colored_textured.view
           := openGL.Model.capsule.lit_colored_textured.new_Capsule (Radius => 0.5,
                                                                     Height => 0.0,
                                                                     Color  => (palette.Green, Opaque));

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
         the_multi_Sphere_Model : constant openGL.Model.capsule.lit_colored_textured.view
           := openGL.Model.capsule.lit_colored_textured.new_Capsule (Radius => 0.5,
                                                                     Height => 0.0,
                                                                     Color  => (palette.Green, Opaque),
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
           := openGL.Model.box.colored.new_Box (Size  => (s*2.0, s*2.0, s*2.0),
                                                Faces => (Front => (Colors => (others => (Shade_of (Grey, 1.0), Opaque))),
                                                          Rear  => (Colors => (others => (Shade_of (Grey, 0.5), Opaque))),
                                                          Upper => (Colors => (others => (Shade_of (Grey, 0.4), Opaque))),
                                                          Lower => (Colors => (others => (Shade_of (Grey, 0.3), Opaque))),
                                                          Left  => (Colors => (others => (Shade_of (Grey, 0.2), Opaque))),
                                                          Right => (Colors => (others => (Shade_of (Grey, 0.1), Opaque)))));
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
         the_Applet.gui_World.add (the_Ball);
         the_Applet.gui_World.add (the_Box);
         the_Applet.gui_World.add (the_Cone);
         the_Applet.gui_World.add (the_Cylinder);
         the_Applet.gui_World.add (the_Capsule);
         the_Applet.gui_World.add (the_multi_Sphere);
         the_Applet.gui_World.add (the_Hull);

         the_Ball        .Site_is (( x,        y,      0.0));
         the_Box         .Site_is (( 0.0,      y,     -2.5));
         the_Cone        .Site_is (( 0.0,      y,      0.0));
         the_Capsule     .Site_is (( 0.0 + X,  y,  0.0 + x));
         the_Cylinder    .Site_is (( 0.0,      y,      4.4));
         the_Hull        .Site_is (( 4.0,      y,      4.4));
         the_multi_Sphere.Site_is ((-4.0,      y,      4.4));

         x := x + 2.0;
         y := y + 2.0;
      end;

   end loop;


   while the_Applet.is_open
   loop
      the_Applet.freshen;     -- Handle any new events, evolve physics and update the screen.
   end loop;

   the_Applet.destroy;
end launch_mixed_Shapes;
