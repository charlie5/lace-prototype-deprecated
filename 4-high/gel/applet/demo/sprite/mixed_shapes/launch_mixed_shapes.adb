with
     gel.Window.lumen,
     gel.Applet.gui_world,
     gel.Forge,
     gel.Sprite,
     gel.Terrain,
     physics.Model,

     openGL.Model.box.colored,
     openGL.Model.sphere.lit_colored_textured,
     openGL.Model.capsule.lit_colored_textured,
     openGL.Model.any,
     openGL.Model.terrain,
     openGL.IO,
     openGL.Light.directional,
     openGL.Palette,

     Physics,

     float_Math.Algebra.linear;

pragma Unreferenced (gel.Window.lumen);


procedure launch_mixed_Shapes
--
--  Drops a variety of shapes onto a terrain.
--
is
   use gel.Applet,
       gel.Math,
       openGL,
       openGL.Model.box,
       opengl.Palette;

   use type openGL.Real;

   the_Applet : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("mixed Shapes", 1920, 1200);

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
      Last_i : Index_t := From'Last (1);
      Last_j : Index_t := From'Last (2);
   begin
      for i in Result'Range (1)
      loop
         for j in Result'Range (1)
         loop
            --  Result (i, j) := math.Real (From (Index_t (i),
            --                                    Index_t (j)));
            --  Result (j, i) := math.Real (From (Last_i - Index_t (i) + 1,
            --                                    Last_j - Index_t (j) + 1));
            Result (i, j) := math.Real (From (Last_i - Index_t (i) + 1,
                                              Index_t (j)));
         end loop;
      end loop;

      return Result;
   end to_Heightfield;


   --  terrain_Heights : openGL.asset_Name := to_Asset ("assets/gel/kidwelly_128x128.tga");
   --  terrain_Colors  : openGL.asset_Name := to_Asset ("assets/gel/kidwelly_128x128.tga");

   terrain_Heights : openGL.asset_Name := to_Asset ("assets/gel/kidwelly-terrain.png");
   terrain_Colors  : openGL.asset_Name := to_Asset ("assets/gel/kidwelly-terrain-texture.png");

   --  hs : constant := 4.0 * 2.0;
   hs : constant := 1.0;

   gl_Heights : openGL.IO.height_Map_view := opengl.IO.to_height_Map (image_Filename => terrain_Heights,
                                                                      scale          => 2.0);

   the_Heightfield_Model : constant openGL.Model.terrain.view
     := openGL.Model.terrain.new_Item (heights_Asset => terrain_Heights,
                                       row => 1,
                                       col => 1,
                                       heights => openGL.Model.terrain.height_Map_view (gl_Heights),
                                       color_Map => terrain_Colors,
                                       --  scale   => (hs, hs*2.0, hs),
                                       tiling  => (s => (0.0, 1.0),
                                                   t => (0.0, 1.0)));

   the_Heightfield_physics_Model : constant physics.Model.view
     := physics.Model.Forge.new_physics_Model (shape_info => (kind    => physics.Model.heightfield,
                                                              heights => new physics.Heightfield' (to_Heightfield (gl_Heights.all)),
                                                              height_Range => (0.0, 200.0)),
                                               scale      =>  (hs, 1.0, hs));
   the_Heightfield : constant gel.Sprite.view
     := gel.Sprite.forge.new_Sprite ("demo.Hull",
                                     the_Applet.gui_World.all'Access,
                                     Origin_3D,
                                     the_Heightfield_Model.all'Access,
                                     the_Heightfield_physics_Model);

   --  the_terrain_Grid : constant access gel.Sprite.Grid
   --    := gel.Terrain.new_Terrain (World        => the_Applet.gui_World,
   --                                --  heights_File => "assets/gel/kidwelly_255x255.tga",
   --                                --  texture_File => "assets/gel/kidwelly_255x255.tga",
   --                                heights_File => "assets/gel/golf_green-16x16.tga",
   --                                texture_File => "assets/gel/golf_green-16x16.tga",
   --                                --  heights_File => "assets/gel/kidwelly_255x255.tga",
   --                                --  texture_File => "assets/gel/kidwelly_255x255.tga",
   --                                --  heights_File => "assets/gel/kidwelly_255x255.tga",
   --                                --  texture_File => "assets/gel/kidwelly_255x255.tga",
   --                                scale        => (1.0, 1.0, 1.0));
   --                                --  scale        => (1.0, 64.0, 1.0));



begin
   the_Applet.gui_Camera.Site_is ((0.0, 4.0, 30.0));      -- Position the camera.

   the_Applet.enable_simple_Dolly (in_world => 1);                    -- Enable user camera control via keyboard.
   the_Applet.Dolly.Speed_is (0.1);

   the_Applet.Renderer.Background_is (Blue);

   -- Set the lights initial position.
   --
   declare
      Light : openGL.Light.directional.item := the_Applet.Renderer.Light (Id => 1);
   begin
      Light.Site_is ((0.0, 1000.0, 0.0));
      the_Applet.Renderer.Light_is (Id => 1, Now => Light);
   end;

--     -- Terrain.
--     --
--     the_Ground := gel.terrain.new_Terrain (the_Applet.gui_World,
--                                            gel.Sprite.local.physics_Space_view (the_Applet.gui_World.Physics),
--                                            "./assets/terrain/kidwelly/kidwelly-terrain.png",
--                                            "./assets/terrain/kidwelly/kidwelly-terrain-texture.png");
--     for Row in the_Ground'range (1) loop
--        for Col in the_Ground'range (2) loop
--           the_Applet.gui_World.add (the_Ground (Row, Col)'unchecked_access);
--        end loop;
--     end loop;

         --  the_Applet.gui_World.add (the_Plane);                  -- Add plane
         the_Applet.gui_World.add (the_heightfield);            -- Add heightfield.
   --  the_Applet.gui_World.add (the_terrain_Grid (1, 1));        -- Add heightfield.

         --  the_Plane       .Site_is (( 0.0,  0.0,  0.0));
         --  the_Heightfield .Site_is (( 0.0,  0.0,  0.0));
--           the_terrain_Grid (1, 1).Site_is
--           the_terrain_Grid (1, 1).Site_is ((0.0, 0.0, 0.0));
--                                    ((64.0/2.0,      0.0,      64.0/2.0));


   --  declare
   --     Heights : constant asset_Name := to_Asset ("assets/gel/kidwelly-terrain-510x510.png");
   --     Texture : constant asset_Name := to_Asset ("assets/gel/kidwelly-terrain-texture-255x255.png");
   --
   --     Terrain : constant openGL.Visual.Grid := openGL.Terrain.new_Terrain (heights_File => Heights,
   --                                                                          texture_File => Texture,
   --                                                                          Scale        => (1.0, 25.0, 1.0));
   --     --  Count   : constant Positive :=   Terrain'Length (1)
   --     --                                 * Terrain'Length (2);
   --     --  Last    : Natural := 0;
   --     --  Sprites : openGL.Visual.views (1 .. Count);
   --
   --  begin
   --     for Row in Terrain'Range (1)
   --     loop
   --        for Col in Terrain'Range (2)
   --        loop
   --           Last           := Last + 1;
   --           Sprites (Last) := Terrain (Row, Col);
   --        end loop;
   --     end loop;
   --  end;


   --  Add several of each shape.
   --
   for i in 1 .. 5
   loop
      declare
         use float_Math,
             float_math.Algebra.linear;

         --  Box
         --
         the_box_Model : constant openGL.Model.box.colored.view
           --  := openGL.Model.box.colored.new_Box (size => (1.0, 2.0, 4.0),
           := openGL.Model.box.colored.new_Box (size => (1.0, 1.0, 1.0),
                                                faces => (front => (colors => (others => (Red,     Opaque))),
                                                          rear  => (colors => (others => (Blue,    Opaque))),
                                                          upper => (colors => (others => (Violet,  Opaque))),
                                                          lower => (colors => (others => (Yellow,  Opaque))),
                                                          left  => (colors => (others => (Cyan,    Opaque))),
                                                          right => (colors => (others => (Magenta, Opaque)))));
         the_box_physics_Model : constant physics.Model.view
           := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                                    half_extents => the_box_Model.Size / 2.0),
                                                     mass       => 1.0);

         the_Box : constant gel.Sprite.view
           := gel.Sprite.Forge.new_Sprite ("demo.Box",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3d,
                                           the_box_Model.all'Access,
                                           the_box_physics_Model);

         --  Ball
         --
         the_ball_physics_Model : constant physics.Model.view
           := physics.Model.Forge.new_physics_Model (shape_Info => (kind          => physics.Model.a_Sphere,
                                                                    sphere_radius => 1.0),
                                                     mass       => 1.0);

         the_ball_Model : constant openGL.Model.sphere.lit_colored_textured.view
           := openGL.Model.sphere.lit_colored_textured.new_Sphere (radius => 1.0,
                                                                   image  => openGL.to_Asset ("assets/gel/golf_green-16x16.tga"));
         the_Ball : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Ball",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3d,
                                           the_ball_Model,
                                           the_ball_physics_Model);

         --  Cone
         --
         the_cone_Model : constant openGL.Model.any.view
           := openGL.Model.any.new_Model (Scale            => (1.0, 1.0, 1.0) * 1.0,
                                          Model            => openGL.to_Asset ("assets/gel/model/unit_cone.obj"),
                                          Texture          => openGL.null_Asset,
                                          Texture_is_lucid => False);
         the_cone_physics_Model : constant physics.Model.view
           := physics.Model.Forge.new_physics_Model (shape_Info => (kind => physics.Model.Cone),
                                                     mass       => 1.0);

         the_Cone : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Cone",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3d,
                                           the_cone_Model.all'Access,
                                           the_cone_physics_Model);

         --  Cylinder
         --
         the_cylinder_Model : constant openGL.Model.any.view
           := openGL.Model.any.new_Model (Scale            => (1.0, 1.0, 1.0),
                                          Model            => openGL.to_Asset ("assets/gel/model/unit_cylinder.obj"),
                                          Texture          => openGL.null_Asset,
                                          Texture_is_lucid => False);

         the_cylinder_physics_Model : constant physics.Model.view
           := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.cylinder,
                                                                    half_extents => the_cylinder_Model.Scale / 2.0),
                                                     mass       => 1.0);

         the_Cylinder : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Cylinder",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3d,
                                           the_cylinder_Model.all'Access,
                                           the_cylinder_physics_Model);

         --  Capsule
         --
         the_capsule_Model : constant openGL.Model.capsule.lit_colored_textured.view
           --  := openGL.Model.capsule.lit_colored_textured.new_Capsule (radius => 0.5,
           --                                                            height => 1.0,
           --                                                            color  => (palette.Green, Opaque));
           := openGL.Model.capsule.lit_colored_textured.new_Capsule (radius => 0.5, -- 0.5,
                                                                     height => 0.0,
                                                                     color  => (palette.Green, Opaque));

         the_capsule_physics_Model : constant physics.Model.view
           := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.a_Capsule,
                                                                    lower_radius => 0.5,
                                                                    upper_radius => 0.5,
                                                                    height       => 1.0),
           --  := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.a_Capsule,
           --                                                           lower_radius => 0.5 / 2.0,
           --                                                           upper_radius => 0.5 / 2.0,
           --                                                           height       => 1.0),
                                                     mass       => 1.0);
         the_Capsule : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Capsule",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3d,
                                           the_capsule_Model.all'Access,
                                           the_capsule_physics_Model);

         --  multi_Sphere
         --
         the_multi_Sphere_Model : constant openGL.Model.capsule.lit_colored_textured.view
           := openGL.Model.capsule.lit_colored_textured.new_Capsule (radius => 0.5,
                                                                     height => 0.0,
                                                                     color  => (palette.Green, Opaque),
                                                                     image  => openGL.to_Asset ("assets/gel/golf_green-16x16.tga"));

         the_multi_Sphere_physics_Model : constant physics.Model.view
           := physics.Model.Forge.new_physics_Model (shape_Info => (kind  => physics.Model.multi_Sphere,
                                                                    --  sites => new physics.Vector_3_array' ((0.0, 0.0, -0.5),
                                                                    --                                        (0.0, 0.0,  0.5)),
                                                                    --  radii => new float_math.Vector' (1 => 0.5,
                                                                    --                                   2 => 0.5)),
                                                                    sites => new physics.Vector_3_array' ((-0.5, -0.0, 0.0),
                                                                                                          ( 0.5,  0.0, 0.0)),
                                                                    radii => new float_math.Vector' (1 => 0.5,
                                                                                                     2 => 0.5)),
                                                     mass       => 1.0);

         the_multi_Sphere : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.multi_Sphere",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3d,
                                           the_multi_Sphere_Model.all'Access,
                                           the_multi_Sphere_physics_Model);

         --  Hull
         --
         s              : constant := 0.5;
         the_hull_Model : constant openGL.Model.box.colored.view
           := openGL.Model.box.colored.new_Box (size  => (s*2.0, s*2.0, s*2.0),
           --  := openGL.Model.box.colored.new_Box (size  => (s, s, s), -- (s*2.0, s*2.0, s*2.0),
                                                faces => (front => (colors => (others => (Shade_of (Grey, 1.0), Opaque))),
                                                          rear  => (colors => (others => (Shade_of (Grey, 0.5), Opaque))),
                                                          upper => (colors => (others => (Shade_of (Grey, 0.4), Opaque))),
                                                          lower => (colors => (others => (Shade_of (Grey, 0.3), Opaque))),
                                                          left  => (colors => (others => (Shade_of (Grey, 0.2), Opaque))),
                                                          right => (colors => (others => (Shade_of (Grey, 0.1), Opaque)))));
         the_Hull_physics_Model : constant physics.Model.view
           := physics.Model.Forge.new_physics_Model (shape_Info => (kind   => physics.Model.Hull,
                                                                    points => new physics.Vector_3_array'
                                                                                     ((-s, -s,  s),
                                                                                      ( s, -s,  s),
                                                                                      ( s,  s,  s),
                                                                                      (-s,  s,  s),

                                                                                      (-s, -s, -s),
                                                                                      ( s, -s, -s),
                                                                                      ( s,  s, -s),
                                                                                      (-s,  s, -s))),
                                                     mass       => 1.0);
         the_Hull : constant gel.Sprite.view
           := gel.Sprite.forge.new_Sprite ("demo.Hull",
                                           the_Applet.gui_World.all'Access,
                                           Origin_3d,
                                           the_Hull_Model.all'Access,
                                           the_Hull_physics_Model);

      begin
         the_Ball        .Site_is (( x,        y,  0.0));
         the_Applet.gui_World.add (the_Ball);                   -- Add ball.
         the_Applet.gui_World.add (the_Box);                    -- Add box.
         the_Applet.gui_World.add (the_Cone);                   -- Add cone.
         the_Applet.gui_World.add (the_Cylinder);               -- Add cylinder.
         the_Applet.gui_World.add (the_Capsule);                -- Add capsule.
         the_Applet.gui_World.add (the_multi_Sphere);           -- Add multi Sphere.
         the_Applet.gui_World.add (the_Hull);                   -- Add hull.

         the_Ball        .Site_is (( x,        y,  0.0));
         the_Box         .Site_is (( 0.0,      y,     -2.5));
         the_Cone        .Site_is (( 0.0,      y,      0.0));
         the_Capsule     .Site_is (( 0.0 + X,  y,  0.0 + x));
         the_Cylinder    .Site_is (( 0.0,      y,      4.4));
         the_Hull        .Site_is (( 4.0,      y,      4.4));
         the_multi_Sphere.Site_is ((-4.0,      y,      4.4));

         --     the_Cone    .Spin_is (y_Rotation_from (to_Radians (90.0)));
         --     the_Cylinder.Spin_is (x_Rotation_from (to_Radians (45.0)));
         --     the_Cylinder.Gyre_is ((0.0, 1.0, 0.0));
         --  the_Ball.Gyre_is ((0.0, 1.0, 0.0));

         x := x + 2.0;
         y := y + 2.0;
      end;

   end loop;


   while the_Applet.is_open
   loop
      the_Applet.freshen;                                 -- Handle any new events and update the screen.
   end loop;

   the_Applet.destroy;
end launch_mixed_shapes;
