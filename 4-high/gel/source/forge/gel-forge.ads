with
     gel.Applet.gui_world,
     gel.Applet.gui_and_sim_world,
     gel.Applet.server_world,
     gel.Applet.client_world,
     gel.Sprite,
     gel.World,

     Physics,

     openGL.Primitive,
     openGL.Model.sphere,
     openGL.Font,
     openGL.Palette;


package gel.Forge
--
--  Provides utility constructor functions for various GEL classes.
--
is

   -----------
   --- Applets
   --

   function         new_gui_Applet (Named         : in String;
                                    window_Width  : in Positive           := 500;
                                    window_Height : in Positive           := 500;
                                    space_Kind    : in physics.space_Kind := physics.Bullet) return gel.Applet.gui_world.view;

   function new_gui_and_sim_Applet (Named         : in String;
                                    window_Width  : in Positive           := 500;
                                    window_Height : in Positive           := 500;
                                    space_Kind    : in physics.space_Kind := physics.Bullet) return gel.Applet.gui_and_sim_World.view;

   function      new_server_Applet (Named         : in String;
                                    window_Width  : in Positive           := 500;
                                    window_Height : in Positive           := 500;
                                    space_Kind    : in physics.space_Kind := physics.Bullet) return gel.Applet.server_world.view;

   function      new_client_Applet (Named         : in String;
                                    window_Width  : in Positive           := 500;
                                    window_Height : in Positive           := 500;
                                    space_Kind    : in physics.space_Kind := physics.Bullet) return gel.Applet.client_world.view;
   -----------
   --- Sprites
   --

   -- 2D
   --

   function new_circle_Sprite    (in_World : in gel.World.view;
                                  Site     : in math.Vector_2     := math.Origin_2D;
                                  Mass     : in math.Real         := 1.0;
                                  Friction : in math.Real         := 0.5;
                                  Bounce   : in math.Real         := 0.5;
                                  Radius   : in math.Real         := 0.5;
                                  Color    : in openGL.Color      := opengl.Palette.White;
                                  Texture  : in openGL.asset_Name := openGL.null_Asset) return gel.Sprite.view;

   function new_polygon_Sprite   (in_World : in gel.World.view;
                                  Site     : in math.Vector_2    := math.Origin_2D;
                                  Mass     : in math.Real        := 1.0;
                                  Friction : in math.Real        := 0.5;
                                  Bounce   : in math.Real        := 0.5;
                                  Vertices : in Geometry_2d.Sites;
                                  Color    : in openGL.Color     := opengl.Palette.White) return gel.Sprite.view;

   function new_rectangle_Sprite (in_World : in gel.World.view;
                                  Site     : in math.Vector_2 := math.Origin_2D;
                                  Mass     : in math.Real     := 1.0;
                                  Friction : in math.Real     := 0.5;
                                  Bounce   : in math.Real     := 0.5;
                                  Width,
                                  Height   : in math.Real;
                                  Color    : in openGL.Color  := opengl.Palette.White) return gel.Sprite.view;
   -- 3D
   --

   function new_ball_Sprite      (in_World   : in gel.World.view;
                                  Site       : in math.Vector_3      := math.Origin_3D;
                                  Mass       : in math.Real          := 1.0;
                                  Radius     : in math.Real          := 0.5;
                                  lat_Count  : in Positive           := openGL.Model.sphere.default_latitude_Count;
                                  long_Count : in Positive           := openGL.Model.sphere.default_longitude_Count;
                                  is_Lit     : in Boolean            := True;
                                  Color      : in openGL.lucid_Color := opengl.no_lucid_Color;
                                  Texture    : in openGL.asset_Name  := openGL.null_Asset) return gel.Sprite.view;

   function new_skysphere_Sprite (in_World : in gel.World.view;
                                  Site     : in math.Vector_3 := math.Origin_3D;
                                  Radius   : in math.Real     := 1_000_000.0;
                                  Texture  : in openGL.asset_Name) return gel.Sprite.view;


   subtype box_Colors is openGL.Colors (1 .. 6);

   function new_box_Sprite       (in_World     : in gel.World.view;
                                  Site         : in math.Vector_3 := math.Origin_3D;
                                  Mass         : in math.Real     := 1.0;
                                  Size         : in math.Vector_3 := (1.0, 1.0, 1.0);
                                  Colors       : in box_Colors    := (others => opengl.Palette.random_Color);
                                  is_Kinematic : in Boolean       := False) return gel.Sprite.view;

   function new_box_Sprite       (in_World     : in gel.World.view;
                                  Site         : in math.Vector_3 := math.Origin_3D;
                                  Mass         : in math.Real     := 1.0;
                                  Size         : in math.Vector_3 := (1.0, 1.0, 1.0);
                                  Texture      : in openGL.asset_Name) return gel.Sprite.view;

   function new_billboard_Sprite (in_World     : in gel.World.view;
                                  Site         : in math.Vector_3     := math.Origin_3D;
                                  Mass         : in math.Real         := 1.0;
                                  Size         : in math.Vector_3     := (1.0, 1.0, 1.0);
                                  Texture      : in openGL.asset_Name := openGL.null_Asset) return gel.Sprite.view;

   function new_billboard_Sprite (in_World     : in gel.World.view;
                                  Site         : in math.Vector_3     := math.Origin_3D;
                                  Color        : in openGL.lucid_Color;
                                  Mass         : in math.Real         := 1.0;
                                  Size         : in math.Vector_3     := (1.0, 1.0, 1.0);
                                  Texture      : in openGL.asset_Name := openGL.null_Asset) return gel.Sprite.view;

   function new_arrow_Sprite     (in_World     : in gel.World.view;
                                  Site         : in math.Vector_3      := math.Origin_3D;
                                  Mass         : in math.Real          := 0.0;
                                  Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                  Texture      : in openGL.asset_Name  := openGL.null_Asset;
                                  Color        : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                                  line_Width   : in openGL.Real        := openGL.Primitive.unused_line_Width) return gel.Sprite.view;

   function new_line_Sprite      (in_World     : in gel.World.view;
                                  Site         : in math.Vector_3      := math.Origin_3D;
                                  Mass         : in math.Real          := 0.0;
                                  Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                  Texture      : in openGL.asset_Name  := openGL.null_Asset;
                                  Color        : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                                  line_Width   : in openGL.Real        := openGL.Primitive.unused_line_Width) return gel.Sprite.view;

   function new_segment_line_Sprite (in_World     : in gel.World.view;
                                     Site         : in math.Vector_3      := math.Origin_3D;
                                     Mass         : in math.Real          := 0.0;
                                     Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                     Texture      : in openGL.asset_Name  := openGL.null_Asset;
                                     Color        : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                                     line_Width   : in openGL.Real        := openGL.Primitive.unused_line_Width) return gel.Sprite.view;
   -- Text
   --

   function new_text_Sprite (in_World : in gel.World.view;
                             Site     : in math.Vector_3 := math.Origin_3D;
                             Text     : in String;
                             Font     : in openGL.Font.font_Id;
                             Color    : in openGL.Color  := opengl.Palette.Black;
                             Size     : in math.Vector_3 := (1.0, 1.0, 1.0);
                             Centered : in Boolean       := True) return gel.Sprite.view;

end gel.Forge;
