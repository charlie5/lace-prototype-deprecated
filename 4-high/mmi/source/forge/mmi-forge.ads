with
     mmi.Applet.gui_world,
     mmi.Applet.gui_and_sim_world,
     mmi.Sprite,
     mmi.World,

     physics.Forge,

     openGL.Primitive,
     openGL.Font,
     openGL.Palette;


package mmi.Forge
--
--  Provides utility constructor functions for various MMI classes.
--
is

   ------------
   --- Applets
   --

   function         new_gui_Applet (Named         : in String;
                                    window_Width  : in Positive                 := 500;
                                    window_Height : in Positive                 := 500;
                                    space_Kind    : in physics.Forge.space_Kind := physics.Forge.Bullet) return mmi.Applet.gui_world.view;

   function new_gui_and_sim_Applet (Named         : in String;
                                    window_Width  : in Positive                 := 500;
                                    window_Height : in Positive                 := 500;
                                    space_Kind    : in physics.Forge.space_Kind := physics.Forge.Bullet) return mmi.Applet.gui_and_sim_World.view;



   ------------
   --- Sprites
   --

   --- 2D
   --

   function new_circle_Sprite    (in_World : in mmi.World.view;
                                  Mass     : in math.Real        := 1.0;
                                  Radius   : in math.Real        := 0.5;
                                  Color    : in openGL.Color     := opengl.Palette.White) return mmi.Sprite.view;

   function new_polygon_Sprite   (in_World : in mmi.World.view;
                                  Mass     : in math.Real        := 1.0;
                                  Vertices : in Geometry_2d.Sites;
                                  Color    : in openGL.Color     := opengl.Palette.White) return mmi.Sprite.view;

   function new_rectangle_Sprite (in_World : in mmi.World.view;
                                  Mass     : in math.Real        := 1.0;
                                  Width,
                                  Height   : in math.Real;
                                  Color    : in openGL.Color     := opengl.Palette.White) return mmi.Sprite.view;

   --- 3D
   --

   function new_ball_Sprite          (in_World     : in mmi.World.view;
                                      Mass         : in math.Real          := 1.0;
                                      Radius       : in math.Real          := 0.5;
                                      Color        : in openGL.Color       := opengl.Palette.White) return mmi.Sprite.view;

   subtype box_Colors is openGL.Colors (1 .. 6);

   function new_box_Sprite           (in_World     : in mmi.World.view;
                                      Mass         : in math.Real          := 1.0;
                                      Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Colors       : in box_Colors         := (others => opengl.Palette.random_Color);
                                      is_Kinematic : in Boolean            := False)            return mmi.Sprite.view;

   function new_box_Sprite           (in_World     : in mmi.World.view;
                                      Mass         : in math.Real          := 1.0;
                                      Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Texture      : in openGL.asset_Name)                      return mmi.Sprite.view;

   function new_billboard_Sprite     (in_World     : in mmi.World.view;
                                      Mass         : in math.Real          := 1.0;
                                      Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Texture      : in openGL.asset_Name  := openGL.null_Asset) return mmi.Sprite.view;

   function new_billboard_Sprite     (in_World     : in mmi.World.view;
                                      Color        : in openGL.lucid_Color;
                                      Mass         : in math.Real          := 1.0;
                                      Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Texture      : in openGL.asset_Name  := openGL.null_Asset) return mmi.Sprite.view;


   function new_arrow_Sprite         (in_World     : in mmi.World.view;
                                      Mass         : in math.Real          := 0.0;
                                      Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Texture      : in openGL.asset_Name  := openGL.null_Asset;
                                      Color        : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                                      line_Width   : in openGL.Real        := openGL.Primitive.unused_line_Width) return mmi.Sprite.view;

   function new_line_Sprite          (in_World     : in mmi.World.view;
                                      Mass         : in math.Real          := 0.0;
                                      Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Texture      : in openGL.asset_Name  := openGL.null_Asset;
                                      Color        : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                                      line_Width   : in openGL.Real        := openGL.Primitive.unused_line_Width) return mmi.Sprite.view;

   function new_segment_line_Sprite  (in_World     : in mmi.World.view;
                                      Mass         : in math.Real          := 0.0;
                                      Size         : in math.Vector_3      := (1.0, 1.0, 1.0);
                                      Texture      : in openGL.asset_Name  := openGL.null_Asset;
                                      Color        : in openGL.lucid_Color := (openGL.Palette.Black, openGL.Opaque);
                                      line_Width   : in openGL.Real        := openGL.Primitive.unused_line_Width) return mmi.Sprite.view;


   --- Text
   --

   function new_text_Sprite (in_World : in mmi.World.view;
                             Text     : in String;
                             Font     : in openGL.Font.font_Id;
                             Color    : in openGL.Color       := opengl.Palette.Black;
                             Scale    : in math.Vector_3      := (1.0, 1.0, 1.0);
                             Centered : in Boolean            := True)                 return mmi.Sprite.view;

end mmi.Forge;
