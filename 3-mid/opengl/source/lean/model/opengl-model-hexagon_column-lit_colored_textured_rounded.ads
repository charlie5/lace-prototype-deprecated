with
     openGL.Model,
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.hexagon_Column.lit_colored_textured_rounded
--
--  Models a lit, colored and textured Column with rounded hexagon sides.
--
is

   ---------
   --- Faces
   --

   type hex_Face is
      record
         center_Color : openGL.lucid_Color;                                          --  The color of the center of the hex.
         Colors       : openGL.lucid_Colors (1 .. 6);                                --  The color of each of the faces 4 vertices.
         Texture      : openGL.Texture.Object       := openGL.Texture.null_Object;   --  The texture to be applied to the face.
      end record;


   type shaft_Face is
      record
         Color        : openGL.lucid_Color;                                          -- The color of the shaft.
         Texture      : openGL.Texture.Object := openGL.Texture.null_Object;         --  The texture to be applied to the shaft.
      end record;



   ---------------
   --- Column Item
   --

   type Item is new openGL.Model.Hexagon_Column.item with
      record
         upper_Face,
         lower_Face : lit_colored_textured_rounded.hex_Face;
         shaft_Face : lit_colored_textured_rounded.shaft_Face;
      end record;

   type View is access all Item'Class;



   ---------
   --- Forge
   --
   package Forge
   is
      function new_hexagon_Column (Scale : in math.Vector_3;
                                   Upper,
                                   Lower : in lit_colored_textured_rounded.hex_Face;
                                   Shaft : in shaft_Face) return View;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries     (Self : access Item;         Textures : access Texture.name_Map_of_texture'Class;
                                                               Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;


end openGL.Model.hexagon_Column.lit_colored_textured_rounded;
