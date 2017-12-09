with
     openGL.Model,
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.hexagon_Column.lit_colored_textured_rounded
--
--  Models a lit, colored and textured column with six rounded sides.
--
--  The shaft of the column appears rounded, whereas the top and bottom appear as hexagons.
--
is


   type Item is new openGL.Model.Hexagon_Column.item with private;
   type View is access all Item'Class;


   ---------
   --- Faces
   --

   type hex_Face is
      record
         center_Color : openGL.lucid_Color;                         --  The color of the center of the hex.
         Colors       : openGL.lucid_Colors (1 .. 6);               --  The color of each of the faces 4 vertices.
         Texture      : openGL.asset_Name := openGL.null_Asset;     --  The texture to be applied to the face.
      end record;


   type shaft_Face is
      record
         Color   : openGL.lucid_Color;                              -- The color of the shaft.
         Texture : openGL.asset_Name := openGL.null_Asset;          -- The texture to be applied to the shaft.
      end record;


   ---------
   --- Forge
   --

   package Forge
   is
      function new_hexagon_Column (Radius : in Real;
                                   Height : in Real;
                                   Upper,
                                   Lower  : in lit_colored_textured_rounded.hex_Face;
                                   Shaft  : in shaft_Face) return View;
   end Forge;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;


private

   type Item is new openGL.Model.Hexagon_Column.item with
      record
         upper_Face,
         lower_Face : lit_colored_textured_rounded.hex_Face;
         shaft_Face : lit_colored_textured_rounded.shaft_Face;
      end record;


end openGL.Model.hexagon_Column.lit_colored_textured_rounded;
