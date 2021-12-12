with
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.hexagon_Column.lit_colored_rounded
--
--  Models a lit and colored column with six rounded sides.
--
--  The shaft of the column appears rounded, whereas the top and bottom appear as hexagons.
--
is
   type Item is new Model.hexagon_Column.item with private;
   type View is access all Item'Class;


   ---------
   --- Faces
   --

   type hex_Face is
      record
         center_Color : lucid_Color;                  -- The color of the center of the hex.
         Colors       : lucid_Colors (1 .. 6);        -- The color of each of the faces 4 vertices.
      end record;


   type shaft_Face is
      record
         Color   : lucid_Color;                       -- The color of the shaft.
      end record;


   ---------
   --- Forge
   --

   function new_hexagon_Column (Radius : in Real;
                                Height : in Real;
                                Upper,
                                Lower  : in hex_Face;
                                Shaft  : in shaft_Face) return View;

   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;


private

   type Item is new Model.hexagon_Column.item with
      record
         upper_Face,
         lower_Face : hex_Face;
         Shaft      : shaft_Face;
      end record;

end openGL.Model.hexagon_Column.lit_colored_rounded;
