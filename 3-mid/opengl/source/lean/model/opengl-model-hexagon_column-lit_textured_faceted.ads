with
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.hexagon_Column.lit_textured_faceted
--
--  Models a lit, colored and textured column with 6 faceted shaft sides.
--
is
   type Item is new Model.hexagon_Column.Item with private;
   type View is access all Item'Class;


   ---------
   --- Faces
   --

   type hex_Face is
      record
         Texture      : asset_Name := openGL.null_Asset;     -- The texture to be applied to the face.
      end record;

   type shaft_Face is
      record
         Texture : asset_Name := null_Asset;                 -- The texture to be applied to the shaft.
      end record;


   ---------
   --- Forge
   --

   function new_hexagon_Column (Radius : in Real;
                                Height : in Real;
                                Upper,
                                Lower  : in   hex_Face;
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

end openGL.Model.hexagon_Column.lit_textured_faceted;
