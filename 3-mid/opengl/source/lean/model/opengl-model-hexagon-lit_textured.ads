with
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.hexagon.lit_textured
--
--  Models a lit, colored and textured hexagon.
--
is
   type Item is new Model.item with private;
   type View is access all Item'Class;

   type Face is
      record
         Texture : openGL.Texture.Object := openGL.Texture.null_Object;   -- The texture to be applied to the hex.
      end record;


   ---------
   --- Forge
   --

   function new_Hexagon (Radius : in Real;
                         Face   : in lit_textured.Face) return View;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;


private

   type Item is new Model.hexagon.item with
      record
         Face : lit_textured.Face;
      end record;

end openGL.Model.hexagon.lit_textured;
