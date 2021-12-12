with
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.hexagon.lit_colored_textured
--
--  Models a lit, colored and textured hexagon.
--
is
   type Item is new Model.item with private;
   type View is access all Item'Class;

   type Face is
      record
         center_Color : lucid_Color;                                           --  The color at the center of the hex.
         Colors       : lucid_Colors (1 .. 6);                                 --  The color at each of the hexes 6 vertices.
         Texture      : openGL.Texture.Object := openGL.Texture.null_Object;   --  The texture to be applied to the hex..
      end record;


   ---------
   --- Forge
   --

   function new_Hexagon (Radius : in Real;
                         Face   : in lit_colored_textured.Face) return View;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;


private

   type Item is new Model.hexagon.item with
      record
         Face : lit_colored_textured.Face;
      end record;

end openGL.Model.hexagon.lit_colored_textured;
