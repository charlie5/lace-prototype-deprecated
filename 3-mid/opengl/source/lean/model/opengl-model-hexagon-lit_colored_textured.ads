with
     openGL.Model,
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.Hexagon.lit_colored_textured
--
--  Models a lit, colored and textured hexagon.
--
is

   --------
   --- Face
   --

   type Face is
      record
         center_Color : openGL.lucid_Color;                                          --  The color of the center of the hex.
         Colors       : openGL.lucid_Colors (1 .. 6);                                --  The color of each of the faces 6 vertices.
         Texture      : openGL.Texture.Object       := openGL.Texture.null_Object;   --  The texture to be applied to the face.
      end record;


   -----------------
   --- Hexagon Model
   --

   type Item is new openGL.Model.Hexagon.item with
      record
         Face : lit_colored_textured.Face;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   package Forge
   is
      function new_Hexagon (Radius : in Real;
                            Face   : in lit_colored_textured.Face) return View;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

end openGL.Model.Hexagon.lit_colored_textured;
