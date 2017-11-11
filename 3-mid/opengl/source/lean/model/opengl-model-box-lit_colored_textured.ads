with
     openGL.Model,
     openGL.Geometry,
     openGL.Font,
     openGL.Texture;


package openGL.Model.Box.lit_colored_textured
--
--  Models a lit, colored and textured box.
--
--  Each side may be separately colored via each of its 4 vertices.
--  Each side may have a separate texture.
--
is

   --------
   --- Face
   --

   type Face is
      record
         Colors         : openGL.lucid_Colors (1 .. 4);                          -- The color of each of the faces 4 vertices.

         texture_Name   : asset_Name            := null_Asset;
--           texture_Object : openGL.Texture.Object := openGL.Texture.null_Object;   -- The texture applied to the face.
      end record;

   type Faces is array (Side) of Face;



   -------
   --- Box
   --

   type Item is new openGL.Model.box.item with
      record
         Faces : lit_colored_textured.Faces;
      end record;

   type View is access all Item'Class;



   ---------
   --- Forge
   --

   package Forge
   is
      function new_Box (Size  : in math.Vector_3;
                        Faces : in lit_colored_textured.Faces) return View;
   end Forge;

   procedure free (Self : in out view);



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;


end openGL.Model.Box.lit_colored_textured;
