with
     openGL.Geometry,
     openGL.Font;


package openGL.Model.Box.lit_textured
--
--  Models a lit and textured box.
--
--  Each face may have a separate texture.
--
is
   type Item is new Model.box.item with private;
   type View is access all Item'Class;


   type Face is
      record
         texture_Name : asset_Name := null_Asset;   -- The texture applied to the face.
      end record;

   type Faces is array (Side) of Face;


   ---------
   --- Forge
   --

   function new_Box (Size  : in Vector_3;
                     Faces : in lit_textured.Faces) return View;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;


private

   type Item is new Model.box.item with
      record
         Faces : lit_textured.Faces;
      end record;

end openGL.Model.Box.lit_textured;
