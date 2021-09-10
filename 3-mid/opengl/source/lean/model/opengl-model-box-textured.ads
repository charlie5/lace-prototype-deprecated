with
     openGL.Geometry,
     openGL.Font,
     openGL.Texture;

package openGL.Model.Box.textured
--
--  Models a textured box.
--
--  Each face may have a separate texture.
--
is
   --------
   --- Face
   --

   type Face is
      record
         texture_Name : asset_Name := null_Asset;
      end record;

   type Faces is array (Side) of Face;


   -------
   --- Box
   --

   type Item is new openGL.Model.box.item with     -- TODO: Make private.
      record
         Faces     : textured.Faces;
         is_Skybox : Boolean := False;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Box (Size      : in Vector_3;
                     Faces     : in textured.Faces;
                     is_Skybox : in Boolean := False) return View;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;

end openGL.Model.Box.textured;
