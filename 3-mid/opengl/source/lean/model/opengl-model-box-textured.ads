with
     openGL.Model,
     openGL.Geometry,
     openGL.Font,
     openGL.Texture;


package openGL.Model.Box.textured
--
--  Models a textured box.
--
--  Each side may have a separate texture.
--
is

   --------
   --- Face
   --

   type Face is
      record
         texture_Name   : asset_Name            := null_Asset;
         texture_Object : openGL.Texture.Object := openGL.Texture.null_Object;   -- The texture applied to the face.
      end record;

   type Faces is array (Side) of Face;


   -------
   --- Box
   --

   type Item is new openGL.Model.box.item with
      record
         Faces : textured.Faces;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   package Forge
   is
      function new_Box (Scale : in math.Vector_3;
                        Faces : in textured.Faces) return View;
   end Forge;


   procedure free (Self : in out view);


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

end openGL.Model.Box.textured;
