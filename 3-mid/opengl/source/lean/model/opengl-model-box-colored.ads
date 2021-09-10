with
     openGL.Font,
     openGL.Geometry;

package openGL.Model.box.colored
--
--  Models a colored box.
--
--  Each face may be separately colored via each of its 4 vertices.
--
is

   --------
   --- Face

   type Face is
      record
         Colors : lucid_Colors (1 .. 4);     -- The color of each of the faces 4 vertices.
      end record;

   type Faces is array (Side) of Face;


   -------
   --- Box
   --

   type Item is new Model.box.item with     -- TODO: Make private.
      record
         Faces : colored.Faces;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Box (Size  : in Vector_3;
                     Faces : in colored.Faces) return View;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;


end openGL.Model.box.colored;
