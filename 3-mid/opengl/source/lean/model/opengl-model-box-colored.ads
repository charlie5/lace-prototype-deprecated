with
     openGL.Model,
     openGL.Font,
     openGL.Geometry;


package openGL.Model.box.colored
--
--  Models a colored box.
--
--  Each side may be separately colored via each of its 4 vertices.
--
is

   --------
   --- Face

   type Face is
      record
         Colors : openGL.lucid_Colors (1 .. 4);  -- The color of each of the faces 4 vertices.
      end record;

   type Faces is array (Side) of Face;



   -------
   --- Box
   --

   type Item is new openGL.Model.box.item with
      record
         Faces : colored.Faces;
      end record;

   type View is access all Item'Class;



   ---------
   --- Forge
   --

   package Forge
   is
      function new_Box (Scale : in math.Vector_3;
                        Faces : in colored.Faces) return View;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries  (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                      Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;


end openGL.Model.box.colored;
