with
     openGL.Geometry;

package openGL.Model.sphere.colored
--
--  Models a colored sphere.
--
is

   type Item is new openGL.Model.sphere.item with     -- TODO: Make private.
      record
         Color : lucid_Color;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Sphere (Radius : in Real;
                        Color  : in lucid_Color) return View;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;

end openGL.Model.sphere.colored;
