with
     openGL.Model,
     openGL.Geometry;


package openGL.Model.sphere.colored
--
--  Models a colored sphere.
--
is

   type Item is new openGL.Model.sphere.item with
      record
         Color : openGL.lucid_Color;
      end record;

   type View is access all Item'Class;



   ---------
   --- Forge
   --

   function new_Sphere (Radius : in math.Real;
                        Color  : in openGL.lucid_Color) return View;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

end openGL.Model.sphere.colored;
