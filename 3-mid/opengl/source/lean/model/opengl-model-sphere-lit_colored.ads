with
     openGL.Model,
     openGL.Font,
     openGL.Geometry;


package openGL.Model.sphere.lit_colored
--
--  Models a lit, colored sphere.
--
is

   type Item is new openGL.Model.sphere.item with
      record
         Color : openGL.lucid_Color;
      end record;

   type View is access all Item'Class;

   procedure free (Self : in out View);



   package Forge
   is
      function new_Sphere (Radius : in math.Real;
                           Color  : in openGL.lucid_Color) return View;
   end Forge;


   overriding
   function  to_GL_Geometries (Self : access Item;    Textures : access Texture.name_Map_of_texture'Class;
                                                      Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

end openGL.Model.sphere.lit_colored;
