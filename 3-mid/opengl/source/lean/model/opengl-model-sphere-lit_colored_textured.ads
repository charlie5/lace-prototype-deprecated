with
     openGL.Model,
     openGL.Font,
     openGL.Geometry;


package openGL.Model.sphere.lit_colored_textured
--
--  Models a lit, colored, textured sphere.
--
is

   type Item is new openGL.Model.sphere.item with
      record
         Image : asset_Name := null_Asset;     -- Usually a mercator projection to be mapped onto the sphere.
      end record;

   type View is access all Item'Class;



   function new_Sphere (Radius : in math.Real;
                        Image  : in asset_Name := null_Asset) return View;


   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

end openGL.Model.sphere.lit_colored_textured;
