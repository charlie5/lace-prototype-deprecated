with
     openGL.Font,
     openGL.Geometry;


package openGL.Model.sphere.lit_colored_textured
--
--  Models a lit, colored, textured sphere.
--
is
   type Item is new Model.sphere.item with private;
   type View is access all Item'Class;


   function new_Sphere (Radius : in Real;
                        Image  : in asset_Name := null_Asset) return View;


   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;



private

   type Item is new Model.sphere.item with
      record
         Image : asset_Name := null_Asset;     -- Usually a mercator projection to be mapped onto the sphere.
      end record;

end openGL.Model.sphere.lit_colored_textured;
