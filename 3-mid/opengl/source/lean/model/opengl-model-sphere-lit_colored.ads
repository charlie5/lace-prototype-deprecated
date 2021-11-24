with
     openGL.Font,
     openGL.Geometry;


package openGL.Model.sphere.lit_colored
--
--  Models a lit, colored sphere.
--
is
   type Item is new Model.sphere.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Sphere (Radius     : in Real;
                        lat_Count  : in Positive := default_latitude_Count;
                        long_Count : in Positive := default_longitude_Count;
                        Color      : in lucid_Color) return View;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;    Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;



private

   type Item is new Model.sphere.item with
      record
         Color : lucid_Color;
      end record;

end openGL.Model.sphere.lit_colored;
