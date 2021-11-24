with
     openGL.Model,
     openGL.Font,
     openGL.Geometry;


package openGL.Model.polygon.lit_colored
--
--  Models a lit, colored polygon.
--
is
   type Item is new Model.polygon.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --
   function new_Polygon (Vertices : in Vector_2_array;
                         Color    : in lucid_Color) return View;

   --------------
   --- Attributes
   --
   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;


private

   type Item is new Model.polygon.item with
      record
         Color        : lucid_Color;

         Vertices     : Vector_2_array (1 .. 8);
         vertex_Count : Natural := 0;
      end record;

end openGL.Model.polygon.lit_colored;
