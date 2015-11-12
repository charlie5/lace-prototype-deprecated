with
     openGL.Model,
     openGL.Font,
     openGL.Geometry;


package openGL.Model.polygon.lit_colored
--
--  Models a lit, colored polygon.
--
is

   type Item is new openGL.Model.polygon.item with
      record
         Color        : openGL.lucid_Color;

         Vertices     : Vector_2_array (1 .. 8);
         vertex_Count : Natural                := 0;

         Bounds       : openGL.Bounds;
      end record;

   type View is access all Item'Class;



   package Forge
   is
      function new_Polygon (Vertices : in Vector_2_array;    Color : openGL.lucid_Color) return View;
   end Forge;



   overriding
   function  Bounds           (Self : in     Item) return openGL.Bounds;

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

end openGL.Model.polygon.lit_colored;
