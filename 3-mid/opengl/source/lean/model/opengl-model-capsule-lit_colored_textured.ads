with
     openGL.Model,
     openGL.Geometry;


package openGL.Model.capsule.lit_colored_textured
--
--  Models a lit, colored, textured capsule.
--
is

   type Item is new openGL.Model.capsule.item with
      record
         Image : asset_Name := null_Asset;   -- Usually a mercator projection to be mapped onto the sphere.
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   package Forge
   is
      function new_Capsule (Radius : in math.Real;
                            Height : in math.Real;
                            Image  : in asset_Name := null_Asset) return View;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function  Bounds (Self : in Item) return openGL.Bounds;


   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

end openGL.Model.capsule.lit_colored_textured;
