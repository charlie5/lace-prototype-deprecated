with
     openGL.Geometry;


package openGL.Model.capsule.lit_colored_textured
--
--  Models a lit, colored and textured capsule.
--
is
   type Item is new Model.capsule.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Capsule (Radius : in Real;
                         Height : in Real;
                         Color  : in lucid_Color;
                         Image  : in asset_Name := null_Asset) return View;

   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;



private

   type Item is new Model.capsule.item with
      record
         Radius : Real;
         Height : Real;

         Color  : rgba_Color;
         Image  : asset_Name := null_Asset;
      end record;

end openGL.Model.capsule.lit_colored_textured;
