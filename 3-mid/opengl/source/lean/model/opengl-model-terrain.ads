with
     openGL.Geometry;


package openGL.Model.terrain
--
--  Models lit, textured terrain.
--
is
   type Item is new Model.item with private;
   type View is access all Item'Class;


   type height_Map_view is access all height_Map;


   ---------
   --- Forge
   --

   function new_Terrain (heights_Asset : in asset_Name;
                         Row, Col      : in Integer;
                         Heights       : in height_Map_view;
                         color_Map     : in asset_Name;
                         Tiling        : in texture_Transform_2d := (S => (0.0, 1.0),
                                                                     T => (0.0, 1.0))) return View;
   overriding
   procedure destroy (Self : in out Item);


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;


private

   type Item is new Model.item with
      record
         heights_Asset : asset_Name := null_Asset;

         Heights       : height_Map_view;
         Row, Col      : Integer;

         color_Map     : asset_Name := null_Asset;
         Tiling        : texture_Transform_2D;
      end record;


   overriding
   procedure set_Bounds (Self : in out Item);

end openGL.Model.terrain;
