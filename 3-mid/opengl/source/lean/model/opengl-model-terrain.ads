with
     openGL.Geometry;


package openGL.Model.terrain
--
--  Models lit, textured terrain.
--
is

   type height_Map_view is access all openGL.height_Map;


   type Item is new openGL.Model.item with
      record
--           heights_Asset : asset_Name    := null_Asset;

         Heights       : height_Map_view;
--           Row, Col      : Integer;

         color_Map     : asset_Name    := null_Asset;  -- Texture must be square, atm.
         Tiling        : openGL.texture_Transform_2d;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Item (-- heights_Asset : in asset_Name;
                      -- Row, Col      : in Integer;
                      Heights       : in height_Map_view;
                      color_Map     : in asset_Name;
                      Tiling        : in openGL.texture_Transform_2d :=  (s => (0.0, 1.0),
                                                                          t => (0.0, 1.0))) return View;

   overriding
   procedure destroy (Self : in out Item);



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;


private

   overriding
   procedure set_Bounds (Self : in out Item);

end openGL.Model.terrain;
