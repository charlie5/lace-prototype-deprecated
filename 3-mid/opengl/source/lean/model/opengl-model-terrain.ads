with openGL.Model,
     openGL.Geometry,
     ada.Streams;



package openGL.Model.terrain
--
--  models lit, textured terrain.
--
is

   type height_Map_view is access all openGL.height_Map;


   type Item is new openGL.Model.item with
      record
         heights_Asset : asset_Name    := null_Asset;

         Heights       : height_Map_view;
         Row, Col      : Integer;

         color_Map     : asset_Name    := null_Asset;  -- Texture must be square, atm.
         Tiling        : openGL.texture_Transform_2d;

--           Bounds        : openGL.Bounds := null_Bounds;
      end record;

   type View is access all Item'Class;


   package Forge
   is
      function new_Item (heights_Asset : in asset_Name;
                         Row, Col      : in Integer;
                         Heights       : in height_Map_view;
                         color_Map     : in asset_Name;
                         Tiling        : in openGL.texture_Transform_2d :=  (s => (0.0, 1.0),
                                                                             t => (0.0, 1.0))) return View;
   end Forge;





   overriding
   procedure destroy (Self : in out Item);

   overriding
   function  to_GL_Geometries     (Self : access Item;         Textures : access Texture.name_Map_of_texture'Class;
                                                               Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;
--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds;



   -----------
   --  Streams
   --

   procedure Item_write  (Stream : not null access Ada.Streams.Root_Stream_Type'Class;   Self : in  Item);
   for       Item'Write use Item_write;

   procedure Item_read   (Stream : not null access Ada.Streams.Root_Stream_Type'Class;   Self : out Item);
   for       Item'Read use Item_read;


   procedure Item_output (Stream : not null access Ada.Streams.Root_Stream_Type'Class;   Self : in  Item);
   for       Item'Output use Item_output;

   function Item_input   (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Item;
   for      Item'Input use Item_input;





private

   overriding
   procedure set_Bounds (Self : in out Item);

end openGL.Model.terrain;
