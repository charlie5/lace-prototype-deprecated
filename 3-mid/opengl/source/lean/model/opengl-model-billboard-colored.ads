with
     openGL.Geometry.colored,
     openGL.Texture,
     openGL.Font,
     openGL.Palette;


package openGL.Model.billboard.colored
--
--  Models a colored, textured billboard.
--
is
   type Item is new Model.billboard.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Billboard (Size    : in Size_t         := default_Size;
                           Plane   : in billboard.Plane;
                           Color   : in lucid_Color;
                           Texture : in asset_Name) return View;

   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries   (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                       Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;

   procedure Color_is           (Self : in out Item;   Now : in lucid_Color);
   procedure Texture_Coords_are (Self : in out Item;   Now : in Coordinates);

   overriding
   procedure modify      (Self : in out Item);

   overriding
   function  is_Modified (Self : in     Item) return Boolean;



private

   type Item is new Model.billboard.item with
      record
         Color          : lucid_Color           := (Palette.White, Opaque);

         texture_Name   : asset_Name            := null_Asset;
         Texture        : openGL.Texture.Object := openGL.Texture.null_Object;      -- The texture to be applied to the billboard face.
         texture_Coords : Coordinates           := ((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0));

         is_Modified    : Boolean               := False;

         Vertices : access Geometry.colored.Vertex_array := new geometry.colored.Vertex_array (1 .. 4);
         Geometry : access Geometry.colored.item'Class;
      end record;

end openGL.Model.billboard.colored;
