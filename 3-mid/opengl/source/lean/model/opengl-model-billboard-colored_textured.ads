with
     openGL.Geometry.colored_textured,
     openGL.Texture,
     openGL.Font,
     openGL.Palette;


package openGL.Model.billboard.colored_textured
--
--  Models a colored, textured billboard.
--
is

   type Coordinates is array (1 .. 4) of Coordinate_2D;


   type Item is new openGL.Model.billboard.item with
      record
         Color          : lucid_Color           := (openGL.Palette.White, openGL.Opaque);

         texture_Name   : asset_Name            := null_Asset;
         Texture        : openGL.Texture.Object := openGL.Texture.null_Object;      -- The texture to be applied to the billboard face.
         texture_Coords : Coordinates           := ((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0));

         is_Modified    : Boolean               := False;

         Vertices : access openGL.Geometry.colored_textured.Vertex_array := new openGL.geometry.colored_textured.Vertex_array (1 .. 4);
         Geometry : access openGL.Geometry.colored_textured.item'Class;
      end record;

   type View is access all Item'Class;



   ---------
   --- Forge
   --

   function new_Billboard (Scale   : in math.Vector_3;
                           Plane   : in billboard.Plane;
                           Color   : in openGL.lucid_Color;
                           Texture : in openGL.asset_Name) return View;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries   (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                       Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;


   procedure Color_is           (Self : in out Item;   Now : in lucid_Color);
   procedure Texture_Coords_are (Self : in out Item;   Now : in Coordinates);
   procedure Scale_is           (Self : in out Item;   Now : in Vector_3);

   overriding
   procedure modify      (Self : in out Item);

   overriding
   function  is_Modified (Self : in     Item) return Boolean;


end openGL.Model.billboard.colored_textured;
