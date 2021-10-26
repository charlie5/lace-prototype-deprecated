with
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.billboard.textured
--
--  Models a textured billboard.
--
is
   type Coordinates is array (1 .. 4) of Coordinate_2D;

   type       Image_view  is access       Image;
   type lucid_Image_view  is access lucid_Image;


   type Item (Lucid : Boolean) is new Model.billboard.item with     -- TODO: Make this private.
      record
         texture_Name   : asset_Name            := null_Asset;
         Texture        : openGL.Texture.Object := openGL.Texture.null_Object;      -- The texture to be applied to the billboard face.
         texture_Coords : Coordinates           := ((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0));     -- TODO: Should be constant/static ?

         case Lucid is
            when True  => lucid_Image : lucid_Image_view;
            when False =>       Image :       Image_view;
         end case;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   package Forge
   is
      function new_Billboard (Size    : in Size_t         := default_Size;
                              Plane   : in billboard.Plane;
                              Texture : in asset_Name;
                              Lucid   : in Boolean        := False) return View;
   end Forge;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries   (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                       Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;

   procedure Texture_Coords_are (Self : in out Item;   Now : in Coordinates);
   procedure Size_is            (Self : in out Item;   Now : in Size_t);

   procedure Image_is           (Self : in out Item;   Now : in       Image);
   procedure Image_is           (Self : in out Item;   Now : in lucid_Image);


end openGL.Model.billboard.textured;
