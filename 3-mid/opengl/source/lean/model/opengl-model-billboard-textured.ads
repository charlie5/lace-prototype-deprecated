with
     openGL.Model,
     openGL.Geometry,
     openGL.Texture;


package openGL.Model.billboard.textured
--
--  Models a textured billboard.
--
is

   type Coordinates is array (1 .. 4) of Coordinate_2D;
   type Image_view  is access openGL.Image;


   type Item is new openGL.Model.billboard.item with
      record
         texture_Name   : asset_Name            := null_Asset;
         Image          : Image_view;
         Texture        : openGL.Texture.Object := openGL.Texture.null_Object;      -- The texture to be applied to the billboard face.
         texture_Coords : Coordinates           := ((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0));
      end record;

   type View is access all Item'Class;



   ---------
   --- Forge
   --

   package Forge
   is
      function new_Billboard (Scale   : in math.Vector_3;
                              Plane   : in billboard.Plane;
                              Texture : in openGL.asset_Name) return View;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries   (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                       Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

   procedure Texture_Coords_are (Self : in out Item;   Now : in Coordinates);
   procedure Scale_is           (Self : in out Item;   Now : in Vector_3);
   procedure Image_is           (Self : in out Item;   Now : in openGL.Image);


end openGL.Model.billboard.textured;
