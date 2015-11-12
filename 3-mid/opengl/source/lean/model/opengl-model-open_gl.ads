with
     openGL.Model,
     openGL.Geometry;


package openGL.Model.open_gl
--
--  Provides a general 3D model.
--
is

   type Geometry_view is access all openGL.Geometry.item'Class;


   type Item is new openGL.Model.item with
      record
         Model             : asset_Name   := null_Asset;   -- A wavefront '.obj' or collada '.dae' file.

         Texture           : asset_Name   := null_Asset;   -- The models texture image.
         has_lucid_Texture : Boolean      := False;

         Bounds            : openGL.Bounds;
         Geometry          : Geometry_view;
      end record;

   type View is access all Item'Class;



   ---------
   --- Forge
   --

   package Forge is
      function  to_Model (Scale            : in math.Vector_3;
                          Model            : in asset_Name;
                          Texture          : in asset_Name;
                          Texture_is_lucid : in Boolean) return openGL.Model.open_gl.item;

      function new_Model (Scale            : in math.Vector_3;
                          Model            : in asset_Name;
                          Texture          : in asset_Name;
                          Texture_is_lucid : in Boolean) return openGL.Model.open_gl.view;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function  Bounds           (Self : in     Item) return openGL.Bounds;

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;
   --
   --  Raises unsupported_model_Format when the Model is not a wavefront '.obj' or a collada '.dae' file.

   unsupported_model_Format : exception;




private

   procedure build_GL_Geometries (Self : in out Item);

end openGL.Model.open_gl;
