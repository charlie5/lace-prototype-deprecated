with
     openGL.Geometry;


package openGL.Model.any
--
--  Provides a general 3D model.
--
--  This model is largely used by the IO importers of various model formats (ie collada, wavefront, etc).
--
is
   type Item is new Model.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Model (Model            : in asset_Name;
                       Texture          : in asset_Name;
                       Texture_is_lucid : in Boolean) return openGL.Model.any.view;

   --------------
   --- Attributes
   --

   function model_Name (Self : in Item) return asset_Name;

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;
   --
   --  Raises unsupported_model_Format when the model is not a :
   --     - wavefront       '.obj'
   --     - collada         '.dae'
   --     - lat_long_radius '.tab'

   unsupported_model_Format : exception;



private

   type Item is new Model.item with
      record
         Model             : asset_Name := null_Asset;   -- A wavefront '.obj' or collada '.dae' file.   -- TODO: Rename to 'model_Name'.

         Texture           : asset_Name := null_Asset;   -- The models texture image.
         has_lucid_Texture : Boolean    := False;

         Geometry          : openGL.Geometry.view;
      end record;

   procedure build_GL_Geometries (Self : in out Item);


end openGL.Model.any;
