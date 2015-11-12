with
     openGL.remote_Model,
     openGL.Font,
     openGL.Texture,
     openGL.Geometry;


package openGL.Model
--
--  Provides a type for 3D models.
--
is
   use Geometry_3d;


   type access_Geometry_views is access openGL.Geometry.views;


   type Item is abstract new openGL.remote_Model.item with
      record
         opaque_Geometries : access_Geometry_views;
         lucid_Geometries  : access_Geometry_views;

         needs_Rebuild     : safe_Boolean := False;
      end record;

   type View is access all Item'Class;



   --------
   -- Forge
   --

   procedure define  (Self : in out Item;   Scale : in Vector_3);
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);



   -------------
   -- Attributes
   --

   function  Bounds           (Self : in     Item) return openGL.Bounds is abstract;

   function  Id               (Self : in     Item'Class)     return openGL.Model_Id;
   procedure Id_is            (Self : in out Item'Class;   Now : in openGL.Model_Id);


   procedure modify           (Self : in out Item)             is null;
   function  is_Modified      (Self : in     Item) return Boolean;

   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
                               is abstract;


   -------------
   -- Operations
   --

   procedure create_GL_Geometries (Self : in out Item'Class;   Textures : access Texture.name_Map_of_texture'Class;
                                                               Fonts    : in     Font.font_id_Maps_of_font.Map);


end openGL.Model;
