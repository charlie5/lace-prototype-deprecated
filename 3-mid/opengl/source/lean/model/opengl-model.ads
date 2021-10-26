with
     openGL.remote_Model,
     openGL.Font,
     openGL.Texture,
     openGL.Geometry;


package openGL.Model
--
--  Provides an abstract base class for 3D models.
--
--  TODO: Make subprograms and 'with's private where possible.
is
   use Geometry_3d;

   type Item  is abstract new remote_Model.item with private;
   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   --------
   -- Forge
   --

   procedure define  (Self :    out Item);     -- TODO: Rid this.
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   -------------
   -- Attributes
   --

   function  Id               (Self : in     Item'Class)     return model_Id;
   procedure Id_is            (Self : in out Item'Class;   Now : in model_Id);

   procedure    modify        (Self : in out Item) is null;
   function  is_modified      (Self : in     Item) return Boolean;

   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return Geometry.views
                               is abstract;

   type access_Geometry_views is access Geometry.views;

   function opaque_Geometries (Self : in     Item) return access_Geometry_views;
   function  lucid_Geometries (Self : in     Item) return access_Geometry_views;


   procedure set_Bounds       (Self : in out Item);
   --
   -- Recalculate the bounds based on model geometry.

   function  Bounds           (Self : in     Item) return openGL.Bounds;
   --
   -- Returns the bounds in model space.


   function  needs_Rebuild    (Self : in     Item) return Boolean;
   procedure needs_Rebuild    (Self : in out Item);


   -------------
   -- Operations
   --

   procedure create_GL_Geometries (Self : in out Item'Class;   Textures : access Texture.name_Map_of_texture'Class;
                                                               Fonts    : in     Font.font_id_Map_of_font);


private

   type Item is abstract new remote_Model.item with
      record
         opaque_Geometries : access_Geometry_views;
         lucid_Geometries  : access_Geometry_views;

         Bounds            : openGL.Bounds := null_Bounds;   -- The combined bounds of all geometries.
         needs_Rebuild     : safe_Boolean  := False;
      end record;

end openGL.Model;
