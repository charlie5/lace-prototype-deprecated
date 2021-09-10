with
     openGL.Font,
     openGL.Geometry;

package openGL.Model.sphere.textured
--
--  Models a textured sphere.
--
is

   type Item is new Model.sphere.item with     -- TODO: Make private.
      record
         Image        : asset_Name := null_Asset;     -- Usually a mercator projection to be mapped onto the sphere.
         is_Skysphere : Boolean    := False;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Sphere (Radius       : in Real;
                        Image        : in asset_Name := null_Asset;
                        is_Skysphere : in Boolean    := False) return View;

   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;

end openGL.Model.sphere.textured;
