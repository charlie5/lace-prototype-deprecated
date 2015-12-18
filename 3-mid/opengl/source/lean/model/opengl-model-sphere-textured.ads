with
     openGL.Model,
     openGL.Font,
     openGL.Geometry;


package openGL.Model.sphere.textured
--
--  Models a lit, colored, textured sphere.
--
is

   type Item is new openGL.Model.sphere.item with
      record
         Image        : asset_Name := null_Asset;     -- Usually a mercator projection to be mapped onto the sphere.
         is_Skysphere : Boolean    := False;
      end record;

   type View is access all Item'Class;



   package Forge
   is
      function new_Sphere (Radius       : in math.Real;
                           Image        : in asset_Name := null_Asset;
                           is_Skysphere : in Boolean    := False) return View;
   end Forge;


   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

end openGL.Model.sphere.textured;
