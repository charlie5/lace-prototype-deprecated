with
     openGL.Model,
     openGL.Font.texture,
     openGL.Geometry;


package openGL.Model.Text.lit_colored_textured
--
--  Models a lit, colored text.
--
is

   type Item is new openGL.Model.text.item with
      record
         Text     : access String;

         Font_Id  : openGL.Font.font_Id;
         Font     : openGL.Font.texture.View;

         Color    : openGL.lucid_Color;
         Centered : Boolean;

--           Bounds   : openGL.Bounds;
      end record;

   type View is access all Item'Class;



   ---------
   --- Forge
   --

   package Forge
   is
      function new_Text (Scale    : in math.Vector_3;
                         Text     : in String;
                         Font     : in openGL.Font.font_Id;
                         Color    : in openGL.lucid_Color;
                         Centered : in Boolean            := True) return View;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     openGL.Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;
   overriding
   procedure Text_is    (Self : in out Item;   Now : in String);
   overriding
   function  Text       (Self : in     Item)     return String;

   overriding
   function  Font       (Self : in     Item) return openGL.Font.view;

--     overriding
--     function  Bounds           (Self : in     Item) return openGL.Bounds;

   overriding
   procedure set_Bounds (Self : in out Item);

end openGL.Model.Text.lit_colored_textured;
