with
     openGL.Font.texture,
     openGL.Geometry;

package openGL.Model.Text.lit_colored_textured
--
--  Models lit, colored, textured text.
--
is

   type Item is new Model.text.item with
      record
         Text     : access String;

         Font_Id  : openGL.Font.font_Id;
         Font     : openGL.Font.texture.view;

         Color    : lucid_Color;
         Centered : Boolean;
      end record;

   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Text (Scale    : in Vector_3;
                      Text     : in String;
                      Font     : in openGL.Font.font_Id;
                      Color    : in lucid_Color;
                      Centered : in Boolean := True) return View;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     openGL.Font.font_id_Map_of_font) return Geometry.views;
   overriding
   procedure Text_is (Self : in out Item;   Now : in String);
   overriding
   function  Text    (Self : in     Item)     return String;

   overriding
   function  Font    (Self : in     Item) return openGL.Font.view;

end openGL.Model.Text.lit_colored_textured;
