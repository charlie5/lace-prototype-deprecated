with
     openGL.Model,
     openGL.Font;


package openGL.Model.text
--
--  Provides a model for rendering text.
--
is

   type Item is abstract new openGL.Model.item with
      record
         null;
      end record;

   procedure Text_is (Self : in out Item;   Now : in String)            is abstract;
   function  Text    (Self : in     Item)     return String             is abstract;

   function  Font    (Self : in     Item)     return openGL.Font.view   is abstract;

end openGL.Model.text;
