with
     openGL.Font;


package openGL.Model.text
--
--  Provides an abstract model for rendering text.
--
is

   type Item is abstract new openGL.Model.item with
      record
         null;
      end record;

   type View is access all Item'Class;


   procedure Text_is (Self : in out Item;   Now : in String)            is abstract;
   function  Text    (Self : in     Item)     return String             is abstract;

   function  Font    (Self : in     Item)     return openGL.Font.view   is abstract;

end openGL.Model.text;
