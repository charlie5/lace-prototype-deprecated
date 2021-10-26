with
     openGL.Font;


package openGL.Model.text
--
--  Provides an abstract model for rendering text.
--
is
   type Item is abstract new Model.item with private;
   type View is access all Item'Class;


   procedure Text_is (Self : in out Item;   Now : in String)   is abstract;
   function  Text    (Self : in     Item)     return String    is abstract;

   function  Font    (Self : in     Item)     return Font.view is abstract;

   no_such_Font : exception;



private

   type Item is abstract new Model.item with null record;

end openGL.Model.text;
