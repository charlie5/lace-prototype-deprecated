with
     interfaces.C.Strings;


package body openGL.Display
is


   function Default return Item
   is
      Self    : Display.item;
   begin
      return Self;
   end Default;



   function screen_Id (Self : in Item) return interfaces.c.int
   is
   begin
      return Self.screen_Id;
   end screen_Id;


end openGL.Display;
