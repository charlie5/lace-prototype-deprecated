with
     lace.Observer,
     lace.make_Observer,
     lace.Any;


package lace.Observer.instant
--
--  Provides a concrete local event Observer.
--
is

   type Item is limited new Any.limited_Item
                        and Observer   .item with private;

   type View is access all Item'Class;




   package Forge
   is
      function new_Observer (Name : in String) return View;
   end Forge;



   overriding
   function Name (Self : in Item) return String;




private

   package Observer is new make_Observer (Any.limited_item);

   type Item is limited new Observer.item with
      record
         Name : access String;
      end record;

end lace.Observer.instant;


