with -- lace.Observer,
     lace.remote.make_Observer,
     lace.Any;



package lace.remote.Observer.instant
--
--  Provides a concrete instant remote event Observer.
--
is

   type Item is limited new Any.limited_Item
                        and remote.Observer.item with private;

   type View is access all Item'Class;




   package Forge
   is
      function new_Observer (Name : in String) return View;
   end Forge;




   overriding
   function Name (Self : in Item) return String;





private

   package my_Observer is new lace.remote.make_Observer (Any.limited_item);

   type Item is limited new my_Observer.item with
      record
         Name : access String;
      end record;

end lace.remote.Observer.instant;


