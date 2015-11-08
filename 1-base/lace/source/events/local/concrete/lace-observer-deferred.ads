with
     lace.Observer,
     lace.make_Observer.deferred,
     lace.Any;


package lace.Observer.deferred
--
--  Provides a concrete local event Observer.
--
is

   type Item is limited new Any.limited_Item
                        and Observer   .item with private;

   type View is access all Item'Class;



   package Forge
   is
      function to_Observer  (Name : in String) return Item;
      function new_Observer (Name : in String) return View;
   end Forge;



   overriding
   function Name (Self : in Item) return String;




private

   package my_Observer is new lace.make_Observer   (Any.limited_item);
   package my_Deferred is new my_Observer.deferred (my_Observer.item);

   type Item is limited new my_Deferred.item with
      record
         Name : access String;
      end record;

end lace.Observer.deferred;


