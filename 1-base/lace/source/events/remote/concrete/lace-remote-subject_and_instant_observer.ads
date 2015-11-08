with lace.make_Subject,
     lace.make_Observer.deferred,
     lace.Any,
     lace.Subject,
     lace.Observer;



package lace.remote.Subject_and_instant_Observer
--
--  Provides a concrete type for a combined event Subject and instant Observer.
--
is


   type Item is limited new lace.Any.limited_Item
                        and lace.Subject    .Item
                        and lace.Observer   .Item with private;

   type View is access all Item'Class;




   package Forge
   is
      function to_Subject_and_Observer (Name : in String) return Item;
   end Forge;



   overriding
   function Name (Self : in Item) return String;





private

   package Subject  is new make_Subject      (Any.limited_Item);
   package Observer is new make_Observer     (Subject    .Item);


   type Item is limited new Observer.item with
      record
         Name : access String;
      end record;


end lace.remote.Subject_and_instant_Observer;
