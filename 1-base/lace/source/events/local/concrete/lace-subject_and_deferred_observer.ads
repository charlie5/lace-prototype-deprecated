with lace.Subject,
     lace.Observer,
     lace.make_Subject,
     lace.make_Observer.deferred,
     lace.Any;



package lace.Subject_and_deferred_Observer
--
--  Provides a concrete type for a combined event Subject and deferred Observer.
--
is


   type Item is limited new lace.Any.limited_Item
                        and lace.Subject    .Item
                        and lace.Observer   .Item with private;

   type View is access all Item'Class;




   package Forge
   is
      function  to_Subject_and_Observer (Name : in String) return Item;
      function new_Subject_and_Observer (Name : in String) return View;
   end Forge;

   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   overriding
   function Name (Self : in Item) return String;





private

   package Subject  is new make_Subject      (Any.limited_Item);
   package Observer is new make_Observer     (Subject    .Item);
   package Deferred is new Observer.deferred (Observer   .Item);


   type Item is limited new Deferred.item with
      record
         Name : access String;
      end record;


end lace.Subject_and_deferred_Observer;
