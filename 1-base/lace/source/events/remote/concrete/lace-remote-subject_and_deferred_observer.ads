with
     lace.remote.Subject,
     lace.remote.Observer,
     lace.remote.make_Subject,
     lace.remote.make_Observer.deferred,
     lace.Any;

package lace.remote.Subject_and_deferred_Observer
--
--  Provides a concrete type for a combined event Subject and a deferred Observer.
--
is
   type Item is limited new lace.Any.    limited_item
                        and lace.remote.Subject. item
                        and lace.remote.Observer.item with private;

   type View is access all Item'Class;


   package Forge
   is
      function  to_Subject_and_Observer (Name : in String) return Item;
      function new_Subject_and_Observer (Name : in String) return View;
   end Forge;

   procedure destroy (Self : in out Item);


   overriding
   function Name (Self : in Item) return String;



private

   package Subject  is new make_Subject      (Any.limited_item);
   package Observer is new make_Observer     (Subject    .item);
   package Deferred is new Observer.deferred (Observer   .item);

   type Item is limited new Deferred.item with
      record
         Name : access String;
      end record;

end lace.remote.Subject_and_deferred_Observer;
