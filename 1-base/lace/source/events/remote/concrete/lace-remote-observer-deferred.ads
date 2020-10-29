with
     lace.remote.make_Observer.deferred,
     lace.Any;

package lace.remote.Observer.deferred
--
--  Provides a concrete deferred remote event Observer.
--
is
   type Item is limited new Any.limited_item
                        and remote.Observer.item with private;

   type View is access all Item'Class;


   package Forge
   is
      function to_Observer  (Name : in String) return Item;
      function new_Observer (Name : in String) return View;
   end Forge;


   overriding
   function Name (Self : in Item) return String;



private

   package Observer is new lace.remote.make_Observer (Any.limited_item);
   package Deferred is new Observer.deferred (Observer.item);

   type Item is limited new Deferred.item with
      record
         Name : access String;
      end record;

end lace.remote.Observer.deferred;


