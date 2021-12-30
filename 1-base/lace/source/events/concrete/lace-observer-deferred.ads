with
     lace.make_Observer.deferred,
     lace.Any;

private
with
     ada.Strings.unbounded;


package lace.Observer.deferred
--
--  Provides a concrete deferred event observer.
--
is
   type Item is limited new Any.limited_item
                        and Observer   .item with private;

   type View is access all Item'Class;


   package Forge
   is
      function  to_Observer (Name : in Event.observer_Name) return Item;
      function new_Observer (Name : in Event.observer_Name) return View;
   end Forge;


   overriding
   function Name (Self : in Item) return Event.observer_Name;



private
   use ada.Strings.unbounded;

   package Observer is new lace.make_Observer (Any.limited_item);
   package Deferred is new Observer.deferred  (Observer.item);

   type Item is limited new Deferred.item with
      record
         Name : unbounded_String;
      end record;

end lace.Observer.deferred;
