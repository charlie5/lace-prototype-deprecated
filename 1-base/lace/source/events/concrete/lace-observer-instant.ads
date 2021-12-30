with
     lace.make_Observer,
     lace.Any;

private
with
     ada.Strings.unbounded;


package lace.Observer.instant
--
--  Provides a concrete instant event observer.
--
is
   type Item is limited new Any.limited_item
                        and Observer   .item with private;

   type View is access all Item'Class;


   package Forge
   is
      function new_Observer (Name : in Event.observer_Name) return View;
   end Forge;


   overriding
   function Name (Self : in Item) return Event.observer_Name;



private
   use ada.Strings.unbounded;

   package Observer is new make_Observer (Any.limited_item);

   type Item is limited new Observer.item with
      record
         Name : unbounded_String;
      end record;

end lace.Observer.instant;
