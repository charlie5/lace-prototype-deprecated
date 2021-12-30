with
     lace.Subject,
     lace.Observer,
     lace.make_Subject,
     lace.make_Observer.deferred,
     lace.Any;

private
with
     ada.Strings.unbounded;


package lace.Subject_and_deferred_Observer
--
--  Provides a concrete type for a combined event subject and a deferred observer.
--
is
   type Item is limited new lace.Any.limited_item
                        and lace.Subject    .item
                        and lace.Observer   .item with private;

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
   use ada.Strings.unbounded;

   package Subject  is new make_Subject      (Any.limited_item);
   package Observer is new make_Observer     (Subject    .item);
   package Deferred is new Observer.deferred (Observer   .item);

   type Item is limited new Deferred.item with
      record
         Name : unbounded_String;
      end record;

end lace.Subject_and_deferred_Observer;
