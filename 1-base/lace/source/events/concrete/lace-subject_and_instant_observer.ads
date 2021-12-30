with
     lace.make_Subject,
     lace.make_Observer,
     lace.Any,
     lace.Subject,
     lace.Observer;

private
with
     ada.Strings.unbounded;


package lace.Subject_and_instant_Observer
--
--  Provides a concrete type for a combined event subject and an instant observer.
--
is
   type Item is limited new lace.Any.limited_item
                        and lace.Subject    .item
                        and lace.Observer   .item with private;

   type View is access all Item'Class;


   package Forge
   is
      function to_Subject_and_Observer (Name : in String) return Item;
   end Forge;


   overriding
   function Name (Self : in Item) return String;



private
   use ada.Strings.unbounded;

   package Subject  is new make_Subject  (Any.limited_item);
   package Observer is new make_Observer (Subject    .item);

   type Item is limited new Observer.item with
      record
         Name : unbounded_String;
      end record;

end lace.Subject_and_instant_Observer;
