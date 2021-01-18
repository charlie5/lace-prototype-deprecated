with
     lace.make_Subject,
     lace.Any;

package lace.Subject.local
--
--  Provides a concrete local event Subject.
--
is
   type Item is limited new Any.limited_item
                        and Subject    .item with private;

   type View is access all Item'Class;


   package Forge
   is
      function  to_Subject (Name : in event.subject_Name) return Item;
      function new_Subject (Name : in event.subject_Name) return View;
   end Forge;

   procedure destroy (Self : in out Item);

   overriding
   function Name (Self : in Item) return event.subject_Name;


private

   type Name_view is access all event.subject_Name;

   package Subject is new make_Subject (Any.limited_item);

   type Item is limited new Subject.item with
      record
         Name : Name_view;
      end record;

end lace.Subject.local;
