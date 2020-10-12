with
     lace.remote.make_Subject,
     lace.Any;

package lace.remote.Subject.local
--
--  Provides a concrete local event Subject.
--
is
   type Item is limited new Any.limited_item
                        and Subject    .Item with private;

   type View is access all Item'Class;


   package Forge
   is
      function  to_Subject (Name : in String) return Item;
      function new_Subject (Name : in String) return View;
   end Forge;

   procedure destroy (Self : in out Item);


   overriding
   function Name (Self : in Item) return String;



private

   type String_view is access all String;

   package Subject is new make_Subject (Any.limited_item);

   type Item is limited new Subject.item with
      record
         Name : String_view;
      end record;

end lace.remote.Subject.local;
