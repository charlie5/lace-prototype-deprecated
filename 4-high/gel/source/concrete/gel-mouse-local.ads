with
     lace.Subject.local;


package gel.Mouse.local
--
-- Provides a concrete mouse.
--
is
   type Item is limited new lace.Subject.local.item
                        and gel.Mouse         .item with private;

   type View is access all Item'Class;


   package Forge
   is
      function  to_Mouse (of_Name : in String) return Item;
      function new_Mouse (of_Name : in String) return View;
   end Forge;

   procedure free (Self : in out View);



private

   type Item is limited new lace.Subject.local.item
                        and gel.Mouse         .item with null record;

end gel.Mouse.local;
