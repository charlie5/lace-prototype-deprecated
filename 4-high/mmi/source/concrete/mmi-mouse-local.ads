with
     lace.Subject.local;


package mmi.Mouse.local
--
-- Provides a concrete mouse.
--
is

   type Item is limited new lace.Subject.local.item
                        and mmi.Mouse         .item with private;

   type View is access all Item'Class;


   package Forge
   is
      function  to_Mouse (of_Name : in String) return Item;
      function new_Mouse (of_Name : in String) return View;
   end Forge;

   procedure free (Self : in out View);



private

   type Item is limited new lace.Subject.local.item
                        and mmi.Mouse         .item with
      record
         null;
      end record;

end mmi.Mouse.local;
