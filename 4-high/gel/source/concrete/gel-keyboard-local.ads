with
     lace.Subject.local;

package gel.Keyboard.local
--
--  Provides a concrete keyboard.
--
is
   type Item is limited new lace.Subject.local.item
                        and gel.Keyboard.item with private;

   type View is access all Item'class;



   package Forge
   is
      function  to_Keyboard (of_Name : in String) return Item;
      function new_Keyboard (of_Name : in String) return View;
   end Forge;


   procedure free (Self : in out View);


   --------------
   --- Attributes
   --

   overriding
   function Modifiers (Self : in Item) return Modifier_Set;


   --------------
   --- Operations
   --

   overriding
   procedure emit_key_press_Event   (Self : in out Item;   Key      : in keyboard.Key;
                                                           key_Code : in Integer);
   overriding
   procedure emit_key_release_Event (Self : in out Item;   Key      : in keyboard.Key);



private

   type Item is limited new lace.Subject.local.item
                        and gel.Keyboard.item with
      record
         Modifiers : Modifier_Set := no_Modifiers;
      end record;

end gel.Keyboard.local;
