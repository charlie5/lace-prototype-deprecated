with
     ada.unchecked_Deallocation;

package body gel.Keyboard.local
is

   package body Forge
   is
      function to_Keyboard (of_Name : in String) return Item
      is
      begin
         return Self : constant Item := (lace.Subject.local.Forge.to_Subject (of_Name)
                                         with no_Modifiers)
         do
            null;
         end return;
      end to_Keyboard;


      function new_Keyboard (of_Name : in String) return View
      is
      begin
         return new Item' (to_Keyboard (of_Name));
      end new_Keyboard;

   end Forge;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;


   --------------
   --- Attributes
   --

   overriding
   function Modifiers (Self : in Item) return Modifier_Set
   is
   begin
      return Self.Modifiers;
   end Modifiers;


   --------------
   --- Operations
   --

   overriding
   procedure emit_key_press_Event (Self : in out Item;   Key      : in keyboard.Key;
                                                         key_Code : in Integer)
   is
      the_key_press_Event : key_press_Event;
   begin
      case Key is
         when LSHIFT    => Self.Modifiers (LSHIFT) := True;
         when RSHIFT    => Self.Modifiers (RSHIFT) := True;
         when LCTRL     => Self.Modifiers (LCTRL)  := True;
         when RCTRL     => Self.Modifiers (RCTRL)  := True;
         when LALT      => Self.Modifiers (LALT)   := True;
         when RALT      => Self.Modifiers (RALT)   := True;
         when LMETA     => Self.Modifiers (LMETA)  := True;
         when RMETA     => Self.Modifiers (RMETA)  := True;
         when NUMLOCK   => Self.Modifiers (NUM)    := True;
         when CAPSLOCK  => Self.Modifiers (CAPS)   := True;
         when MODE      => Self.Modifiers (MODE)   := True;
         when others    => null;
      end case;

      the_key_press_Event := ((Key, Self.Modifiers),  key_Code);
      Self.emit (the_key_press_Event);
   end emit_key_press_Event;



   overriding
   procedure emit_key_release_Event (Self : in out Item;   Key : in keyboard.Key)
   is
      the_key_release_Event : key_release_Event;
   begin
      case Key is
         when LSHIFT    => Self.Modifiers (LSHIFT) := False;
         when RSHIFT    => Self.Modifiers (RSHIFT) := False;
         when LCTRL     => Self.Modifiers (LCTRL)  := False;
         when RCTRL     => Self.Modifiers (RCTRL)  := False;
         when LALT      => Self.Modifiers (LALT)   := False;
         when RALT      => Self.Modifiers (RALT)   := False;
         when LMETA     => Self.Modifiers (LMETA)  := False;
         when RMETA     => Self.Modifiers (RMETA)  := False;
         when NUMLOCK   => Self.Modifiers (NUM)    := False;
         when CAPSLOCK  => Self.Modifiers (CAPS)   := False;
         when MODE      => Self.Modifiers (MODE)   := False;
         when others    => null;
      end case;

      the_key_release_Event := (modified_Key => (Key, Self.Modifiers));
      Self.emit (the_key_release_Event);
   end emit_key_release_Event;


end gel.Keyboard.local;
