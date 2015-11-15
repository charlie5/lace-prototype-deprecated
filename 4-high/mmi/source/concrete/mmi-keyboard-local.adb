with
     ada.unchecked_Deallocation;


package body mmi.Keyboard.local
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




   --  Operations
   --

   overriding
   procedure emit_key_press_Event (Self : in out Item;   Key      : in keyboard.Key;
                                                         key_Code : in Integer)
   is
      the_key_press_Event : key_press_Event;
   begin
      case Key is
         when LSHIFT    => self.Modifiers (LSHIFT) := True;
         when RSHIFT    => self.Modifiers (RSHIFT) := True;
         when LCTRL     => self.Modifiers (LCTRL)  := True;
         when RCTRL     => self.Modifiers (RCTRL)  := True;
         when LALT      => self.Modifiers (LALT)   := True;
         when RALT      => self.Modifiers (RALT)   := True;
         when LMETA     => self.Modifiers (LMETA)  := True;
         when RMETA     => self.Modifiers (RMETA)  := True;
         when NUMLOCK   => self.Modifiers (NUM)    := True;
         when CAPSLOCK  => self.Modifiers (CAPS)   := True;
         when MODE      => self.Modifiers (MODE)   := True;
         when others    => null;
      end case;

      the_key_press_Event := ((Key, self.Modifiers),  key_Code);
      self.emit (the_key_press_Event);
   end emit_key_press_Event;



   overriding
   procedure emit_key_release_Event (Self : in out Item;   Key      : in keyboard.Key)
   is
      the_key_release_Event : key_release_Event;
   begin
      case Key is
         when LSHIFT    => self.Modifiers (LSHIFT) := False;
         when RSHIFT    => self.Modifiers (RSHIFT) := False;
         when LCTRL     => self.Modifiers (LCTRL)  := False;
         when RCTRL     => self.Modifiers (RCTRL)  := False;
         when LALT      => self.Modifiers (LALT)   := False;
         when RALT      => self.Modifiers (RALT)   := False;
         when LMETA     => self.Modifiers (LMETA)  := False;
         when RMETA     => self.Modifiers (RMETA)  := False;
         when NUMLOCK   => self.Modifiers (NUM)    := False;
         when CAPSLOCK  => self.Modifiers (CAPS)   := False;
         when MODE      => self.Modifiers (MODE)   := False;
         when others    => null;
      end case;

      the_key_release_Event := (modified_Key => (Key, self.Modifiers));
      self.emit (the_key_release_Event);
   end emit_key_release_Event;



   overriding
   function Modifiers (Self : in Item) return Modifier_Set
   is
   begin
      return self.Modifiers;
   end Modifiers;


end mmi.Keyboard.local;
