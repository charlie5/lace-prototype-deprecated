with
     ada.unchecked_Deallocation;

package body lace.Subject.local
is
   package body Forge
   is
      function to_Subject (Name : in String) return Item
      is
      begin
         return Self : Item
         do
            Self.Name := new String'(Name);
         end return;
      end to_Subject;


      function new_Subject (Name : in String) return View
      is
         Self : constant View := new Item'(to_Subject (Name));
      begin
         return Self;
      end new_Subject;
   end Forge;


   overriding
   procedure destroy (Self : in out Item)
   is
      procedure free is new ada.unchecked_Deallocation (String, String_view);
   begin
      Subject.destroy (Subject.item (Self));
      free (Self.Name);
   end destroy;


   overriding
   function Name (Self : in Item) return String
   is
   begin
      return Self.Name.all;
   end Name;

end lace.Subject.local;
