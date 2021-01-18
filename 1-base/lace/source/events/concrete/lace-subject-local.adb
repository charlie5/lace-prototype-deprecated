with
     ada.unchecked_Deallocation;

package body lace.Subject.local
is
   package body Forge
   is
      function to_Subject (Name : in event.subject_Name) return Item
      is
      begin
         return Self : Item
         do
            Self.Name := new event.subject_Name' (Name);
         end return;
      end to_Subject;


      function new_Subject (Name : in event.subject_Name) return View
      is
         Self : constant View := new Item' (to_Subject (Name));
      begin
         return Self;
      end new_Subject;
   end Forge;


   overriding
   procedure destroy (Self : in out Item)
   is
      procedure free is new ada.unchecked_Deallocation (event.subject_Name, Name_view);
   begin
      Subject.destroy (Subject.item (Self));   -- Destroy base class.
      free (Self.Name);
   end destroy;


   overriding
   function Name (Self : in Item) return event.subject_Name
   is
   begin
      return Self.Name.all;
   end Name;

end lace.Subject.local;
