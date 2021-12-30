package body lace.Subject.local
is
   package body Forge
   is
      function to_Subject (Name : in Event.subject_Name) return Item
      is
      begin
         return Self : Item
         do
            Self.Name := to_unbounded_String (Name);
         end return;
      end to_Subject;


      function new_Subject (Name : in Event.subject_Name) return View
      is
         Self : constant View := new Item' (to_Subject (Name));
      begin
         return Self;
      end new_Subject;
   end Forge;


   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      Subject.destroy (Subject.item (Self));   -- Destroy base class.
   end destroy;


   overriding
   function Name (Self : in Item) return Event.subject_Name
   is
   begin
      return to_String (Self.Name);
   end Name;

end lace.Subject.local;
