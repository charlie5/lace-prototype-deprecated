with
     lace.remote.Observer,
     lace.Event.utility,
     ada.Text_IO;

package body chat.Client.local
is
   -- Globals
   --

   the_Response : aliased chat.Client.local.show;


   -- Forge
   --

   function to_Client (Name : in String) return Item
   is
      use ada.Strings.unbounded;
   begin
      return Self : Item
      do
         Self.Name := to_unbounded_String (Name);
      end return;
   end to_Client;


   function new_Client (Name : in String) return View
   is
      Self : constant View := new Item'(to_Client (Name));
   begin
      return Self;
   end new_Client;


   -- Attributes
   --

   overriding
   function Name (Self : in Item) return String
   is
      use ada.Strings.unbounded;
   begin
      return to_String (Self.Name);
   end Name;


   overriding
   function as_Observer (Self : access Item) return lace.remote.Observer.view
   is
   begin
      return Self;
   end as_Observer;


   -- Operations
   --

   overriding
   procedure register_Client (Self : in out Item;   other_Client : in Client.view)
   is
      use lace.Event.utility,
          ada.Text_IO;
   begin
      Self.register (other_Client.as_Observer,
                     to_Kind (chat.Client.Message'Tag));

      Self.add (the_Response'Access,
                to_Kind (chat.Client.Message'Tag),
                other_Client.Name);

      put_Line (other_Client.Name & " is here.");
   end register_Client;


   overriding
   procedure deregister_Client (Self : in out Item;   other_Client : in Client.view)
   is
      use lace.Event.utility,
          ada.Text_IO;
   begin
      Self.deregister (other_Client.as_Observer,
                       to_Kind (chat.Client.Message'Tag));

      Self.rid (the_Response'unchecked_Access,
                to_Kind (chat.Client.Message'Tag),
                other_Client.Name);

      put_Line (other_Client.Name & " leaves.");
   end deregister_Client;


   overriding
   procedure Registrar_has_shutdown  (Self : in out Item)
   is
   begin
      ada.Text_IO.put_Line ("The Registrar has shutdown. Press <Enter> to exit.");
      Self.Registrar_has_shutdown := True;
   end Registrar_has_shutdown;


   function Registrar_has_shutdown (Self : in Item) return Boolean
   is
   begin
      return Self.Registrar_has_shutdown;
   end Registrar_has_shutdown;


   -- Responses
   --

   overriding
   procedure respond (Self : in out Show;   to_Event : in lace.Event.item'Class)
   is
      use ada.Text_IO;
      the_Message : constant Message := Message (to_Event);
   begin
      put_Line (the_Message.Text (1 .. the_Message.Length));
   end respond;

end chat.Client.local;
