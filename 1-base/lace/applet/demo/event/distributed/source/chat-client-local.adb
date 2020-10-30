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
   begin
      return Self : Item
      do
         Self.Name := new String'(Name);
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
   begin
      return Self.Name.all;
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
      use lace.Event.utility;
   begin
      Self.register (other_Client.as_Observer,
                     to_Kind (chat.Client.Message'Tag));

      Self.add (the_Response'Access,
                to_Kind (chat.Client.Message'Tag),
                other_Client.Name);
   end register_Client;


   overriding
   procedure deregister_Client (Self : in out Item;   Other : in lace.remote.Observer.view)
   is
      use lace.Event.utility;
   begin
      Self.deregister (Other,
                       to_Kind (chat.Client.Message'Tag));

      Self.rid (the_Response'unchecked_Access,
                to_Kind (chat.Client.Message'Tag),
                Other.Name);
   end deregister_Client;


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
