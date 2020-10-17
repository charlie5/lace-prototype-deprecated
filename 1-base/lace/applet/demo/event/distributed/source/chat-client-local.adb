with
     lace.event       .Utility,
     lace.remote.Event.Utility,
     ada.Text_IO;

package body chat.Client.local
is
   -- Globals
   --

   the_Response : aliased chat.client.local.Show;


   --- Forge
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


   --- Operations
   --

   overriding
   procedure register_Client (Self : in out Item;   Other : lace.remote.Subject.view)
   is
      use lace.event       .Utility,
          lace.remote.Event.utility;
   begin
      connect (lace.remote.Observer.view' (Self'unchecked_Access),
               Other,
               the_Response'unchecked_Access,
               to_Kind (chat.Client.Message'Tag));
   end register_Client;


   overriding
   procedure deregister_Client (Self : in out Item;   Other : lace.remote.Observer.view)
   is
      use lace.Event       .utility,
          lace.remote.Event.utility;
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
