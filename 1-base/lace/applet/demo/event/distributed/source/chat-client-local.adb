with
     lace.event       .Utility,
     lace.event.remote.Utility,
     ada.text_IO;



package body chat.Client.local
is

   ------------
   --- Globals
   --

   the_Response : aliased chat.client.local.Show;




   ----------
   --- Forge
   --

   function to_Client (Name : in String) return Item
   is
   begin
      return Self : Item
      do
         Self.Name := new String' (Name);
      end return;
   end;



   function new_Client (Name : in String) return View
   is
      Self : View := new Item' (to_Client (Name));
   begin
      return Self;
   end;




   ---------------
   --- Attributes
   --

   function Name (Self : in Item) return String
   is
   begin
      return Self.Name.all;
   end;




   ---------------
   --- Operations
   --

   procedure register_Client (Self : in out Item;   Other : lace.remote.Subject.view)
   is
      use lace.event       .Utility,
          lace.event.remote.Utility;
   begin
      connect (lace.remote.Observer.view' (Self'unchecked_Access),
               Other,
               the_Response'unchecked_Access,
               to_Kind (chat.Client.Message'Tag));
   end;



   procedure deregister_Client (Self : in out Item;   Other : lace.remote.Observer.view)
   is
      use lace.event       .Utility,
          lace.event.remote.Utility;
   begin
      Self.deregister (Other,
                       to_Kind (chat.Client.Message'Tag));

      Self.rid (the_Response'unchecked_Access,
                to_Kind (chat.Client.Message'Tag),
                Other.Name);
   end;




   --------------
   --- Responses
   --

   procedure respond (Self : in out Show;   to_Event : in lace.Event.item'Class)
   is
      use ada.text_IO;
      the_Message : Message := Message (to_Event);
   begin
      put_Line (the_Message.Text (1 .. the_Message.Length));
   end;



end chat.Client.local;
