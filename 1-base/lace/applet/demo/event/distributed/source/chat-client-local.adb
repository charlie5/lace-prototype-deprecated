with
     chat.Registrar,

     lace.remote.Response,
     lace.remote.Observer,
     lace.remote.Event.Utility,
     lace.Event.Utility,

     system.RPC,
     ada.Text_IO;

package body chat.Client.local
is

   procedure Last_Chance_Handler (Msg  : in system.Address;
                                  Line : in Integer);

   pragma Export (C, Last_Chance_Handler,
                  "__gnat_last_chance_handler");

   procedure Last_Chance_Handler (Msg  : in System.Address;
                                  Line : in Integer)
   is
      pragma Unreferenced (Msg, Line);
      use ada.Text_IO;
   begin
      put_Line ("The Registar is not running.");
      put_Line ("Press Ctrl-C to quit.");
      delay Duration'Last;
   end Last_Chance_Handler;


   -- Responses
   --
   type Show is new lace.remote.Response.item with null record;

   -- Response is to display the chat message on the users console.
   --
   overriding
   procedure respond (Self : in out Show;   to_Event : in lace.Event.item'Class)
   is
      pragma Unreferenced (Self);
      use ada.Text_IO;
      the_Message : constant Message := Message (to_Event);
   begin
      put_Line (the_Message.Text (1 .. the_Message.Length));
   end respond;

   the_Response : aliased chat.Client.local.show;


   -- Forge
   --
   function to_Client (Name : in String) return Item
   is
   begin
      return Self : Item
      do
         Self.Name := to_unbounded_String (Name);
      end return;
   end to_Client;


   -- Attributes
   --
   function "+" (From : in unbounded_String) return String
                 renames to_String;

   overriding
   function Name (Self : in Item) return String
   is
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
   begin
      Self.rid_Client (other_Client.Name,
                       other_Client.as_Observer);
   end deregister_Client;


   overriding
   procedure rid_Client (Self : in out Item;   other_Client_Name     : in String;
                                               other_Client_Observer : in lace.remote.Observer.view)
   is
      use lace.Event.utility,
          ada.Text_IO;
   begin
      Self.deregister (other_Client_Observer,
                       to_Kind (chat.Client.Message'Tag));

      Self.rid (the_Response'unchecked_Access,
                to_Kind (chat.Client.Message'Tag),
                other_Client_Name);

      put_Line (other_Client_Name & " leaves.");
   end rid_Client;



   overriding
   procedure Registrar_has_shutdown  (Self : in out Item)
   is
   begin
      ada.Text_IO.put_Line ("The Registrar has shutdown. Press <Enter> to exit.");
      Self.Registrar_has_shutdown := True;
   end Registrar_has_shutdown;


   procedure start (the_Client : in out chat.Client.local.item)
   is
      use ada.Text_IO;
   begin
      -- Setup
      --
      begin
         chat.Registrar.register (the_Client'unchecked_Access);   -- Register our client with the registrar.
      exception
         when chat.Registrar.Name_already_used =>
            put_Line (+the_Client.Name & " is already in use.");
            return;
      end;

      lace.remote.event.Utility.use_text_Logger ("events");

      declare
         procedure broadcast (the_Text : in String)
         is
            the_Message : constant chat.client.Message := (Length (the_Client.Name) + 2 + the_Text'Length,
                                                           +the_Client.Name & ": " & the_Text);
         begin
            the_Client.emit (the_Message);
         end broadcast;

      begin
         declare
            Peers : constant chat.Client.views := chat.Registrar.all_Clients;
         begin
            for i in Peers'Range
            loop
               if the_Client'unchecked_Access /= Peers (i)
               then
                  Peers (i) .register_Client (the_Client'unchecked_Access);   -- Register our client with all other clients.
                  the_Client.register_Client (Peers (i));                     -- Register all other clients with our client.
               end if;
            end loop;
         end;

         -- Main loop
         --
         loop
            declare
               chat_Message : constant String := get_Line;
            begin
               exit
                 when chat_Message = "q"
                 or   the_Client.Registrar_has_shutdown;
               broadcast (chat_Message);
            end;
         end loop;

         -- Shutdown
         --
         if not the_Client.Registrar_has_shutdown
         then
            begin
               chat.Registrar.deregister (the_Client'unchecked_Access);
            exception
               when system.RPC.Communication_Error =>
                  the_Client.Registrar_is_dead := True;
            end;

            if not the_Client.Registrar_is_dead
            then
               declare
                  Peers : constant chat.Client.views := chat.Registrar.all_Clients;
               begin
                  for i in Peers'Range
                  loop
                     if the_Client'unchecked_Access /= Peers (i)
                     then
                        begin
                           Peers (i).deregister_Client (the_Client'unchecked_Access);   -- Deregister our client with every other client.
                        exception
                           when system.RPC.Communication_Error =>
                              null;   -- Peer is dead, so do nothing.
                        end;
                     end if;
                  end loop;
               end;
            end if;
         end if;
      end;

      lace.remote.event.Utility.close;
   end start;

end chat.Client.local;
