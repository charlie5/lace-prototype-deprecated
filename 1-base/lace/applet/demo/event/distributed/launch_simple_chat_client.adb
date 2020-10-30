with
     chat.Registrar,
     chat.Client.local,

     lace.remote.Event.utility,

     ada.command_Line,
     ada.Text_IO;

procedure launch_simple_chat_Client
--
-- Starts a chat client.
--
-- note: This demo is complicated by what seems a problem with interface conversions with RACW (see interface_test).
--       To work around the problem, a client is treated 'separately' as a chat.Client and a lace.Observer.
--
is
begin
   -- Usage
   --
   if ada.command_Line.argument_Count /= 1
   then
      ada.Text_IO.put_Line ("Usage:   $ ./launch_simple_chat_Client  <nickname>");
      return;
   end if;

   lace.remote.event.Utility.use_text_Logger ("events");

   declare
      use chat.Client.local;

      client_Name : constant String                 := ada.command_Line.Argument (1);
      the_Client  : constant chat.Client.local.view := new_Client (client_Name);
   begin
      -- Setup
      --
      chat.Registrar.register (the_Client.all'Access);   -- Register our client with the registrar.

      declare
         procedure broadcast (the_Text : in String)
         is
            the_Message : constant chat.client.Message := (client_Name'Length + 2 + the_Text'Length,
                                                           client_Name & ": " & the_Text);
         begin
            the_Client.emit (the_Message);
         end broadcast;

         Peers : constant chat.Client.views := chat.Registrar.all_Clients;

         use type chat.Client.view;
      begin
         for i in Peers'Range
         loop
            if the_Client.all'Access /= Peers (i)
            then
               Peers (i) .register_Client (the_Client.all'Access);   -- Register our client with all other clients.
               the_Client.register_Client (Peers (i));               -- Register all other clients with our client.
            end if;
         end loop;

         -- Main loop
         --
         loop
            declare
               chat_Message : constant String := ada.Text_IO.get_Line;
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
            chat.Registrar.deregister (the_Client.all'Access);

            declare
               Peers : constant chat.Client.views := chat.Registrar.all_Clients;
            begin
               for i in Peers'Range
               loop
                  if the_Client.all'Access /= Peers (i)
                  then
                     Peers (i).deregister_Client (the_Client.all'Access);   -- Deregister our client with every other client.
                  end if;
               end loop;
            end;
         end if;
      end;
   end;

   lace.remote.event.Utility.close;
end launch_simple_chat_Client;
