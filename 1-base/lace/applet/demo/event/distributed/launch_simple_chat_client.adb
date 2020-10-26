with
     chat.Registrar,
     chat.Client.local,

     lace.Event.utility,
     lace.remote.Observer,
     lace.remote.Subject,
     lace.remote.Event.utility,

     ada.command_Line,
     ada.Text_IO;

procedure launch_simple_chat_Client
--
-- Starts a chat client.
--
-- note: This demo is complicated by what seems a problem with interface conversions with RACW (see interface_test).
--       To work around the problem, a client is treated 'separately' as a chat.Client and a lace.Subject.
--
is
   use ada.Text_IO;
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
      use type lace.remote.Subject.view;

      client_Name : constant String                 := ada.command_Line.Argument (1);
      the_Client  : constant chat.Client.local.view := new_Client (client_Name);
   begin
      -- Setup
      --

      -- Register our client with the registrar.
      --
      chat.Registrar.register (chat.Client.remote        (the_Client));
      chat.Registrar.register (lace.remote.Subject .view (the_Client));
      chat.Registrar.register (lace.remote.Observer.view (the_Client));

      declare
         use type chat.Client.remote,
                  lace.Remote.Observer.view;

         procedure broadcast (the_Text : in String)
         is
            the_Message : constant chat.client.Message := (client_Name'Length + 2 + the_Text'Length,
                                                           client_Name & ": " & the_Text);
         begin
            the_Client.emit (the_Message);
         end broadcast;

         Peers              : constant chat.Client.remotes        := chat.Registrar.all_Clients;
         Peers_as_subjects  : constant lace.remote.Subject .views := chat.Registrar.all_Subjects;
         Peers_as_observers : constant lace.Remote.Observer.Views := chat.Registrar.all_Observers;

         use lace.Event.utility;
      begin
         -- Register our client with all other clients.
         --
         for i in Peers'Range
         loop
            if chat.Client.remote (the_Client) /= Peers (i)
            then
               Peers (i).register_Client (lace.remote.Observer.view'(the_Client.all'Access));   -- Register our client as a subject with every other client.
            end if;
         end loop;

         for i in Peers_as_observers'Range
         loop
            if lace.Remote.Observer.view (the_Client) /= Peers_as_observers (i)
            then
               the_Client.register (Peers_as_observers (i),
                                    to_Kind (chat.Client.Message'Tag));
            end if;
         end loop;

         for i in Peers_as_subjects'Range
         loop
            if lace.remote.Subject.view (the_Client) /= Peers_as_subjects (i)
            then
               the_Client.register_Client (Peers_as_subjects (i));   -- Register every other client with our client.
               Peers_as_subjects (i).register (lace.remote.Observer.view (the_Client),
                                               to_Kind (chat.Client.Message'Tag));
            end if;
         end loop;

         -- Main loop
         --
         loop
            declare
               chat_Message : constant String := ada.Text_IO.get_Line;
            begin
               exit when chat_Message = "q";
               broadcast (chat_Message);
            end;
         end loop;

         -- Shutdown
         --
         chat.Registrar.deregister (chat.Client.remote       (the_Client));
         chat.Registrar.deregister (lace.remote.Subject.view (the_Client));

         declare
            use lace.remote.Event.utility;
            Peers : constant chat.Client.remotes := chat.Registrar.all_Clients;
         begin
            for i in Peers'Range
            loop
               if chat.Client.remote (the_Client) /= Peers (i)
               then
                  Peers (i).deregister_Client (lace.remote.Observer.view (the_Client));   -- Deregister our client with every other client.
               end if;
            end loop;
         end;
      end;
   end;

   lace.remote.event.Utility.close;
end launch_simple_chat_Client;
