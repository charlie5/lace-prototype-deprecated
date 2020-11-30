with
     chat.Registrar,
     chat.Client.local,

     lace.remote.Event.utility,

     ada.Characters.latin_1,
     ada.command_Line,
     ada.Text_IO,
     ada.Exceptions;

procedure launch_simple_chat_Client
--
-- Starts a chat client.
--
is
   use ada.Text_IO;
begin
   -- Usage
   --
   if ada.command_Line.argument_Count /= 1
   then
      put_Line ("Usage:   $ ./launch_simple_chat_Client  <nickname>");
      return;
   end if;

   declare
      use chat.Client.local;

      client_Name : constant String                 := ada.command_Line.Argument (1);
      the_Client  : constant chat.Client.local.view := new_Client (client_Name);
   begin
      -- Setup
      --
      begin
         chat.Registrar.register (the_Client.all'Access);   -- Register our client with the registrar.
      exception
         when chat.Registrar.Name_already_used =>
            put_Line (client_Name & " is already in use.");
            return;
      end;

      lace.remote.event.Utility.use_text_Logger ("events");

      declare
         procedure broadcast (the_Text : in String)
         is
            the_Message : constant chat.client.Message := (client_Name'Length + 2 + the_Text'Length,
                                                           client_Name & ": " & the_Text);
         begin
            the_Client.emit (the_Message);
         end broadcast;

         use type chat.Client.view;
      begin
         declare
            Peers : constant chat.Client.views := chat.Registrar.all_Clients;
         begin
            for i in Peers'Range
            loop
               if the_Client.all'Access /= Peers (i)
               then
                  Peers (i) .register_Client (the_Client.all'Access);   -- Register our client with all other clients.
                  the_Client.register_Client (Peers (i));               -- Register all other clients with our client.
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

exception
   when E : others =>
      lace.remote.event.Utility.close;
      new_Line;
      put_Line ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
      put_Line ("Unhandled exception, aborting. Please report the following to developer.");
      put_Line ("________________________________________________________________________");
      put_Line (ada.Exceptions.exception_Information (E));
      put (ada.Characters.latin_1.ESC & "[1A");   -- Move cursor up.
      put_Line ("________________________________________________________________________");
      new_Line;
end launch_simple_chat_Client;
