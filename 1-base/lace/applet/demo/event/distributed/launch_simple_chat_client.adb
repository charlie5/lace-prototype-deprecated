with
     chat.Client.local,

     lace.Event.utility,

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

      client_Name : constant String        := ada.command_Line.Argument (1);
      the_Client  : chat.Client.local.item := to_Client (client_Name);
   begin
      the_Client.start;
   end;

exception
   when E : others =>
      lace.Event.utility.close;

      new_Line;
      put_Line ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
      put_Line ("Unhandled exception, aborting. Please report the following to developer.");
      put_Line ("________________________________________________________________________");
      put_Line (ada.Exceptions.exception_Information (E));
      put (ada.Characters.latin_1.ESC & "[1A");   -- Move cursor up.
      put_Line ("________________________________________________________________________");
      new_Line;
end launch_simple_chat_Client;
