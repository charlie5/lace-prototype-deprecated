with
     chat.Registrar,     -- This 'pulls in' the registrar.
     ada.text_IO;



procedure launch_simple_chat_Registrar
--
-- Launches the chat Registrar for 60 seconds.
--
is
begin

   for i in 1 .. 60
   loop
      delay 1.0;
   end loop;

end launch_simple_chat_Registrar;
