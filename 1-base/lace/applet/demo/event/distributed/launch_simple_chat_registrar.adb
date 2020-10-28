with
     chat.Registrar;   -- This 'pulls in' the registrar.
pragma Unreferenced (chat.Registrar);

procedure launch_simple_chat_Registrar
--
-- Launches the chat registrar for 10 minutes.
--
is
begin
   for i in 1 .. 600
   loop
      delay 1.0;
   end loop;
end launch_simple_chat_Registrar;
