with
     chat.Registrar,   -- This 'pulls in' the registrar.
     ada.Text_IO;

procedure launch_simple_chat_Registrar
--
-- Launches the chat registrar.
--
is
begin
   loop
      declare
         Command : constant String := ada.Text_IO.get_Line;
      begin
         exit when Command = "q";
      end;
   end loop;

   chat.Registrar.shutdown;

end launch_simple_chat_Registrar;
