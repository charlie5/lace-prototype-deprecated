with
     chat.Registrar,   -- This 'pulls in' the registrar singleton.
     ada.Text_IO;

procedure launch_simple_chat_Registrar
--
-- Launches the chat registrar.
--
is
   use ada.Text_IO;
begin
   loop
      declare
         Command : constant String := get_Line;
      begin
         exit when Command = "q";
      end;
   end loop;

   chat.Registrar.shutdown;
end launch_simple_chat_Registrar;
