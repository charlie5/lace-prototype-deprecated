with
     chat.Registrar,   -- This 'pulls in' the registrar singleton.
     ada.Exceptions,
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

   put_Line ("Shutting down.");
   chat.Registrar.shutdown;

exception
   when E : others =>
      new_Line;
      put_Line ("Error in launch_simple_chat_Registrar task.");
      new_Line;
      put_Line (ada.Exceptions.exception_Information (E));
end launch_simple_chat_Registrar;
