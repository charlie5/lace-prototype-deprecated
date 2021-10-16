package body openGL.Tasks
is

   procedure check
   is
      use Ada,
          ada.Task_Identification;

      calling_Task : constant Task_Id := Task_Identification.current_Task;

      --  TODO: Use the assert instead of the exception for performance.
      --  pragma assert (Renderer_Task = calling_Task,
      --                   "Calling task '"      & Task_Identification.Image (current_Task)  & "'"
      --                 & " /= Renderer task '" & Task_Identification.Image (Renderer_Task) & "'");
   begin
      if Renderer_Task /= calling_Task
      then
         raise Error with   "Calling task '"      & Task_Identification.Image (current_Task)  & "'"
                          & " /= Renderer task '" & Task_Identification.Image (Renderer_Task) & "'";
      end if;
   end check;



   function check return Boolean
   is
   begin
      check;
      return True;
   end check;

end openGL.Tasks;
