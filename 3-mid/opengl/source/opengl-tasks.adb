
package body openGL.Tasks
is

   function Check return Boolean
   is
      use Ada, ada.task_Identification;

      pragma assert (openGL.Tasks.current_Task = task_Identification.Current_Task,
                       "Current task '"         & task_Identification.Image (ada.task_Identification.Current_Task) & "'"
                     & "  /=  Renderer task '"  & task_Identification.Image (openGL.Tasks.current_Task)            & "'");
   begin
      return True;
   end Check;

end openGL.Tasks;
