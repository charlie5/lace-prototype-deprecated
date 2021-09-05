with
     ada.Task_Identification;

package openGL.Tasks
--
--  Allow checking that a GL operation is called only in the GL renderer engine task.
--
is

   Renderer_Task : ada.Task_Identification.Task_Id := ada.Task_Identification.null_Task_Id;
   --
   -- The current renderer task.


   procedure check;
   function  check return Boolean;
   --
   -- Check if the calling task is the renderer task.
   -- Otherwise, gives an assertion error.


end openGL.Tasks;
