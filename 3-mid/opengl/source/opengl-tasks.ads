with
     ada.Task_Identification;


package openGL.Tasks
--
--  Provides task info to allow checking that a GL operation is called only in the GL renderer engine task.
--
is

   current_Task : ada.Task_Identification.Task_Id := ada.Task_Identification.Null_Task_Id;
   --
   -- The current renderer task.


   function Check return Boolean;
   --
   -- Returns True if the calling task is the renderer task.
   -- Otherwise, gives an assertion error.

end openGL.Tasks;
