with
     mmi.remote.World;


package mmi_demo_Services
--
-- Provides RCI services.
--
is
   pragma remote_call_Interface;

   function World return mmi.remote.World.view;

end mmi_demo_Services;
