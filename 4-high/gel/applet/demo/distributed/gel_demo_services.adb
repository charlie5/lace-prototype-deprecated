with
     mmi_demo_Server;


package body mmi_demo_Services
is

   function World return mmi.remote.World.view
   is
   begin
      return mmi_demo_Server.the_server_World.all'access;
   end;

end mmi_demo_Services;
