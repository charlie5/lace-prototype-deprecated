with
     gel_demo_Server;


package body gel_demo_Services
is

   function World return gel.remote.World.view
   is
   begin
      return gel_demo_Server.the_server_World.all'access;
   end World;

end gel_demo_Services;
