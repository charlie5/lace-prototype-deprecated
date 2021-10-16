with
     gel_demo_Server,
     gel_demo_Client;


procedure launch_GEL_fused
--
-- Launches the fused version.
--
is
begin
   gel_demo_Server.item.start;
   gel_demo_Client.item.start;
end launch_GEL_fused;
