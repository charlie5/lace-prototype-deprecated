with 
     mmi_demo_Server,
     mmi_demo_Client;


procedure launch_MMI_fused
--
-- Launches the fused version.
--
is
begin
   mmi_demo_Server.item.start;
   mmi_demo_Client.item.start;
end;
