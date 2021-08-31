with
     openGL.Server,
     Lumen.Window,
     ada.Text_IO;


procedure launch_core_Test
--
--  Exercise basic subprograms common to all GL profiles.
--
--  TODO: Complete this.
--
is
   use ada.Text_IO;
   Win : Lumen.Window.Window_Handle;

begin
   -- Create Lumen window to provide a current GL context.
   --
   lumen.Window.create (Win,
                        Name     => "GL Core Test",
                        Animated => False);

   put_Line ("openGL Server: " & openGL.Server.Version);
   delay 5.0;
end launch_core_Test;
