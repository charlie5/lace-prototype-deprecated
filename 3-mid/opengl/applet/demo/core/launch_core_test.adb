with
     openGL.Tasks,
     openGL.Server,

     sdl.Video.Windows.Makers,
     sdl.Video.gl,

     ada.Task_identification,
     ada.Text_IO;


procedure launch_core_Test
--
--  Exercise basic subprograms common to all GL profiles.
--
--  TODO: Complete this.
--
is
   use ada.Text_IO;
   use type sdl.Video.Windows.window_Flags;

   Error : exception;

   Window     : sdl.Video.Windows.Window;
   gl_Context : sdl.Video.gl.Contexts;

begin
   ---------
   --- Setup
   --

   if not SDL.initialise
   then
      raise Error with "Unable to initialise SDL.";
   end if;

   sdl.Video.Windows.Makers.create (Win    => Window,
                                    Title  => "openGL Demo",
                                    X      => 100,
                                    Y      => 100,
                                    Width  => 200,
                                    Height => 200,
                                    Flags  =>    sdl.Video.Windows.openGL
                                              or sdl.Video.Windows.Resizable);

   sdl.Video.gl.create      (gl_Context, From => Window);
   sdl.Video.gl.set_Current (gl_Context, To   => Window);

   openGL.Tasks.renderer_Task := ada.Task_identification.current_Task;

   ---------
   --- Tests
   --

   put_Line ("openGL Server: " & openGL.Server.Version);
   delay 2.0;
end launch_core_Test;
