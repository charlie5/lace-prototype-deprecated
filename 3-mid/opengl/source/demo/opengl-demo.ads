with
     openGL.Model,
     openGL.Visual,
     openGL.Renderer.lean,
     openGL.Camera,
     openGL.Dolly,
     openGL.frame_Counter,

     sdl.Video.Windows,
     sdl.Video.GL;


package openGL.Demo
--
-- Provides a convenient method of setting up a simple openGL demo.
--
is
   Window     : standard.sdl.Video.Windows.Window;
   GL_Context : standard.sdl.Video.GL.Contexts;

   Renderer    : aliased openGL.Renderer.lean.item;
   Camera      : aliased openGL.Camera.item;
   Dolly       :         openGL.Dolly.item (camera => Camera'unchecked_Access);
   FPS_Counter :         openGL.frame_Counter.item;
   Done        :         Boolean := False;

   function Models return openGL.Model.views;
   --
   -- Creates a set of models with one model of each kind.

   procedure layout (the_Visuals : in openGL.Visual.views);
   --
   -- Layout the visuals in a grid fashion for viewing all at once.

   procedure print_Usage (append_Message : in String := "");


   procedure define (Name   : in String;
                     Width  : in Positive := 1366;
                     Height : in Positive :=  768);
   procedure destroy;

end openGL.Demo;
