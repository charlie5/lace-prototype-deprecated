with
     lumen.Window,
     openGL.Renderer.lean,
     openGL.Camera,
     openGL.Dolly,
     openGL.frame_Counter;


package openGL.Demo
--
-- Provides a convenient method of setting up a simple openGL demo.
--
is
   Window       :         lumen.Window.Window_handle;
   Renderer     : aliased openGL.Renderer.lean.item;
   Camera       : aliased openGL.Camera.item;
   Dolly        :         openGL.Dolly.item (camera => Camera'unchecked_Access);
   FPS_Counter  :         openGL.frame_Counter.item;
   Done         :         Boolean := False;

   procedure define (Name : in String);
   procedure destroy;

end openGL.Demo;
