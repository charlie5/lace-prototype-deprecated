private
with SDL.Video.Windows,
     SDL.Video.GL;



package mmi.Window.sdl
--
--
--
is

   type Item is new mmi.Window.item with private;
   type View is access all Item'Class;




   --- construction
   --

   procedure define  (Self : in View;   Title  : in String;
                                            Width  : in Natural;
                                            Height : in Natural);
   overriding
   procedure destroy (Self : in out Item);


   package Forge
   is
      function new_Window (Title  : in String;
                           Width  : in Natural;
                           Height : in Natural) return Window.sdl.view;
   end Forge;




   --- attributes
   --




   --- operations
   --

   procedure emit_Events  (Self : in out Item);
   procedure enable_GL    (Self : in     Item);
   procedure disable_GL   (Self : in     Item);
   procedure swap_GL      (Self : in out Item);



private


   type Item is new mmi.Window.item with
      record
         window_Handle : standard.SDL.Video.Windows.Window;
         GL_Context    : standard.SDL.Video.GL.Contexts;
      end record;


end mmi.Window.sdl;
