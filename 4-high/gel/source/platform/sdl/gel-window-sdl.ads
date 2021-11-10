private
with
     sdl.Video.Windows,
     sdl.Video.GL;

package gel.Window.sdl
--
-- Provides an SDL implementation of a window.
--
is
   type Item is new gel.Window.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure define  (Self : in     View;   Title  : in String;
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


   --------------
   --- Operations
   --

   overriding
   procedure emit_Events (Self : in out Item);
   overriding
   procedure enable_GL   (Self : in     Item);
   overriding
   procedure disable_GL  (Self : in     Item);
   overriding
   procedure swap_GL     (Self : in out Item);



private

   type Item is new gel.Window.item with
      record
         window_Handle : standard.sdl.Video.Windows.Window;
         GL_Context    : standard.sdl.Video.GL.Contexts;
      end record;

end gel.Window.sdl;
