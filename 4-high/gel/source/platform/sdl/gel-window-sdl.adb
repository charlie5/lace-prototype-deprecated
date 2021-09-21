with
     SDL.Events.Keyboards,
     SDL.Video.Windows.Makers,
     SDL.Events.Events,
     SDL.Events.Mice,
     SDL.Log,

     ada.Characters.latin_1,
     ada.Text_IO;

package body gel.Window.sdl
is
   package std_SDL             renames standard.SDL;
   package sdl_Keyboard_Events renames std_SDL.Events.Keyboards;


   program_Exit : exception;


   overriding
   procedure swap_GL (Self : in out Item)
   is
      use std_SDL.Video.GL;
   begin
      swap (Self.window_Handle);
   end swap_GL;



   use std_SDL,
       std_SDL.Events;

   function to_gel_Key (From : in std_SDL.Events.Keyboards.Key_Codes) return gel.keyboard.Key;

   -------------
   --- Constants
   --

   -- Keystrokes we care about.

   Escape    : constant sdl_Keyboard_Events.Key_Codes := sdl_Keyboard_Events.Key_Codes (Character'Pos (Ada.Characters.Latin_1.ESC));
   Letter_q  : constant sdl_Keyboard_Events.Key_Codes := sdl_Keyboard_Events.Key_Codes (Character'Pos (Ada.Characters.Latin_1.LC_Q));


   -----------
   --- Globals
   --

   the_Window : gel.Window.sdl.view;


   ---------
   --- Forge
   --

   procedure define  (Self : in View;   Title  : in String;
                                        Width  : in Natural;
                                        Height : in Natural)
   is
      use std_SDL;
      use type Video.Windows.Window_Flags;
   begin
      if not std_SDL.initialise
      then
         raise gel.Error with "Unable to initialise SDL.";
      end if;

      Video.Windows.Makers.create (Win    => Self.window_Handle,
                                   Title  => Title,
                                   X      => 100,
                                   Y      => 100,
                                   Width  => C.int (Width),
                                   Height => C.int (Height),
                                   Flags  =>    Video.Windows.openGL
                                             or Video.Windows.Resizable);

      Video.GL.create (Self.GL_Context, From => Self.window_Handle);

      if the_Window = null
      then
         the_Window := Self;
      else
         raise program_Error with "Attempt to define SDL window twice.";
      end if;
   end define;



   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      Self.window_Handle.finalize;
      std_SDL.finalise;

      destroy (gel.Window.item (Self));     -- Destroy base class.
   end destroy;



   package body Forge
   is
      function to_Window (Title  : in String;
                          Width  : in Natural;
                          Height : in Natural) return gel.Window.sdl.item
      is
      begin
         return Self : gel.Window.sdl.item := (gel.Window.private_Forge.to_Window (Title, Width, Height)
                                               with others => <>)
         do
            define (Self'unchecked_Access, Title, Width, Height);
         end return;
      end to_Window;


      function new_Window (Title  : in String;
                           Width  : in Natural;
                           Height : in Natural) return Window.sdl.view
      is
         Self : constant gel.Window.sdl.view := new Window.sdl.item' (to_Window (Title, Width, Height));
      begin
         return Self;
      end new_Window;
   end Forge;


   --------------
   --- Operations
   --

   use gel.Keyboard;


   -- Callback triggered by the Resized event; tell OpenGL that the
   -- user has resized the window.
   --
   procedure resize_Handler (Height : in Integer;
                             Width  : in Integer)
   is
   begin
      the_Window.Size_is (Width, Height);
   end Resize_Handler;



   overriding
   procedure emit_Events (Self : in out Item)
   is
      use type std_SDL.Events.Keyboards.Key_Codes;

      Event : aliased std_SDL.Events.Events.Events;

   begin
      while std_SDL.Events.Events.Poll (Event)
      loop
         case Event.Common.Event_Type
         is
            when std_SDL.Events.Quit =>
               Self.is_Open := False;

            when std_SDL.Events.Keyboards.Key_Down =>
               the_Window.Keyboard.emit_key_press_Event (to_gel_Key (Event.keyboard.key_Sym.key_Code),
                                                         Integer    (Event.keyboard.key_Sym.key_Code));

            when std_SDL.Events.Keyboards.Key_Up =>
               std_SDL.Log.put_Debug ("Key up event: " & Event.keyboard.key_Sym. key_Code'Image &
                                      "   Scan code: " & Event.keyboard.key_Sym.scan_Code'Image);

               if Event.keyboard.key_Sym.key_Code = std_SDL.Events.Keyboards.Code_escape
               then
                  Self.is_Open := False;
               end if;

               the_Window.Keyboard.emit_key_release_Event (to_gel_Key (Event.keyboard.key_Sym.key_Code));

            when std_SDL.Events.Mice.Button_Down =>
               the_Window.Mouse.emit_button_press_Event (gel.mouse.button_Id (std_SDL.Events.Mice.Buttons'Pos (Event.mouse_Button.Button) + 1),
                                                         the_Window.Keyboard.Modifiers,
                                                         (Integer (Event.mouse_Button.X),
                                                          Integer (Event.mouse_Button.Y)));

            when std_SDL.Events.Mice.Button_Up =>
               the_Window.Mouse.emit_button_release_Event (gel.mouse.button_Id (std_SDL.Events.Mice.Buttons'Pos (Event.Mouse_Button.Button) + 1),
                                                           the_Window.Keyboard.Modifiers,
                                                           (Integer (Event.mouse_Button.X),
                                                            Integer (Event.mouse_Button.Y)));

            when std_SDL.Events.Mice.Motion =>
               the_Window.Mouse.emit_motion_Event (site => (Integer (Event.Mouse_Motion.x),
                                                            Integer (Event.Mouse_Motion.y)));


            when std_SDL.Events.Mice.Wheel =>
               null;

--              when SDL_VIDEORESIZE =>
--                 self.Size_is (Integer (event.resize.w),
--                               Integer (event.resize.h));

            when others =>
               null;
         end case;
      end loop;

--        SDL_GL_SwapBuffers;
   end emit_Events;


   overriding
   procedure enable_GL (Self : in Item)
   is
   begin
      std_SDL.Video.gl.set_Current (Self.GL_Context, To => Self.window_Handle);
   end enable_GL;


   overriding
   procedure disable_GL (Self : in Item)
   is
      null_Context : standard.SDL.Video.GL.Contexts;
   begin
      std_SDL.Video.gl.set_Current (null_Context, to => Self.window_Handle);
   end disable_GL;



   function to_gel_Key (From : in std_SDL.Events.Keyboards.key_Codes) return gel.keyboard.Key
   is
      use type std_SDL.events.keyboards.key_Codes;
   begin
      case From
      is
         when std_SDL.Events.Keyboards.Code_return
                        => return gel.Keyboard.Enter;
         when 27        => return gel.Keyboard.Escape;
         when 32        => return gel.Keyboard.Space;
         when 43        => return gel.Keyboard.KP_PLUS;

         when 48 ..  59 => return gel.keyboard.Key'Val (From - 48 + gel.keyboard.Key'Pos (KP0));
         when 65 ..  90 => return gel.keyboard.Key'Val (From - 65 + gel.keyboard.Key'Pos (a));
         when 97 .. 122 => return gel.keyboard.Key'Val (From - 97 + gel.keyboard.Key'Pos (a));

         when 65505   => return lShift;
         when 65506   => return rShift;

         when 65507   => return lCtrl;
         when 65508   => return rCtrl;

         when 65513   => return lAlt;
         when 65514   => return rAlt;

         when 65362   => return Up;
         when 65363   => return Right;
         when 65364   => return Down;

         when std_SDL.Events.Keyboards.Code_left
                      => return gel.Keyboard.LEFT;
         when 65365   => return PAGEUP;
         when 65366   => return PAGEDOWN;

         when others  =>
            if From = std_SDL.Events.Keyboards.Code_left
            then
               return Left;
            else
               ada.Text_IO.put_Line ("SDL window unhandled key: " & From'Image);     -- TODO: remaning key codes.
               -- raise Error with "Unhandled key: " & From'Image;
            end if;
      end case;

      return gel.Keyboard.Key'First;
   end to_gel_Key;


   -------------------
   --- Window Creator
   --

   function window_Creator (Name : in String;
                            Width,
                            Height : in Positive) return gel.Window.view
   is
   begin
      return gel.Window.view (Forge.new_Window (Name, Width, Height));
   end window_Creator;


begin
   gel.Window.use_create_Window (window_Creator'Access);
end gel.Window.sdl;
