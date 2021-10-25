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
      package Key renames std_SDL.Events.keyboards;
      use type Key.key_Codes;
   begin
      case From
      is
         when Key.Code_return            => return gel.Keyboard.Enter;
         when Key.Code_escape            => return gel.Keyboard.Escape;
         when Key.Code_backspace         => return gel.Keyboard.BackSpace;
         when Key.Code_tab               => return gel.Keyboard.Tab;
         when Key.Code_space             => return gel.Keyboard.Space;
         when Key.Code_exclamation       => return gel.Keyboard.Exclaim;
         when Key.Code_double_quote      => return gel.Keyboard.QuoteDbl;
         when Key.Code_hash              => return gel.Keyboard.Hash;
         when Key.Code_percent           => return gel.Keyboard.Percent;
         when Key.Code_dollar            => return gel.Keyboard.Dollar;
         when Key.Code_ampersand         => return gel.Keyboard.Ampersand;
         when Key.Code_quote             => return gel.Keyboard.Quote;
         when Key.Code_left_parenthesis  => return gel.Keyboard.leftParen;
         when Key.Code_right_parenthesis => return gel.Keyboard.rightParen;
         when Key.Code_asterisk          => return gel.Keyboard.Asterisk;
         when Key.Code_plus              => return gel.Keyboard.Plus;
         when Key.Code_comma             => return gel.Keyboard.Comma;
         when Key.Code_minus             => return gel.Keyboard.Minus;
         when Key.Code_period            => return gel.Keyboard.Period;
         when Key.Code_slash             => return gel.Keyboard.Slash;

         when Key.Code_0 => return gel.Keyboard.'0';
         when Key.Code_1 => return gel.Keyboard.'1';
         when Key.Code_2 => return gel.Keyboard.'2';
         when Key.Code_3 => return gel.Keyboard.'3';
         when Key.Code_4 => return gel.Keyboard.'4';
         when Key.Code_5 => return gel.Keyboard.'5';
         when Key.Code_6 => return gel.Keyboard.'6';
         when Key.Code_7 => return gel.Keyboard.'7';
         when Key.Code_8 => return gel.Keyboard.'8';
         when Key.Code_9 => return gel.Keyboard.'9';

         when Key.Code_colon      => return gel.Keyboard.Colon;
         when Key.Code_semi_colon => return gel.Keyboard.semiColon;
         when Key.Code_less       => return gel.Keyboard.Less;
         when Key.Code_equals     => return gel.Keyboard.Equals;
         when Key.Code_greater    => return gel.Keyboard.Greater;
         when Key.Code_question   => return gel.Keyboard.Question;
         when Key.Code_at         => return gel.Keyboard.At_key;

         when Key.Code_kp_plus       => return gel.Keyboard.KP_PLUS;

         when Key.Code_left_shift    => return lShift;
         when Key.Code_right_shift   => return rShift;

         when Key.Code_left_control  => return lCtrl;
         when Key.Code_right_control => return rCtrl;

         when Key.Code_left_alt      => return lAlt;
         when Key.Code_right_alt     => return rAlt;

         when Key.Code_up            => return Up;
         when Key.Code_down          => return Down;

         when Key.Code_left          => return Left;
         when Key.Code_right         => return Right;
         when Key.Code_page_up       => return PageUp;
         when Key.Code_page_down     => return PageDown;

         when others =>
            ada.Text_IO.put_Line ("SDL window unhandled key: " & From'Image);     -- TODO: remaning key codes.
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
