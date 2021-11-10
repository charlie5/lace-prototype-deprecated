with
     SDL.Events.Windows,
     SDL.Events.Keyboards,
     SDL.Events.Events,
     SDL.Events.Mice,
     SDL.Video.Windows.Makers,
     SDL.Log,

     ada.Text_IO;


package body gel.Window.sdl
is
   package std_SDL renames standard.SDL;

   use std_SDL,
       std_SDL.Events;

   function to_gel_Key (From : in std_SDL.Events.Keyboards.Key_Codes) return gel.keyboard.Key;



   ---------
   --- Forge
   --

   procedure define  (Self : in View;   Title  : in String;
                                        Width  : in Natural;
                                        Height : in Natural)
   is
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
               Self.Keyboard.emit_key_press_Event (Key      => to_gel_Key (Event.keyboard.key_Sym.key_Code),
                                                   key_Code => Integer    (Event.keyboard.key_Sym.key_Code));

            when std_SDL.Events.Keyboards.Key_Up =>
               std_SDL.Log.put_Debug ("Key up event: " & Event.keyboard.key_Sym. key_Code'Image &
                                      "   Scan code: " & Event.keyboard.key_Sym.scan_Code'Image);

               if Event.keyboard.key_Sym.key_Code = std_SDL.Events.Keyboards.Code_escape     -- TODO: Make this user-configurable.
               then
                  Self.is_Open := False;
               end if;

               Self.Keyboard.emit_key_release_Event (Key => to_gel_Key (Event.keyboard.key_Sym.key_Code));

            when std_SDL.Events.Mice.Button_Down =>
               Self.Mouse.emit_button_press_Event (Button    => gel.mouse.button_Id (std_SDL.Events.Mice.Buttons'Pos (Event.mouse_Button.Button) + 1),
                                                   Modifiers => Self.Keyboard.Modifiers,
                                                   Site      => (Integer (Event.mouse_Button.X),
                                                                 Integer (Event.mouse_Button.Y)));

            when std_SDL.Events.Mice.Button_Up =>
               Self.Mouse.emit_button_release_Event (Button    => gel.mouse.button_Id (std_SDL.Events.Mice.Buttons'Pos (Event.Mouse_Button.Button) + 1),
                                                     Modifiers => Self.Keyboard.Modifiers,
                                                     Site      => (Integer (Event.mouse_Button.X),
                                                                   Integer (Event.mouse_Button.Y)));

            when std_SDL.Events.Mice.Motion =>
               Self.Mouse.emit_motion_Event (Site => (Integer (Event.Mouse_Motion.x),
                                                      Integer (Event.Mouse_Motion.y)));


            when std_SDL.Events.Mice.Wheel =>     -- TODO
               null;


            when std_SDL.Events.Windows.Window =>
               declare
                  use std_SDL.Events.Windows;
               begin
                  if Event.Window.Event_ID = Windows.Resized
                  then
                     Self.Size_is (Integer (Event.Window.Data_1),
                                   Integer (Event.Window.Data_2));
                  end if;
               end;

            when others =>     -- TODO
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
      std_SDL.Video.gl.set_Current (null_Context, To => Self.window_Handle);
   end disable_GL;



   overriding
   procedure swap_GL (Self : in out Item)
   is
      use std_SDL.Video.GL;
   begin
      swap (Self.window_Handle);
   end swap_GL;



   function to_gel_Key (From : in std_SDL.Events.Keyboards.key_Codes) return gel.keyboard.Key
   is
      package Key renames std_SDL.Events.keyboards;
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

         when Key.Code_left_bracket  => return gel.Keyboard.leftBracket;
         when Key.Code_back_slash    => return gel.Keyboard.backSlash;
         when Key.Code_right_bracket => return gel.Keyboard.rightBracket;
         when Key.Code_caret         => return gel.Keyboard.Caret;
         when Key.Code_underscore    => return gel.Keyboard.Underscore;
         when Key.Code_back_quote    => return gel.Keyboard.backQuote;

         when Key.Code_a => return gel.Keyboard.A;
         when Key.Code_b => return gel.Keyboard.B;
         when Key.Code_c => return gel.Keyboard.C;
         when Key.Code_d => return gel.Keyboard.D;
         when Key.Code_e => return gel.Keyboard.E;
         when Key.Code_f => return gel.Keyboard.F;
         when Key.Code_g => return gel.Keyboard.G;
         when Key.Code_h => return gel.Keyboard.H;
         when Key.Code_i => return gel.Keyboard.I;
         when Key.Code_j => return gel.Keyboard.J;
         when Key.Code_k => return gel.Keyboard.K;
         when Key.Code_l => return gel.Keyboard.L;
         when Key.Code_m => return gel.Keyboard.M;
         when Key.Code_n => return gel.Keyboard.N;
         when Key.Code_o => return gel.Keyboard.O;
         when Key.Code_p => return gel.Keyboard.P;
         when Key.Code_q => return gel.Keyboard.Q;
         when Key.Code_r => return gel.Keyboard.R;
         when Key.Code_s => return gel.Keyboard.S;
         when Key.Code_t => return gel.Keyboard.T;
         when Key.Code_u => return gel.Keyboard.U;
         when Key.Code_v => return gel.Keyboard.V;
         when Key.Code_w => return gel.Keyboard.W;
         when Key.Code_x => return gel.Keyboard.X;
         when Key.Code_y => return gel.Keyboard.Y;
         when Key.Code_z => return gel.Keyboard.Z;

         when Key.Code_caps_lock => return gel.Keyboard.CapsLock;

         when Key.Code_F1  => return gel.Keyboard.F1;
         when Key.Code_F2  => return gel.Keyboard.F2;
         when Key.Code_F3  => return gel.Keyboard.F3;
         when Key.Code_F4  => return gel.Keyboard.F4;
         when Key.Code_F5  => return gel.Keyboard.F5;
         when Key.Code_F6  => return gel.Keyboard.F6;
         when Key.Code_F7  => return gel.Keyboard.F7;
         when Key.Code_F8  => return gel.Keyboard.F8;
         when Key.Code_F9  => return gel.Keyboard.F9;
         when Key.Code_F10 => return gel.Keyboard.F10;
         when Key.Code_F11 => return gel.Keyboard.F11;
         when Key.Code_F12 => return gel.Keyboard.F12;

         when Key.Code_print_screen => return gel.Keyboard.Print;
         when Key.Code_scroll_lock  => return gel.Keyboard.ScrollLock;
         when Key.Code_pause        => return gel.Keyboard.Pause;
         when Key.Code_insert       => return gel.Keyboard.Insert;
         when Key.Code_home         => return gel.Keyboard.Home;
         when Key.Code_page_up      => return gel.Keyboard.PageUp;
         when Key.Code_delete       => return gel.Keyboard.Delete;
         when Key.Code_end          => return gel.Keyboard.End_key;
         when Key.Code_page_down    => return gel.Keyboard.PageDown;
         when Key.Code_right        => return gel.Keyboard.Right;
         when Key.Code_left         => return gel.Keyboard.Left;
         when Key.Code_down         => return gel.Keyboard.Down;
         when Key.Code_up           => return gel.Keyboard.Up;

         when Key.Code_num_lock_clear => return gel.Keyboard.NumLock;

         when Key.Code_KP_divide   => return gel.Keyboard.KP_Divide;
         when Key.Code_KP_multiply => return gel.Keyboard.KP_Multiply;
         when Key.Code_KP_minus    => return gel.Keyboard.KP_Minus;
         when Key.Code_KP_plus     => return gel.Keyboard.KP_Plus;
         when Key.Code_KP_enter    => return gel.Keyboard.KP_Enter;
         when Key.Code_KP_1        => return gel.Keyboard.KP1;
         when Key.Code_KP_2        => return gel.Keyboard.KP2;
         when Key.Code_KP_3        => return gel.Keyboard.KP3;
         when Key.Code_KP_4        => return gel.Keyboard.KP4;
         when Key.Code_KP_5        => return gel.Keyboard.KP5;
         when Key.Code_KP_6        => return gel.Keyboard.KP6;
         when Key.Code_KP_7        => return gel.Keyboard.KP7;
         when Key.Code_KP_8        => return gel.Keyboard.KP8;
         when Key.Code_KP_9        => return gel.Keyboard.KP9;
         when Key.Code_KP_0        => return gel.Keyboard.KP0;
         when Key.Code_KP_period   => return gel.Keyboard.KP_Period;

         --  when Key.Code_application     => return gel.Keyboard.;
         when Key.Code_power           => return gel.Keyboard.Power;
         when Key.Code_KP_equals       => return gel.Keyboard.KP_Equals;
         when Key.Code_F13             => return gel.Keyboard.F13;
         when Key.Code_F14             => return gel.Keyboard.F14;
         when Key.Code_F15             => return gel.Keyboard.F15;
         --  when Key.Code_F16             => return gel.Keyboard.;
         --  when Key.Code_F17             => return gel.Keyboard.;
         --  when Key.Code_F18             => return gel.Keyboard.;
         --  when Key.Code_F19             => return gel.Keyboard.;
         --  when Key.Code_F20             => return gel.Keyboard.;
         --  when Key.Code_F21             => return gel.Keyboard.;
         --  when Key.Code_F22             => return gel.Keyboard.;
         --  when Key.Code_F23             => return gel.Keyboard.;
         --  when Key.Code_F24             => return gel.Keyboard.;
         --  when Key.Code_execute         => return gel.Keyboard.;
         when Key.Code_help            => return gel.Keyboard.Help;
         when Key.Code_menu            => return gel.Keyboard.Menu;
         --  when Key.Code_select          => return gel.Keyboard.;
         --  when Key.Code_stop            => return gel.Keyboard.;
         --  when Key.Code_again           => return gel.Keyboard.;
         when Key.Code_undo            => return gel.Keyboard.Undo;
         --  when Key.Code_cut             => return gel.Keyboard.;
         --  when Key.Code_copy            => return gel.Keyboard.;
         --  when Key.Code_paste           => return gel.Keyboard.;
         --  when Key.Code_find            => return gel.Keyboard.;
         --  when Key.Code_mute            => return gel.Keyboard.;
         --  when Key.Code_volume_up       => return gel.Keyboard.;
         --  when Key.Code_volume_down     => return gel.Keyboard.;
         --  when Key.Code_KP_comma        => return gel.Keyboard.;
         --  when Key.Code_KP_equals_AS400 => return gel.Keyboard.;

         --  when Key.Code_alt_erase   => return gel.Keyboard.;
         when Key.Code_sys_req     => return gel.Keyboard.SysReq;
         --  when Key.Code_cancel      => return gel.Keyboard.;
         when Key.Code_clear       => return gel.Keyboard.Clear;
         --  when Key.Code_prior       => return gel.Keyboard.;
         --  when Key.Code_return_2    => return gel.Keyboard.;
         --  when Key.Code_separator   => return gel.Keyboard.;
         --  when Key.Code_out         => return gel.Keyboard.;
         --  when Key.Code_oper        => return gel.Keyboard.;
         --  when Key.Code_clear_again => return gel.Keyboard.;
         --  when Key.Code_CR_sel      => return gel.Keyboard.;
         --  when Key.Code_Ex_sel      => return gel.Keyboard.;

         --  when Key.Code_KP_00                  => return gel.Keyboard.;
         --  when Key.Code_KP_000                 => return gel.Keyboard.;
         --  when Key.Code_thousands_separator    => return gel.Keyboard.;
         --  when Key.Code_decimal_separator      => return gel.Keyboard.;
         --  when Key.Code_currency_unit          => return gel.Keyboard.;
         --  when Key.Code_KP_left_parenthesis    => return gel.Keyboard.;
         --  when Key.Code_KP_right_parentheesis  => return gel.Keyboard.;
         --  when Key.Code_KP_left_brace          => return gel.Keyboard.;
         --  when Key.Code_KP_right_brace         => return gel.Keyboard.;
         --  when Key.Code_KP_tab                 => return gel.Keyboard.;
         --  when Key.Code_KP_backspace           => return gel.Keyboard.;
         --  when Key.Code_KP_A                   => return gel.Keyboard.;
         --  when Key.Code_KP_B                   => return gel.Keyboard.;
         --  when Key.Code_KP_C                   => return gel.Keyboard.;
         --  when Key.Code_KP_D                   => return gel.Keyboard.;
         --  when Key.Code_KP_E                   => return gel.Keyboard.;
         --  when Key.Code_KP_F                   => return gel.Keyboard.;
         --  when Key.Code_KP_xor                 => return gel.Keyboard.;
         --  when Key.Code_KP_power               => return gel.Keyboard.;
         --  when Key.Code_KP_percent             => return gel.Keyboard.;
         --  when Key.Code_KP_less                => return gel.Keyboard.;
         --  when Key.Code_KP_greater             => return gel.Keyboard.;
         --  when Key.Code_KP_ampersand           => return gel.Keyboard.;
         --  when Key.Code_KP_double_ampersand    => return gel.Keyboard.;
         --  when Key.Code_KP_vertical_bar        => return gel.Keyboard.;
         --  when Key.Code_KP_double_vertical_bar => return gel.Keyboard.;
         --  when Key.Code_KP_colon               => return gel.Keyboard.;
         --  when Key.Code_KP_hash                => return gel.Keyboard.;
         --  when Key.Code_KP_space               => return gel.Keyboard.;
         --  when Key.Code_KP_at                  => return gel.Keyboard.;
         --  when Key.Code_KP_exclamation         => return gel.Keyboard.;
         --  when Key.Code_KP_memory_store        => return gel.Keyboard.;
         --  when Key.Code_KP_memory_recall       => return gel.Keyboard.;
         --  when Key.Code_KP_memory_clear        => return gel.Keyboard.;
         --  when Key.Code_KP_memory_add          => return gel.Keyboard.;
         --  when Key.Code_KP_memory_subtract     => return gel.Keyboard.;
         --  when Key.Code_KP_memory_multiply     => return gel.Keyboard.;
         --  when Key.Code_KP_memory_divide       => return gel.Keyboard.;
         --  when Key.Code_KP_plus_minus          => return gel.Keyboard.;
         --  when Key.Code_KP_clear               => return gel.Keyboard.;
         --  when Key.Code_KP_clear_entry         => return gel.Keyboard.;
         --  when Key.Code_KP_binary              => return gel.Keyboard.;
         --  when Key.Code_KP_octal               => return gel.Keyboard.;
         --  when Key.Code_KP_decimal             => return gel.Keyboard.;
         --  when Key.Code_KP_hexadecimal         => return gel.Keyboard.;

         when Key.Code_left_control  => return gel.Keyboard.lCtrl;
         when Key.Code_left_shift    => return gel.Keyboard.lShift;
         when Key.Code_left_alt      => return gel.Keyboard.lAlt;
         --  when Key.Code_left_gui      => return gel.Keyboard.;
         when Key.Code_right_control => return gel.Keyboard.rCtrl;
         when Key.Code_right_shift   => return gel.Keyboard.rShift;
         when Key.Code_right_alt     => return gel.Keyboard.rAlt;
         --  when Key.Code_right_gui     => return gel.Keyboard.;
         --  when Key.Code_mode          => return gel.Keyboard.;

         --  when Key.Code_audio_next     => return gel.Keyboard.;
         --  when Key.Code_audio_previous => return gel.Keyboard.;
         --  when Key.Code_audio_stop     => return gel.Keyboard.;
         --  when Key.Code_audio_play     => return gel.Keyboard.;
         --  when Key.Code_audio_mute     => return gel.Keyboard.;
         --  when Key.Code_media_select   => return gel.Keyboard.;
         --  when Key.Code_www            => return gel.Keyboard.;
         --  when Key.Code_mail           => return gel.Keyboard.;
         --  when Key.Code_calculator     => return gel.Keyboard.;
         --  when Key.Code_computer       => return gel.Keyboard.;
         --  when Key.Code_AC_search      => return gel.Keyboard.;
         --  when Key.Code_AC_home        => return gel.Keyboard.;
         --  when Key.Code_AC_back        => return gel.Keyboard.;
         --  when Key.Code_AC_forward     => return gel.Keyboard.;
         --  when Key.Code_AC_stop        => return gel.Keyboard.;
         --  when Key.Code_AC_refresh     => return gel.Keyboard.;
         --  when Key.Code_AC_bookmarks   => return gel.Keyboard.;

         --  when Key.Code_brightness_down     => return gel.Keyboard.;
         --  when Key.Code_brightness_up       => return gel.Keyboard.;
         --  when Key.Code_display_switch      => return gel.Keyboard.;
         --  when Key.Code_illumination_toggle => return gel.Keyboard.;
         --  when Key.Code_illumination_down   => return gel.Keyboard.;
         --  when Key.Code_illumination_up     => return gel.Keyboard.;
         --  when Key.Code_eject               => return gel.Keyboard.;
         --  when Key.Code_sleep               => return gel.Keyboard.;

         when others =>
            ada.Text_IO.put_Line ("SDL window unhandled key: " & From'Image);     -- TODO: Remaining key codes.
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
