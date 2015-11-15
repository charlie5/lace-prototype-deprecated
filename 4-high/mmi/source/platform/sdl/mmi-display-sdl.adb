
with mmi.Screen;
with SDL_c.binding;
with SDL_c.SDL_Event;

with interfaces.C;

with ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with sdl_c.SDL_keysym;

with interfaces.c;


package body mmi.Display.sdl
--
--
--
is
   use SDL_c, SDL_c.binding;
   use Interfaces, Interfaces.C;




   -- define/destroy
   --

   procedure define  (Self : in out Item)
   is
      status    : C.int;
      SDL_Error : exception;
   begin
      status := SDL_Init (SDL_INIT_VIDEO);
      if status = -1 then   raise SDL_Error with "unable to initialise SDL";   end if;


      self.screen_Count := 1;
      self.Screens (1).define;
   end;




   procedure destroy (Self : in out Item)
   is
   begin
      SDL_quit;
   end;




   package body Forge
   is
      function new_Display return Display.view
      is
         Self : Display.sdl.view := new Display.sdl.item;
      begin
         Self.define;
         return Self.all'access;
      end;
   end Forge;






   -- attributes
   --

   function Screens (Self : access Item) return Screen.views
   is
      the_Screens : Screen.views (1 .. self.screen_Count);
   begin
      for Each in the_Screens'range loop
         the_Screens (Each) := self.Screens (Each)'unchecked_access;
      end loop;

      return the_Screens;
   end;




   function screen_Count (Self : access Item) return Natural
   is
   begin
      return self.screen_Count;
   end;









   -- operations
   --
   use SDL_c;
   use mmi.Keyboard;

   to_lumen_Key : array (sdl_c.SDLKey) of mmi.keyboard.Key
     := (SDLK_FIRST => FIRST ,
         SDLK_BACKSPACE => BACKSPACE,
         SDLK_TAB => TAB,
         SDLK_CLEAR => CLEAR,
         SDLK_RETURN => ENTER,
         SDLK_PAUSE => PAUSE,
         SDLK_ESCAPE => ESCAPE,
         SDLK_SPACE => SPACE,
         SDLK_EXCLAIM => EXCLAIM,
         SDLK_QUOTEDBL => QUOTEDBL,
         SDLK_HASH => HASH,
         SDLK_DOLLAR => DOLLAR,
         SDLK_AMPERSAND => AMPERSAND,
         SDLK_QUOTE => QUOTE,
         SDLK_LEFTPAREN => LEFTPAREN,
         SDLK_RIGHTPAREN => RIGHTPAREN,
         SDLK_ASTERISK => ASTERISK,
         SDLK_PLUS => PLUS,
         SDLK_COMMA => COMMA,
         SDLK_MINUS => MINUS,
         SDLK_PERIOD => PERIOD,
         SDLK_SLASH => SLASH,
         SDLK_0 => '0',
         SDLK_1 => '1',
         SDLK_2 => '2',
         SDLK_3 => '3',
         SDLK_4 => '4',
         SDLK_5 => '5',
         SDLK_6 => '6',
         SDLK_7 => '7',
         SDLK_8 => '8',
         SDLK_9 => '9',
         SDLK_COLON => COLON,
         SDLK_SEMICOLON => SEMICOLON,
         SDLK_LESS => LESS,
         SDLK_EQUALS => EQUALS,
         SDLK_GREATER => GREATER,
         SDLK_QUESTION => QUESTION,
         SDLK_AT => AT_key,
         SDLK_LEFTBRACKET => LEFTBRACKET,
         SDLK_BACKSLASH => BACKSLASH,
         SDLK_RIGHTBRACKET => RIGHTBRACKET,
         SDLK_CARET => CARET,
         SDLK_UNDERSCORE => UNDERSCORE,
         SDLK_BACKQUOTE => BACKQUOTE,
         SDLK_a => a,
         SDLK_b => b,
         SDLK_c => mmi.keyboard.c,
         SDLK_d => d,
         SDLK_e => e,
         SDLK_f => f,
         SDLK_g => g,
         SDLK_h => h,
         SDLK_i => i,
         SDLK_j => j,
         SDLK_k => k,
         SDLK_l => l,
         SDLK_m => m,
         SDLK_n => n,
         SDLK_o => o,
         SDLK_p => p,
         SDLK_q => q,
         SDLK_r => r,
         SDLK_s => s,
         SDLK_t => t,
         SDLK_u => u,
         SDLK_v => v,
         SDLK_w => w,
         SDLK_x => x,
         SDLK_y => y,
         SDLK_z => z,
         SDLK_DELETE => DELETE,
         SDLK_WORLD_0 => WORLD_0,
         SDLK_WORLD_1 => WORLD_1,
         SDLK_WORLD_2 => WORLD_2,
         SDLK_WORLD_3 => WORLD_3,
         SDLK_WORLD_4 => WORLD_4,
         SDLK_WORLD_5 => WORLD_5,
         SDLK_WORLD_6 => WORLD_6,
         SDLK_WORLD_7 => WORLD_7,
         SDLK_WORLD_8 => WORLD_8,
         SDLK_WORLD_9 => WORLD_9,
         SDLK_WORLD_10 => WORLD_10,
         SDLK_WORLD_11 => WORLD_11,
         SDLK_WORLD_12 => WORLD_12,
         SDLK_WORLD_13 => WORLD_13,
         SDLK_WORLD_14 => WORLD_14,
         SDLK_WORLD_15 => WORLD_15,
         SDLK_WORLD_16 => WORLD_16,
         SDLK_WORLD_17 => WORLD_17,
         SDLK_WORLD_18 => WORLD_18,
         SDLK_WORLD_19 => WORLD_19,
         SDLK_WORLD_20 => WORLD_20,
         SDLK_WORLD_21 => WORLD_21,
         SDLK_WORLD_22 => WORLD_22,
         SDLK_WORLD_23 => WORLD_23,
         SDLK_WORLD_24 => WORLD_24,
         SDLK_WORLD_25 => WORLD_25,
         SDLK_WORLD_26 => WORLD_26,
         SDLK_WORLD_27 => WORLD_27,
         SDLK_WORLD_28 => WORLD_28,
         SDLK_WORLD_29 => WORLD_29,
         SDLK_WORLD_30 => WORLD_30,
         SDLK_WORLD_31 => WORLD_31,
         SDLK_WORLD_32 => WORLD_32,
         SDLK_WORLD_33 => WORLD_33,
         SDLK_WORLD_34 => WORLD_34,
         SDLK_WORLD_35 => WORLD_35,
         SDLK_WORLD_36 => WORLD_36,
         SDLK_WORLD_37 => WORLD_37,
         SDLK_WORLD_38 => WORLD_38,
         SDLK_WORLD_39 => WORLD_39,
         SDLK_WORLD_40 => WORLD_40,
         SDLK_WORLD_41 => WORLD_41,
         SDLK_WORLD_42 => WORLD_42,
         SDLK_WORLD_43 => WORLD_43,
         SDLK_WORLD_44 => WORLD_44,
         SDLK_WORLD_45 => WORLD_45,
         SDLK_WORLD_46 => WORLD_46,
         SDLK_WORLD_47 => WORLD_47,
         SDLK_WORLD_48 => WORLD_48,
         SDLK_WORLD_49 => WORLD_49,
         SDLK_WORLD_50 => WORLD_50,
         SDLK_WORLD_51 => WORLD_51,
         SDLK_WORLD_52 => WORLD_52,
         SDLK_WORLD_53 => WORLD_53,
         SDLK_WORLD_54 => WORLD_54,
         SDLK_WORLD_55 => WORLD_55,
         SDLK_WORLD_56 => WORLD_56,
         SDLK_WORLD_57 => WORLD_57,
         SDLK_WORLD_58 => WORLD_58,
         SDLK_WORLD_59 => WORLD_59,
         SDLK_WORLD_60 => WORLD_60,
         SDLK_WORLD_61 => WORLD_61,
         SDLK_WORLD_62 => WORLD_62,
         SDLK_WORLD_63 => WORLD_63,
         SDLK_WORLD_64 => WORLD_64,
         SDLK_WORLD_65 => WORLD_65,
         SDLK_WORLD_66 => WORLD_66,
         SDLK_WORLD_67 => WORLD_67,
         SDLK_WORLD_68 => WORLD_68,
         SDLK_WORLD_69 => WORLD_69,
         SDLK_WORLD_70 => WORLD_70,
         SDLK_WORLD_71 => WORLD_71,
         SDLK_WORLD_72 => WORLD_72,
         SDLK_WORLD_73 => WORLD_73,
         SDLK_WORLD_74 => WORLD_74,
         SDLK_WORLD_75 => WORLD_75,
         SDLK_WORLD_76 => WORLD_76,
         SDLK_WORLD_77 => WORLD_77,
         SDLK_WORLD_78 => WORLD_78,
         SDLK_WORLD_79 => WORLD_79,
         SDLK_WORLD_80 => WORLD_80,
         SDLK_WORLD_81 => WORLD_81,
         SDLK_WORLD_82 => WORLD_82,
         SDLK_WORLD_83 => WORLD_83,
         SDLK_WORLD_84 => WORLD_84,
         SDLK_WORLD_85 => WORLD_85,
         SDLK_WORLD_86 => WORLD_86,
         SDLK_WORLD_87 => WORLD_87,
         SDLK_WORLD_88 => WORLD_88,
         SDLK_WORLD_89 => WORLD_89,
         SDLK_WORLD_90 => WORLD_90,
         SDLK_WORLD_91 => WORLD_91,
         SDLK_WORLD_92 => WORLD_92,
         SDLK_WORLD_93 => WORLD_93,
         SDLK_WORLD_94 => WORLD_94,
         SDLK_WORLD_95 => WORLD_95,
         SDLK_KP0 => KP0,
         SDLK_KP1 => KP1,
         SDLK_KP2 => KP2,
         SDLK_KP3 => KP3,
         SDLK_KP4 => KP4,
         SDLK_KP5 => KP5,
         SDLK_KP6 => KP6,
         SDLK_KP7 => KP7,
         SDLK_KP8 => KP8,
         SDLK_KP9 => KP9,
         SDLK_KP_PERIOD => KP_PERIOD,
         SDLK_KP_DIVIDE => KP_DIVIDE,
         SDLK_KP_MULTIPLY => KP_MULTIPLY,
         SDLK_KP_MINUS => KP_MINUS,
         SDLK_KP_PLUS => KP_PLUS,
         SDLK_KP_ENTER => KP_ENTER,
         SDLK_KP_EQUALS => KP_EQUALS,
         SDLK_UP => UP,
         SDLK_DOWN => DOWN,
         SDLK_RIGHT => RIGHT,
         SDLK_LEFT => LEFT,
         SDLK_INSERT => INSERT,
         SDLK_HOME => HOME,
         SDLK_END => END_key,
         SDLK_PAGEUP => PAGEUP,
         SDLK_PAGEDOWN => PAGEDOWN,
         SDLK_F1 => F1,
         SDLK_F2 => F2,
         SDLK_F3 => F3,
         SDLK_F4 => F4,
         SDLK_F5 => F5,
         SDLK_F6 => F6,
         SDLK_F7 => F7,
         SDLK_F8 => F8,
         SDLK_F9 => F9,
         SDLK_F10 => F10,
         SDLK_F11 => F11,
         SDLK_F12 => F12,
         SDLK_F13 => F13,
         SDLK_F14 => F14,
         SDLK_F15 => F15,
         SDLK_NUMLOCK => NUMLOCK,
         SDLK_CAPSLOCK => CAPSLOCK,
         SDLK_SCROLLOCK => SCROLLOCK,
         SDLK_RSHIFT => RSHIFT,
         SDLK_LSHIFT => LSHIFT,
         SDLK_RCTRL => RCTRL,
         SDLK_LCTRL => LCTRL,
         SDLK_RALT => RALT,
         SDLK_LALT => LALT,
         SDLK_RMETA => RMETA,
         SDLK_LMETA => LMETA,
         SDLK_LSUPER => LSUPER,
         SDLK_RSUPER => RSUPER,
         SDLK_MODE => MODE,
         SDLK_COMPOSE => COMPOSE,
         SDLK_HELP => HELP,
         SDLK_PRINT => PRINT,
         SDLK_SYSREQ => SYSREQ,
         SDLK_BREAK => BREAK,
         SDLK_MENU => MENU,
         SDLK_POWER => POWER,
         SDLK_EURO => EURO,
         SDLK_UNDO => UNDO,
         SDLK_LAST => LAST);




   procedure emit_Events  (Self : in out Item)
   is
      use SDL_c;

      event      : aliased SDL_Event.item;
      event_Type : SDL_EventType;

      function to_SDL_EventType is new ada.Unchecked_Conversion (Interfaces.Unsigned_8, SDL_EventType);
   begin

      while SDL_PollEvent (event'unchecked_Access) /= 0 loop
         event_Type := to_SDL_EventType (event.the_type);

         case event_Type  is
            when SDL_KEYDOWN =>
--                 put_Line ("KEYSYM: " & sdl_c.sdlMOD'image (event.key.keysym.the_mod));
--                 put_Line ("unicode: " & Interfaces.Unsigned_16'image (event.key.keysym.unicode));
--                 put_Line ("scancode: " & Interfaces.Unsigned_8'image (event.key.keysym.scancode));

               self.Keyboard.accept_key_press_Event (to_lumen_Key       (event.key.keysym.sym));
--                                                       to_lumen_Modifiers (SDL_GetModState)); --event.key.keysym.the_mod));

            when SDL_KEYUP =>
               self.Keyboard.accept_key_release_Event (to_lumen_Key       (event.key.keysym.sym));
--                                                         to_lumen_Modifiers (SDL_GetModState)); --event.key.keysym.the_mod));

            when SDL_MOUSEMOTION =>
               null; --put_Line ("Mouse moved");

            when SDL_MOUSEBUTTONDOWN =>
--                 put_Line ("BUTT ID: " & integer'image (Integer (event.button.button)));
               self.Mouse.accept_button_press_Event (mmi.mouse.button_Id (event.button.button),
                                                     self.Keyboard.Modifiers,
                                                     (Integer (event.button.x), Integer (event.button.y)));

            when SDL_MOUSEBUTTONUP =>
               self.Mouse.accept_button_release_Event (mmi.mouse.button_Id (event.button.button),
                                                       self.Keyboard.Modifiers,
                                                       (Integer (event.button.x), Integer (event.button.y)));

            when SDL_VIDEORESIZE =>
               self.Screens (1).Size_is (Integer (event.resize.w),
                                         Integer (event.resize.h));

            when SDL_QUIT =>
               self.is_Open := False;

            when others =>
               null;
         end case;
      end loop;

      SDL_GL_SwapBuffers;
   end;



end mmi.Display.sdl;
