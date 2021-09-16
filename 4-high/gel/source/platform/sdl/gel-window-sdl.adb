--  with lumen.Events.Animate;
with
     SDL.Video.GL,
     SDL.Events.Keyboards,
     SDL.Video.Windows.Makers,
     SDL.Events.Events,
     SDL.Log;

with interfaces.C;

with ada.Unchecked_Conversion;
with ada.Text_IO; use Ada.Text_IO;

with interfaces.c;
--  with lumen.Window;
--  with lumen.Events.Keys;
with ada.Characters.Latin_1;
with SDL.Events.Mice;



package body mmi.Window.sdl
--
--
--
is
   use -- standard.Lumen.Events,
       Interfaces, Interfaces.C;



   package std_SDL             renames standard.SDL;
   package sdl_Keyboard_Events renames std_SDL.Events.Keyboards;


   program_Exit      : exception;

--     the_Window        : mmi.Window.lumen.view;
--     the_window_Handle : standard.lumen.Window.handle;



   procedure swap_GL (Self : in out Item)
   is
      use std_SDL.Video.GL;
   begin
      swap (Self.window_Handle);
--        std_SDL.Video.Windows.swap (Self.window_Handle);
   end;




   use std_SDL,
       std_SDL.Events;

--     function to_mmi_Key (From : in Interfaces.Unsigned_8) return mmi.keyboard.Key;
   function to_mmi_Key (From : in std_SDL.Events.Keyboards.Key_Codes) return mmi.keyboard.Key;

   ----------------------------------------------------------------------------

   -- Constants
--     Win_Start : constant GL.SizeI := 500;  -- in pixels; adjust to suit your scene's needs

   -- Keystrokes we care about
   Escape    : constant sdl_Keyboard_Events.Key_Codes := sdl_Keyboard_Events.Key_Codes (Character'Pos (Ada.Characters.Latin_1.ESC));
   Letter_q  : constant sdl_Keyboard_Events.Key_Codes := sdl_Keyboard_Events.Key_Codes (Character'Pos (Ada.Characters.Latin_1.LC_Q));


   the_Window : mmi.Window.sdl.view;


   --- construction
   --

   procedure define  (Self : in     View;   Title  : in String;
                                            Width  : in Natural;
                                            Height : in Natural)
   is
      use type std_SDL.Video.Windows.Window;
--        Attrs : std_SDL.Window.Context_Attributes := ((std_SDL.Window.Attr_Red_Size,    8),
--                                                        (std_SDL.Window.Attr_Green_Size,  8),
--                                                        (std_SDL.Window.Attr_Blue_Size,   8),
--                                                        (std_SDL.Window.Attr_Alpha_Size,  8),
--                                                        (std_SDL.Window.Attr_Depth_Size, 24));
   begin

      if std_SDL.Initialise = False
      then
         raise mmi.Error with "unable to initialise SDL";
      end if;

      std_SDL.Video.Windows.Makers.Create (Win    => Self.window_Handle,
                                           Title  => Title,
                                           X      => 100,
                                           Y      => 100,
                                           Width  => Width,
                                           Height => Height);

      std_SDL.Video.GL.create (Self.GL_Context, from => Self.window_Handle);


--        standard.Lumen.Window.Create
--          (Self.window_Handle,
--           Name       => Title,
--           Width      => Width,
--           Height     => Height
--           Direct     => True,
--           Attributes => Attrs,
--           Events     => (std_SDL.Window.Want_Key_Press      => True,
--                          std_SDL.Window.Want_Key_Release    => True,
--                          std_SDL.Window.Want_Button_Press   => True,
--                          std_SDL.Window.Want_Button_Release => True,
--                          std_SDL.Window.Want_Exposure       => True,
--                          others                               => False));
--          );


      if the_Window = null
      then
         the_Window := Self;
      else
         raise program_Error with "Attempt to define SDL window twice.";
      end if;
   end define;



   procedure destroy (Self : in out Item)
   is
   begin
--        standard.Lumen.Window.destroy (Self.window_Handle);
      Self.window_Handle.Finalize;
      std_SDL.Finalise;

      destroy (mmi.Window.item (Self));                     -- Destroy base class.
   end;



   package body Forge
   is
      function  to_Window (Title  : in String;
                           Width  : in Natural;
                           Height : in Natural) return mmi.Window.sdl.item
      is
      begin
         return Self : mmi.Window.sdl.item := (mmi.Window.private_Forge.to_Window (Title, Width, Height)
                                               with others => <>)
         do
            define (Self'unchecked_Access,  Title, Width, Height);
         end return;
      end;


      function new_Window (Title  : in String;
                           Width  : in Natural;
                           Height : in Natural) return Window.sdl.view
      is
         Self : mmi.Window.sdl.view := new Window.sdl.item' (to_Window (Title, Width, Height));
      begin
         return Self;
      end;
   end Forge;




   --  Attributes
   --





   --  Operations
   --

   use mmi.Keyboard;




--     procedure key_press_Handler (Event : in std_SDL.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Keyboard.accept_key_press_Event   (to_mmi_Key (Event.key_Data.Key));
--     end;
--
--
--
--
--     procedure key_release_Handler (Event : in std_SDL.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Keyboard.accept_key_release_Event (to_mmi_Key (Event.key_Data.Key));
--     end;
--
--
--
--     procedure button_press_Handler (Event : in std_SDL.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Mouse.emit_button_press_Event (mmi.mouse.button_Id (button'Pos (event.button_Data.Changed) + 1),
--          --                                        the_Window.Keyboard.Modifiers,
--            --                                      (Integer (event.button_Data.x), Integer (event.button_Data.y)));
--
--     end;
--
--
--
--     procedure button_release_Handler (Event : in std_SDL.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Mouse.emit_button_release_Event (mmi.mouse.button_Id (button'Pos (event.button_Data.Changed) + 1),
--          --                                          the_Window.Keyboard.Modifiers,
--            --                                        (Integer (event.button_Data.x), Integer (event.button_Data.y)));
--     end;
--
--
--
--     procedure motion_Handler (Event : in std_SDL.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Mouse.emit_motion_Event ((Integer (event.button_Data.x),
--          --                                   Integer (event.button_Data.y)));
--     end;
--
--
--
--
--     procedure Quit_Handler (Event : in standard.Lumen.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.is_Open := False;
--     end Quit_Handler;
--
--
--
--
--     procedure Expose_Handler (Event : in standard.Lumen.Events.Event_Data)
--     is
--     begin
--        null; --Draw;
--     end Expose_Handler;
--




   ----------------------------------------------------------------------------
   -----                                                                  -----
   -----                      C A L L B A C K S                           -----
   -----                                                                  -----
   ----------------------------------------------------------------------------

   -- Callback triggered by the Resized event; tell OpenGL that the
   -- user has resized the window
   procedure Resize_Handler (Height : in Integer;
                             Width  : in Integer) is
   begin  -- Resize_Handler
      the_Window.Size_is (width,
                          height);

   end Resize_Handler;



--
--
--
--
--     ----------------------------------------------------------------------------
--
--     -- Re-draw the view
--     procedure Expose_Handler (Top    : in Integer;
--                               Left   : in Integer;
--                               Height : in Natural;
--                               Width  : in Natural) is
--     begin  -- Expose_Handler
--  --        put_Line ("Turn renderer ON"); -- Display;
--        the_Window.is_Exposed := True;
--     end Expose_Handler;
--
--
--     procedure Unexpose_Handler (Top    : in Integer;
--                               Left   : in Integer;
--                               Height : in Natural;
--                               Width  : in Natural) is
--     begin  -- Expose_Handler
--  --        put_Line ("Turn renderer OFF"); -- Display;
--        the_Window.is_Exposed := False;
--     end Unexpose_Handler;
--
--     ----------------------------------------------------------------------------
--
--     -- Called once per frame; just re-draws the scene
--  --     function New_Frame (Frame_Delta : in Duration) return Boolean is
--  --     begin  -- New_Frame
--  --        Display;
--  --        return not Terminated;
--  --     end New_Frame;







   procedure emit_Events  (Self : in out Item)
   is
      use std_SDL;

      event      : aliased std_SDL.Events.Events.Events;
--        event_Type :         SDL_EventType;

--        function to_SDL_EventType is new ada.Unchecked_Conversion (Interfaces.Unsigned_8, SDL_EventType);

      use type std_SDL.Events.Event_Types;
      use type std_SDL.Events.Keyboards.Key_Codes;

   begin
      while std_SDL.Events.Events.Poll (Event)
      loop
         case Event.Common.Event_Type
         is
            when std_SDL.Events.Quit =>
               Self.is_Open := False;


            when std_SDL.Events.Keyboards.Key_Down =>

               the_Window.Keyboard.emit_key_press_Event (to_mmi_Key (Event.Keyboard.Key_Sym.Key_Code),
                                                         Integer (Event.Keyboard.Key_Sym.Key_Code));


            when std_SDL.Events.Keyboards.Key_Up =>
               std_SDL.Log.Put_Debug ("Key up event    : " &
                                        std_SDL.Events.Keyboards.Key_Codes'Image (Event.Keyboard.Key_Sym.Key_Code) &
                                        "    Scan code: " &
                                        std_SDL.Events.Keyboards.Scan_Codes'Image (Event.Keyboard.Key_Sym.Scan_Code));

               if Event.Keyboard.Key_Sym.Key_Code = std_SDL.Events.Keyboards.Code_Escape
               then
                  Self.is_Open := False;
               end if;

               the_Window.Keyboard.emit_key_release_Event (to_mmi_Key (Event.Keyboard.Key_Sym.Key_Code));

--              when std_SDL.Events.Joysticks.Axis_Motion =>
--                 std_SDL.Log.Put_Debug
--                   ("Joystick axis event (ID = " & std_SDL.Events.Joysticks.IDs'Image (Event.Joystick_Axis.Which) &
--                      "): Axis: " & std_SDL.Events.Joysticks.Axes'Image (Event.Joystick_Axis.Axis) &
--                      "    Value: " & std_SDL.Events.Joysticks.Axes_Values'Image (Event.Joystick_Axis.Value));


            when std_SDL.Events.Mice.Button_Down =>
               put_Line ("Mouse Button Down !" & std_SDL.Events.Mice.Buttons'Image (Event.Mouse_Button.Button));

--                 the_Window.Mouse.emit_button_press_Event (mmi.mouse.button_Id (std_SDL.Window.Button_Enum'Pos (Event.Mouse.Button) + 1),
               the_Window.Mouse.emit_button_press_Event (mmi.mouse.button_Id (std_SDL.Events.Mice.Buttons'Pos (Event.Mouse_Button.Button) + 1),
                                                         the_Window.Keyboard.Modifiers,
                                                         (Integer (Event.Mouse_Button.X),
                                                          Integer (Event.Mouse_Button.Y)));



            when std_SDL.Events.Mice.Button_Up =>
               put_Line ("Mouse Button Up !");

               the_Window.Mouse.emit_button_release_Event (mmi.mouse.button_Id (std_SDL.Events.Mice.Buttons'Pos (Event.Mouse_Button.Button) + 1),
                                                           the_Window.Keyboard.Modifiers,
                                                           (Integer (Event.Mouse_Button.X),
                                                            Integer (Event.Mouse_Button.Y)));

            when std_SDL.Events.Mice.Motion =>
               put_Line ("Mouse Motion !");

               the_Window.Mouse.emit_motion_Event (site => (Integer (Event.Mouse_Motion.x),
                                                            Integer (Event.Mouse_Motion.y)));


            when std_SDL.Events.Mice.Wheel =>
               put_Line ("Mouse Wheel !");


            when others =>
               null;
         end case;
      end loop;


--        while SDL_PollEvent (event'unchecked_Access) /= 0
--        loop
--           event_Type := to_SDL_EventType (event.the_type);
--
--           case event_Type  is
--              when SDL_KEYDOWN =>
--  --                 put_Line ("KEYSYM:   " & sdl_c.sdlMOD'image           (event.key.keysym.the_mod));
--  --                 put_Line ("unicode:  " & Interfaces.Unsigned_16'image (event.key.keysym.unicode));
--  --                 put_Line ("scancode: " & Interfaces.Unsigned_8'image  (event.key.keysym.scancode));
--
--                 self.Keyboard.accept_key_press_Event (to_lumen_Key       (event.key.keysym.sym));
--  --                                                       to_lumen_Modifiers (SDL_GetModState)); --event.key.keysym.the_mod));
--
--              when SDL_KEYUP =>
--                 self.Keyboard.accept_key_release_Event (to_lumen_Key       (event.key.keysym.sym));
--  --                                                         to_lumen_Modifiers (SDL_GetModState)); --event.key.keysym.the_mod));
--
--              when SDL_MOUSEMOTION =>
--                 self.Mouse.emit_motion_Event ((Integer (event.button.x),
--                                                Integer (event.button.y)));
--
--              when SDL_MOUSEBUTTONDOWN =>
--                 self.Mouse.emit_button_press_Event (mmi.mouse.button_Id (event.button.button),
--                                                     self.Keyboard.Modifiers,
--                                                     (Integer (event.button.x), Integer (event.button.y)));
--
--              when SDL_MOUSEBUTTONUP =>
--                 self.Mouse.emit_button_release_Event (mmi.mouse.button_Id (event.button.button),
--                                                         self.Keyboard.Modifiers,
--                                                         (Integer (event.button.x), Integer (event.button.y)));
--
--              when SDL_VIDEORESIZE =>
--                 self.Size_is (Integer (event.resize.w),
--                               Integer (event.resize.h));
--
--              when SDL_QUIT =>
--                 self.is_Open := False;
--
--              when others =>
--                 null;
--           end case;
--        end loop;
--
--        SDL_GL_SwapBuffers;
   end;





--     procedure emit_Events  (Self : in out Item)
--     is
--        Success : Boolean;
--     begin
--        Success := std_SDL.Window.process_Events (Self.Window_Handle);
--
--        Self.window_Handle.Exposed     := Expose_Handler        'Unrestricted_Access;
--        Self.window_Handle.Unexposed   := Unexpose_Handler      'Unrestricted_Access;
--        Self.window_Handle.Resize      := Resize_Handler        'Unrestricted_Access;
--        Self.window_Handle.Key_Press   := Key_Handler           'Unrestricted_Access;
--        Self.window_Handle.Key_Release := Key_Release_Handler   'Unrestricted_Access;
--        Self.window_Handle.Mouse_Down  := Button_Handler        'Unrestricted_Access;
--        Self.window_Handle.Mouse_Up    := Button_Release_Handler'Unrestricted_Access;
--        Self.window_Handle.Mouse_Move  := Drag_Handler          'Unrestricted_Access;
--
--        std_SDL.Window.Swap (Self.window_Handle);
--     end;



   procedure enable_GL    (Self : in     Item)
   is
   begin
      std_SDL.Video.gl.set_Current (Self.GL_Context, to => Self.window_Handle);
--        standard.lumen.Window.make_current (Self.window_Handle);
   end;



   procedure disable_GL    (Self : in     Item)
   is
      null_Context : standard.SDL.Video.GL.Contexts;
   begin
      std_SDL.Video.gl.set_Current (null_Context, to => Self.window_Handle);
--        standard.lumen.Window.make_non_current (Self.window_Handle);
   end;



   function to_mmi_Key (From : in std_SDL.Events.Keyboards.Key_Codes) return mmi.keyboard.Key
   is
      use type std_SDL.events.keyboards.Key_Codes;
   begin
--        put_Line ("Key: " & std_SDL.Events.Key_Symbol'Image (From));

      put_Line ("FOUND Key: " & std_SDL.Events.Keyboards.Key_Codes'Image (From));

      case From is
         when std_SDL.Events.Keyboards.code_Return        => return mmi.Keyboard.Enter;
         when 27        => return mmi.Keyboard.Escape;
         when 32        => return mmi.Keyboard.Space;
         when 43        => return mmi.Keyboard.KP_PLUS;

         when 48 ..  59 => return mmi.keyboard.Key'Val (From - 48 + mmi.keyboard.Key'Pos (KP0));
         when 65 ..  90 => return mmi.keyboard.Key'Val (From - 65 + mmi.keyboard.Key'Pos (a));
         when 97 .. 122 => return mmi.keyboard.Key'Val (From - 97 + mmi.keyboard.Key'Pos (a));

         when 65505   => return lShift;
         when 65506   => return rShift;

         when 65507   => return lCtrl;
         when 65508   => return rCtrl;

         when 65513   => return lAlt;
         when 65514   => return rAlt;

         when 65362   => return Up;
         when 65363   => return Right;
         when 65364   => return Down;

         when std_SDL.Events.Keyboards.code_Left => return mmi.Keyboard.LEFT;
--           when 65565   => return PAGEUP;
--           when 65566   => return PAGEDOWN;
         when 65365   => return PAGEUP;
         when 65366   => return PAGEDOWN;

         when others  =>
            if From = std_SDL.Events.Keyboards.code_Left
            then
               return Left;
            else
               put_Line ("unhandled Key: " & std_SDL.Events.Keyboards.Key_Codes'Image (From));
            end if;
      end case;

      return mmi.Keyboard.Key'First;
   end;




   -------------------
   --- Window Creator
   --

   function window_Creator (Name : in String;
                            Width,
                            Height : in Positive) return mmi.Window.view
   is
   begin
      return mmi.Window.view (Forge.new_Window (Name, Width, Height));
   end;


begin

   mmi.Window.use_create_Window (window_Creator'Access);

end mmi.Window.sdl;
