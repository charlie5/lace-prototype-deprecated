with ada.Unchecked_Conversion;
with ada.Text_IO; use Ada.Text_IO;

with lumen.Events.Keys;
with ada.Characters.Latin_1;



package body gel.Window.lumen
--
--
--
is
   use standard.Lumen.Events;

   package std_Lumen renames standard.Lumen;
   use std_Lumen;


   -------------
   --- Utilities
   --

   function to_gel_Key (From : in std_lumen.Events.Key_Symbol) return gel.keyboard.Key;


   -----------
   --- Globals
   --
   Escape     : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Letter_q   : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));

   the_Window : gel.Window.lumen.view;


   ---------
   --- Forge
   --

   procedure define (Self : in View;   Title  : in String;
                                       Width  : in Natural;
                                       Height : in Natural)
   is
--        Attrs : std_Lumen.Window.Context_Attributes := ((std_Lumen.Window.Attr_Red_Size,    8),
--                                                        (std_Lumen.Window.Attr_Green_Size,  8),
--                                                        (std_Lumen.Window.Attr_Blue_Size,   8),
--                                                        (std_Lumen.Window.Attr_Alpha_Size,  8),
--                                                        (std_Lumen.Window.Attr_Depth_Size, 24));
   begin
      std_Lumen.Window.create (Self.Window_handle,
                               Name       => Title,
                               Width      => Width,
                               Height     => Height
--           Direct     => True,
--           Attributes => Attrs,
--           Events     => (std_Lumen.Window.Want_Key_Press      => True,
--                          std_Lumen.Window.Want_Key_Release    => True,
--                          std_Lumen.Window.Want_Button_Press   => True,
--                          std_Lumen.Window.Want_Button_Release => True,
--                          std_Lumen.Window.Want_Exposure       => True,
--                          others                               => False));
        );

      if the_Window = null then
         the_Window := Self;
      else
         raise Error with "Attempt to define Lumen window twice.";
      end if;
   end define;


   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      std_Lumen.Window.destroy (Self.window_Handle);
      destroy (gel.Window.item (Self));                     -- Destroy base class.
   end destroy;



   package body Forge
   is
      function  to_Window (Title  : in String;
                           Width  : in Natural;
                           Height : in Natural) return gel.Window.lumen.item
      is
      begin
         return Self : gel.Window.lumen.item := (gel.Window.private_Forge.to_Window (Title, Width, Height)
                                                 with others => <>)
         do
            define (Self'unchecked_Access,  Title, Width, Height);
         end return;
      end to_Window;


      function new_Window (Title  : in String;
                           Width  : in Natural;
                           Height : in Natural) return gel.Window.lumen.view
      is
         Self : constant gel.Window.lumen.view := new Window.lumen.item' (to_Window (Title, Width, Height));
      begin
         return Self;
      end new_Window;
   end Forge;


   --------------
   --- Attributes
   --

   -- Nil.


   --------------
   --- Operations
   --

   use gel.Keyboard;


   overriding
   procedure swap_GL (Self : in out Item)
   is
   begin
      std_Lumen.Window.swap (Self.Window_handle);
   end swap_GL;



--     procedure key_press_Handler (Event : in std_Lumen.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Keyboard.accept_key_press_Event   (to_gel_Key (Event.key_Data.Key));
--     end;
--
--
--
--
--     procedure key_release_Handler (Event : in std_Lumen.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Keyboard.accept_key_release_Event (to_gel_Key (Event.key_Data.Key));
--     end;
--
--
--
--     procedure button_press_Handler (Event : in std_Lumen.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Mouse.emit_button_press_Event (gel.mouse.button_Id (button'Pos (event.button_Data.Changed) + 1),
--          --                                        the_Window.Keyboard.Modifiers,
--            --                                      (Integer (event.button_Data.x), Integer (event.button_Data.y)));
--
--     end;
--
--
--
--     procedure button_release_Handler (Event : in std_Lumen.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Mouse.emit_button_release_Event (gel.mouse.button_Id (button'Pos (event.button_Data.Changed) + 1),
--          --                                          the_Window.Keyboard.Modifiers,
--            --                                        (Integer (event.button_Data.x), Integer (event.button_Data.y)));
--     end;
--
--
--
--     procedure motion_Handler (Event : in std_Lumen.Events.Event_Data)
--     is
--     begin
--        null;
--        --the_Window.Mouse.emit_motion_Event ((Integer (event.button_Data.x),
--          --                                   Integer (event.button_Data.y)));
--     end;
--
--
--
--     procedure Expose_Handler (Event : in standard.Lumen.Events.Event_Data)
--     is
--     begin
--        null; --Draw;
--     end Expose_Handler;
--




   -------------
   --- Callbacks

   -- Callback triggered by the Resized event; tell OpenGL that the user has resized the window.
   --
   procedure resize_Handler (Height : in Integer;
                             Width  : in Integer)
   is
   begin
      the_Window.Size_is (Width,
                          Height);
   end resize_Handler;



   -- Simple event handler routine for keypresses.
   --
   procedure key_Handler (Category  : in std_Lumen.Events.key_Category;
                          Symbol    : in std_Lumen.Events.key_Symbol;
                          Modifiers : in std_Lumen.Events.modifier_Set) is
   begin
      case Symbol is
         when Escape
            | Letter_q =>
            the_Window.Is_Open := False;

         when Events.Keys.Home =>
--              RotV  := 0.0;
--              RotH  := 0.0;
--              RotC  := 0.0;
--              Scale := 1.0;
--              Display;
            null;

         when others =>
            the_Window.Keyboard.emit_key_press_Event (to_gel_Key (Symbol),
                                                      Integer (Symbol));
            null;
      end case;
   end key_Handler;



   -- Simple event handler routine for key releases.
   --
   procedure key_release_Handler (Category  : in std_Lumen.Events.Key_Category;
                                  Symbol    : in std_Lumen.Events.Key_Symbol;
                                  Modifiers : in std_Lumen.Events.Modifier_Set)
   is
   begin
      case Symbol is
         when Escape | Letter_q =>
            the_Window.Is_Open := False;

         when Events.Keys.Home =>
--              RotV  := 0.0;
--              RotH  := 0.0;
--              RotC  := 0.0;
--              Scale := 1.0;
--              Display;
            null;

         when others =>
            the_Window.Keyboard.emit_key_release_Event (to_gel_Key (Symbol));
      end case;
   end Key_Release_Handler;



   -- Mouse button has been pressed.
   --
   procedure button_Handler (X         : in Integer;
                             Y         : in Integer;
                             Button    : in std_Lumen.Window.Button_enum;
                             Modifiers : in Events.modifier_Set)
   is
      use type std_Lumen.Window.Button_Enum;
   begin
      -- Record starting position and current rotation/scale.

--        StartX := GL.Double (X);
--        StartY := GL.Double (Curr_H - Y);
--        Curr_RotV  := RotV;
--        Curr_RotH  := RotH;
--        Curr_RotC  := RotC;
--        Curr_Scale := Scale;

      -- Handle mouse wheel events.
      --
      if Button = std_Lumen.Window.Button_4
      then
--           Scale := Curr_Scale - 0.1;
--           Display;
         null;
      elsif Button = std_Lumen.Window.Button_5
      then
--           Scale := Curr_Scale + 0.1;
--           Display;
         null;
      end if;

      the_Window.Mouse.emit_button_press_Event (gel.Mouse.button_Id (std_Lumen.Window.Button_enum'Pos (Button) + 1),
                                                the_Window.Keyboard.Modifiers,
                                                (X, Y));
   end button_Handler;



   -- Mouse button has been released.
   --
   procedure button_release_Handler (X         : in Integer;
                                     Y         : in Integer;
                                     Button    : in std_Lumen.Window.Button_enum;
                                     Modifiers : in events.modifier_Set) is
   begin
      the_Window.Mouse.emit_button_release_Event (gel.mouse.button_Id (std_Lumen.Window.Button_enum'Pos (Button) + 1),
                                                  the_Window.Keyboard.Modifiers,
                                                  (X, Y));
   end button_release_Handler;



   -- Mouse movement.
   --
   procedure drag_Handler (X         : in Integer;
                           Y         : in Integer;
                           Modifiers : in Events.modifier_Set)
   is
      Y_Prime  : Integer;
   begin
      -- Get the event data.
      --
--        Y_Prime := Curr_H - Y;

      -- If it's a drag, update the figure parameters.
      --
      if Modifiers (Events.mod_Button_1)
      then
--           RotV := GL.Double (Integer (Curr_RotV + (GL.Double (Y_Prime) - StartY)) mod 360);
--           RotH := GL.Double (Integer (Curr_RotH + (GL.Double (X) - StartX)) mod 360);
--           Display;
         null;
      elsif Modifiers (Events.mod_Button_2)
      then
--           RotC := GL.Double (integer (Curr_RotC - ((GL.Double (X) - StartX) + (StartY - GL.Double (Y_Prime)))) mod 360);
--           Display;
         null;
      elsif Modifiers (Events.mod_Button_3)
      then
--           Scale := Curr_Scale + ((GL.Double (X) - StartX) / GL.Double (Curr_W)) -
--                    ((GL.Double (Y_Prime) - StartY) / GL.Double (Curr_H));
--           Display;
         null;
      end if;

      the_Window.Mouse.emit_motion_Event (Site => (X, Y));
   end drag_Handler;



   -- Re-draw the view.
   --
   procedure expose_Handler (Top    : in Integer;
                             Left   : in Integer;
                             Height : in Natural;
                             Width  : in Natural)
   is
   begin
--        put_Line ("Turn renderer ON"); -- Display;
      the_Window.is_Exposed := True;
   end expose_Handler;



   procedure unexpose_Handler (Top    : in Integer;
                               Left   : in Integer;
                               Height : in Natural;
                               Width  : in Natural)
   is
   begin
--        put_Line ("Turn renderer OFF"); -- Display;
      the_Window.is_Exposed := False;
   end unexpose_Handler;



   -- Called once per frame; just re-draws the scene.
   --
--     function New_Frame (Frame_Delta : in Duration) return Boolean is
--     begin  -- New_Frame
--        Display;
--        return not Terminated;
--     end New_Frame;


   overriding
   procedure emit_Events (Self : in out Item)
   is
      Success : Boolean;
   begin
      Success := std_Lumen.Window.process_Events (Self.Window_Handle);

      Self.is_Open := Success;

--        std_Lumen.Events.animate.process_current_Events
--          (Win   => Self.the_window_Handle,
--           Calls => (Key_Press      => key_press_Handler     'unrestricted_Access,
--                     Key_Release    => key_release_Handler   'unrestricted_Access,
--                     button_Press   => button_press_Handler  'unrestricted_Access,
--                     button_Release => button_release_Handler'unrestricted_Access,
--                     pointer_Motion => motion_Handler        'unrestricted_Access,
--                     Exposed        => Expose_Handler        'unrestricted_Access,
--                     Resized        => Resize_Handler        'unrestricted_Access,
--                     Close_Window   => Quit_Handler          'unrestricted_Access,
--                     others         => No_Callback));

      Self.Window_handle.Exposed     := expose_Handler        'unrestricted_Access;
      Self.Window_handle.Unexposed   := unexpose_Handler      'unrestricted_Access;
      Self.Window_handle.Resize      := resize_Handler        'unrestricted_Access;
      Self.Window_handle.Key_press   := key_Handler           'unrestricted_Access;
      Self.Window_handle.Key_release := key_release_Handler   'unrestricted_Access;
      Self.Window_handle.Mouse_down  := button_Handler        'unrestricted_Access;
      Self.Window_handle.Mouse_up    := button_release_Handler'unrestricted_Access;
      Self.Window_handle.Mouse_move  := drag_Handler          'unrestricted_Access;

      std_Lumen.Window.Swap (Self.Window_handle);
   end emit_Events;


   overriding
   procedure enable_GL (Self : in Item)
   is
   begin
      std_Lumen.Window.make_current (Self.Window_handle);
   end enable_GL;


   overriding
   procedure disable_GL (Self : in Item)
   is
   begin
      std_Lumen.Window.make_non_current (Self.Window_handle);
   end disable_GL;


   -------------
   --- Utilities
   --

   function to_gel_Key (From : in std_lumen.Events.key_Symbol) return gel.keyboard.Key
   is
   begin
--        put_Line ("Key: " & std_lumen.Events.Key_Symbol'Image (From));

      case From
      is
         when 13        => return gel.Keyboard.Enter;
         when 27        => return gel.Keyboard.Escape;
         when 32        => return gel.Keyboard.Space;
         when 43        => return gel.Keyboard.KP_PLUS;

         when 48 ..  59 => return gel.keyboard.Key'Val (From - 48 + gel.keyboard.Key'Pos (KP0));
         when 65 ..  90 => return gel.keyboard.Key'Val (From - 65 + gel.keyboard.Key'Pos (a));
         when 97 .. 122 => return gel.keyboard.Key'Val (From - 97 + gel.keyboard.Key'Pos (a));

         when 65505     => return lShift;
         when 65506     => return rShift;

         when 65507     => return lCtrl;
         when 65508     => return rCtrl;

         when 65513     => return lAlt;
         when 65514     => return rAlt;

         when 65361     => return Left;
         when 65362     => return Up;
         when 65363     => return Right;
         when 65364     => return Down;

--           when 65565   => return PAGEUP;
--           when 65566   => return PAGEDOWN;
         when 65365     => return PAGEUP;
         when 65366     => return PAGEDOWN;

         when others    => put_Line ("Unhandled key: " & From'Image);
      end case;

      return gel.keyboard.Key'First;
   end to_gel_Key;


   ------------------
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
end gel.Window.lumen;
