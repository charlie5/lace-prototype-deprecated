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

   program_Exit      : exception;

--     the_Window        : gel.Window.lumen.view;
--     the_window_Handle : standard.lumen.Window.handle;



   overriding
   procedure swap_GL (Self : in out Item)
   is
   begin
      std_Lumen.Window.swap (Self.window_Handle);
   end swap_GL;




   use std_Lumen;  -- yes, we're definitely using Lumen
--     function to_gel_Key (From : in Interfaces.Unsigned_8) return gel.keyboard.Key;
   function to_gel_Key (From : in std_lumen.Events.Key_Symbol) return gel.keyboard.Key;

   ----------------------------------------------------------------------------

   -- Constants
--     Win_Start : constant GL.SizeI := 500;  -- in pixels; adjust to suit your scene's needs

   -- Keystrokes we care about
   Escape    : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Letter_q  : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));


   the_Window : gel.Window.lumen.view;


   --- construction
   --

   procedure define  (Self : in     View;   Title  : in String;
                                            Width  : in Natural;
                                            Height : in Natural)
   is
--        Attrs : std_Lumen.Window.Context_Attributes := ((std_Lumen.Window.Attr_Red_Size,    8),
--                                                        (std_Lumen.Window.Attr_Green_Size,  8),
--                                                        (std_Lumen.Window.Attr_Blue_Size,   8),
--                                                        (std_Lumen.Window.Attr_Alpha_Size,  8),
--                                                        (std_Lumen.Window.Attr_Depth_Size, 24));
   begin
      standard.Lumen.Window.Create
        (Self.window_Handle,
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
         raise program_Error with "Attempt to define lumen window twice.";
      end if;
   end define;




   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      standard.Lumen.Window.destroy (Self.window_Handle);
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




   --  Attributes
   --





   --  Operations
   --

   use gel.Keyboard;




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

   ---------------------------------------------------------------------------

   -- Simple event handler routine for keypresses
   procedure Key_Handler (Category : in std_Lumen.Events.Key_Category;
                          Symbol   : in std_Lumen.Events.Key_Symbol;
                          Modifiers : in std_Lumen.Events.Modifier_Set) is
   begin  -- Key_Handler
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
            the_Window.Keyboard.emit_key_press_Event (to_gel_Key (Symbol),
                                                      Integer (Symbol));
            null;
      end case;
   end Key_Handler;

   ----------------------------------------------------------------------------

   -- Simple event handler routine for key releases
   procedure Key_Release_Handler (Category  : in std_Lumen.Events.Key_Category;
                                  Symbol    : in std_Lumen.Events.Key_Symbol;
                                  Modifiers : in std_Lumen.Events.Modifier_Set) is
   begin  -- Key_Handler
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

   ----------------------------------------------------------------------------

   -- Mouse button has been pressed
   procedure Button_Handler (X         : in Integer;
                             Y         : in Integer;
                             Button    : in std_Lumen.Window.Button_Enum;
                             Modifiers : in Events.Modifier_Set) is

      use type std_Lumen.Window.Button_Enum;

   begin  -- Button_Handler

      -- Record starting position and current rotation/scale
--        StartX := GL.Double (X);
--        StartY := GL.Double (Curr_H - Y);
--        Curr_RotV  := RotV;
--        Curr_RotH  := RotH;
--        Curr_RotC  := RotC;
--        Curr_Scale := Scale;

      -- Handle mouse wheel events
      if Button = std_Lumen.Window.Button_4 then
--           Scale := Curr_Scale - 0.1;
--           Display;
         null;
      elsif Button = std_Lumen.Window.Button_5 then
--           Scale := Curr_Scale + 0.1;
--           Display;
         null;
      end if;

      the_Window.Mouse.emit_button_press_Event (gel.mouse.button_Id (std_Lumen.Window.Button_Enum'Pos (Button) + 1),
                                                the_Window.Keyboard.Modifiers,
                                                (X, Y));
   end Button_Handler;



   -- Mouse button has been released.
   procedure Button_Release_Handler (X         : in Integer;
                                     Y         : in Integer;
                                     Button    : in std_Lumen.Window.Button_Enum;
                                     Modifiers : in Events.Modifier_Set) is
   begin  -- Button_Handler
      the_Window.Mouse.emit_button_release_Event (gel.mouse.button_Id (std_Lumen.Window.Button_Enum'Pos (Button) + 1),
                                                  the_Window.Keyboard.Modifiers,
                                                  (X, Y));
   end Button_Release_Handler;


   ----------------------------------------------------------------------------

   -- Mouse movement
   procedure Drag_Handler (X         : in Integer;
                           Y         : in Integer;
                           Modifiers : in Events.Modifier_Set) is

      Y_Prime  : Integer;

   begin  -- Drag_Handler

      -- Get the event data
--        Y_Prime := Curr_H - Y;

      -- If it's a drag, update the figure parameters
      if Modifiers (Events.Mod_Button_1) then
--           RotV := GL.Double (Integer (Curr_RotV + (GL.Double (Y_Prime) - StartY)) mod 360);
--           RotH := GL.Double (Integer (Curr_RotH + (GL.Double (X) - StartX)) mod 360);
--           Display;
         null;
      elsif Modifiers (Events.Mod_Button_2) then
--           RotC := GL.Double (integer (Curr_RotC - ((GL.Double (X) - StartX) + (StartY - GL.Double (Y_Prime)))) mod 360);
--           Display;
         null;
      elsif Modifiers (Events.Mod_Button_3) then
--           Scale := Curr_Scale + ((GL.Double (X) - StartX) / GL.Double (Curr_W)) -
--                    ((GL.Double (Y_Prime) - StartY) / GL.Double (Curr_H));
--           Display;
         null;
      end if;

      the_Window.Mouse.emit_motion_Event (site => (x, y));
   end Drag_Handler;

   ----------------------------------------------------------------------------

   -- Re-draw the view
   procedure Expose_Handler (Top    : in Integer;
                             Left   : in Integer;
                             Height : in Natural;
                             Width  : in Natural) is
   begin  -- Expose_Handler
--        put_Line ("Turn renderer ON"); -- Display;
      the_Window.is_Exposed := True;
   end Expose_Handler;


   procedure Unexpose_Handler (Top    : in Integer;
                             Left   : in Integer;
                             Height : in Natural;
                             Width  : in Natural) is
   begin  -- Expose_Handler
--        put_Line ("Turn renderer OFF"); -- Display;
      the_Window.is_Exposed := False;
   end Unexpose_Handler;

   ----------------------------------------------------------------------------

   -- Called once per frame; just re-draws the scene
--     function New_Frame (Frame_Delta : in Duration) return Boolean is
--     begin  -- New_Frame
--        Display;
--        return not Terminated;
--     end New_Frame;


   overriding
   procedure emit_Events  (Self : in out Item)
   is
      Success : Boolean;
   begin
      Success := std_Lumen.Window.process_Events (Self.Window_Handle);

      Self.is_Open := Success;

--        std_Lumen.Events.animate.process_current_Events
--          (Win   => Self.the_window_Handle,
--           Calls => (Key_Press      => key_press_Handler     'Unrestricted_Access,
--                     Key_Release    => key_release_Handler   'Unrestricted_Access,
--                     button_Press   => button_press_Handler  'Unrestricted_Access,
--                     button_Release => button_release_Handler'Unrestricted_Access,
--                     pointer_Motion => motion_Handler        'Unrestricted_Access,
--                     Exposed        => Expose_Handler        'Unrestricted_Access,
--                     Resized        => Resize_Handler        'Unrestricted_Access,
--                     Close_Window   => Quit_Handler          'Unrestricted_Access,
--                     others         => No_Callback));

      Self.window_Handle.Exposed     := Expose_Handler        'Unrestricted_Access;
      Self.window_Handle.Unexposed   := Unexpose_Handler      'Unrestricted_Access;
      Self.window_Handle.Resize      := Resize_Handler        'Unrestricted_Access;
      Self.window_Handle.Key_Press   := Key_Handler           'Unrestricted_Access;
      Self.window_Handle.Key_Release := Key_Release_Handler   'Unrestricted_Access;
      Self.window_Handle.Mouse_Down  := Button_Handler        'Unrestricted_Access;
      Self.window_Handle.Mouse_Up    := Button_Release_Handler'Unrestricted_Access;
      Self.window_Handle.Mouse_Move  := Drag_Handler          'Unrestricted_Access;

      std_Lumen.Window.Swap (Self.window_Handle);
   end emit_Events;



   overriding
   procedure enable_GL    (Self : in     Item)
   is
   begin
      standard.lumen.Window.make_current (Self.window_Handle);
   end enable_GL;



   overriding
   procedure disable_GL    (Self : in     Item)
   is
   begin
      standard.lumen.Window.make_non_current (Self.window_Handle);
   end disable_GL;



   function to_gel_Key (From : in std_lumen.Events.Key_Symbol) return gel.keyboard.Key
   is
   begin
--        put_Line ("Key: " & std_lumen.Events.Key_Symbol'Image (From));

      case From is
         when 13        => return gel.Keyboard.Enter;
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

         when 65361   => return Left;
         when 65362   => return Up;
         when 65363   => return Right;
         when 65364   => return Down;

--           when 65565   => return PAGEUP;
--           when 65566   => return PAGEDOWN;
         when 65365   => return PAGEUP;
         when 65366   => return PAGEDOWN;

         when others  => put_Line ("unhandled Key: " & std_lumen.Events.Key_Symbol'Image (From));
      end case;

      return gel.keyboard.Key'First;
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

end gel.Window.lumen;
