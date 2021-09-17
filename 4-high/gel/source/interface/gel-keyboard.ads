with
     lace.Event,
     lace.Subject;


package gel.Keyboard
--
--  Provides an interface for a keyboard.
--
is

   type Item is  limited interface
             and lace.Subject.item;

   type View is access all Item'class;



   --- Keys
   --

   type Key is (Nil,
                BACKSPACE,
                TAB,
                CLEAR,
                ENTER,
                PAUSE,
                ESCAPE,
                SPACE,
                EXCLAIM,
                QUOTEDBL,
                HASH,
                DOLLAR,
                AMPERSAND,
                QUOTE,
                LEFTPAREN,
                RIGHTPAREN,
                ASTERISK,
                PLUS,
                COMMA,
                MINUS,
                PERIOD,
                SLASH,
                '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                COLON,  SEMICOLON,
                LESS,   EQUALS,    GREATER,
                QUESTION,
                AT_key,
                LEFTBRACKET,
                BACKSLASH,
                RIGHTBRACKET,
                CARET,
                UNDERSCORE,
                BACKQUOTE,
                a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
                DELETE,
                WORLD_0,  WORLD_1,  WORLD_2,  WORLD_3,  WORLD_4,  WORLD_5,  WORLD_6,  WORLD_7,  WORLD_8,  WORLD_9,
                WORLD_10, WORLD_11, WORLD_12, WORLD_13, WORLD_14, WORLD_15, WORLD_16, WORLD_17, WORLD_18, WORLD_19,
                WORLD_20, WORLD_21, WORLD_22, WORLD_23, WORLD_24, WORLD_25, WORLD_26, WORLD_27, WORLD_28, WORLD_29,
                WORLD_30, WORLD_31, WORLD_32, WORLD_33, WORLD_34, WORLD_35, WORLD_36, WORLD_37, WORLD_38, WORLD_39,
                WORLD_40, WORLD_41, WORLD_42, WORLD_43, WORLD_44, WORLD_45, WORLD_46, WORLD_47, WORLD_48, WORLD_49,
                WORLD_50, WORLD_51, WORLD_52, WORLD_53, WORLD_54, WORLD_55, WORLD_56, WORLD_57, WORLD_58, WORLD_59,
                WORLD_60, WORLD_61, WORLD_62, WORLD_63, WORLD_64, WORLD_65, WORLD_66, WORLD_67, WORLD_68, WORLD_69,
                WORLD_70, WORLD_71, WORLD_72, WORLD_73, WORLD_74, WORLD_75, WORLD_76, WORLD_77, WORLD_78, WORLD_79,
                WORLD_80, WORLD_81, WORLD_82, WORLD_83, WORLD_84, WORLD_85, WORLD_86, WORLD_87, WORLD_88, WORLD_89,
                WORLD_90, WORLD_91, WORLD_92, WORLD_93, WORLD_94, WORLD_95,
                KP0, KP1, KP2, KP3, KP4, KP5, KP6, KP7, KP8, KP9,
                KP_PERIOD,
                KP_DIVIDE, KP_MULTIPLY, KP_MINUS, KP_PLUS,
                KP_ENTER,  KP_EQUALS,
                UP, DOWN, RIGHT, LEFT,
                INSERT,
                HOME, END_key,
                PAGEUP, PAGEDOWN,
                F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15,
                NUMLOCK, CAPSLOCK, SCROLLOCK,
                RSHIFT,  LSHIFT,
                RCTRL,   LCTRL,
                RALT,    LALT,
                RMETA,   LMETA,
                LSUPER,  RSUPER,
                MODE,
                COMPOSE,
                HELP,
                PRINT,
                SYSREQ,
                BREAK,
                MENU,
                POWER,
                EURO,
                UNDO);

   function is_Graphic (Self : in Key) return Boolean;



   --- Modifiers
   --

   type Modifier is (LSHIFT,
                     RSHIFT,
                     LCTRL,
                     RCTRL,
                     LALT,
                     RALT,
                     LMETA,
                     RMETA,
                     NUM,
                     CAPS,
                     MODE);

   type modifier_Set is array (Modifier) of Boolean;

   no_Modifiers : constant modifier_Set;



   type modified_Key is
      record
         Key          : keyboard.Key;
         modifier_Set : keyboard.modifier_Set;
      end record;

   function Image (Self : in modified_Key) return Character;




   ---  Events
   --

   type key_press_Event is new lace.Event.item with
      record
         modified_Key : keyboard.modified_Key;
         Code         : Integer;
      end record;

   type key_release_Event is new lace.Event.item with
      record
         modified_Key : keyboard.modified_Key;
      end record;




   --  Attributes
   --

   function Modifiers     (Self : in Item) return Modifier_Set   is abstract;




   --  Operations
   --

   procedure emit_key_press_Event   (Self : in out Item;   Key      : in keyboard.Key;
                                                           key_Code : in Integer)        is abstract;

   procedure emit_key_release_Event (Self : in out Item;   Key      : in keyboard.Key)   is abstract;




private

   no_Modifiers : constant modifier_Set := (others => False);

end gel.Keyboard;
