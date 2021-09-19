with
     ada.Characters.latin_1;

package body gel.Keyboard
is

   function Image (Self : in modified_Key) return Character
   is
      use ada.Characters.latin_1;

      key_Map_of_character : constant array (Key) of Character
        := (SPACE  => ' ',
            QUOTE  => ''',
            COMMA  => ',',
            MINUS  => '-',
            PERIOD => '.',
            SLASH  => '/',

            '0' => '0',
            '1' => '1',
            '2' => '2',
            '3' => '3',
            '4' => '4',
            '5' => '5',
            '6' => '6',
            '7' => '7',
            '8' => '8',
            '9' => '9',

            SEMICOLON    => ';',
            EQUALS       => '=',
            LEFTBRACKET  => '[',
            BACKSLASH    => '\',
            RIGHTBRACKET => ']',
            BACKQUOTE    => '`',

            a => 'a',
            b => 'b',
            c => 'c',
            d => 'd',
            e => 'e',
            f => 'f',
            g => 'g',
            h => 'h',
            i => 'i',
            j => 'j',
            k => 'k',
            l => 'l',
            m => 'm',
            n => 'n',
            o => 'o',
            p => 'p',
            q => 'q',
            r => 'r',
            s => 's',
            t => 't',
            u => 'u',
            v => 'v',
            w => 'w',
            x => 'x',
            y => 'y',
            z => 'z',

            KP0 => '0',
            KP1 => '1',
            KP2 => '2',
            KP3 => '3',
            KP4 => '4',
            KP5 => '5',
            KP6 => '6',
            KP7 => '7',
            KP8 => '8',
            KP9 => '9',

            KP_PERIOD   => '.',
            KP_DIVIDE   => '/',
            KP_MULTIPLY => '*',
            KP_MINUS    => '-',
            KP_PLUS     => '+',
            KP_ENTER    => NUL,
            KP_EQUALS   => '=',

            others => NUL);


      shifted_key_Map_of_character : constant array (Key) of Character
        := (SPACE  => ' ',
            QUOTE  => '"',
            COMMA  => '<',
            MINUS  => '_',
            PERIOD => '>',
            SLASH  => '?',

            '0' => ')',
            '1' => '!',
            '2' => '@',
            '3' => '#',
            '4' => '$',
            '5' => '%',
            '6' => '^',
            '7' => '&',
            '8' => '*',
            '9' => '(',

            SEMICOLON    => ':',
            EQUALS       => '+',
            LEFTBRACKET  => '{',
            BACKSLASH    => '|',
            RIGHTBRACKET => '}',
            BACKQUOTE    => '~',

            a => 'A',
            b => 'B',
            c => 'C',
            d => 'D',
            e => 'E',
            f => 'F',
            g => 'G',
            h => 'H',
            i => 'I',
            j => 'J',
            k => 'K',
            l => 'L',
            m => 'M',
            n => 'N',
            o => 'O',
            p => 'P',
            q => 'Q',
            r => 'R',
            s => 'S',
            t => 'T',
            u => 'U',
            v => 'V',
            w => 'W',
            x => 'X',
            y => 'Y',
            z => 'Z',

            KP0 => '0',
            KP1 => '1',
            KP2 => '2',
            KP3 => '3',
            KP4 => '4',
            KP5 => '5',
            KP6 => '6',
            KP7 => '7',
            KP8 => '8',
            KP9 => '9',

            KP_PERIOD   => '.',
            KP_DIVIDE   => '/',
            KP_MULTIPLY => '*',
            KP_MINUS    => '-',
            KP_PLUS     => '+',
            KP_ENTER    => NUL,
            KP_EQUALS   => '=',

            others => NUL);

   begin
      if        Self.modifier_Set (LShift)
        or else Self.modifier_Set (RShift)
      then
         return shifted_key_Map_of_Character (Self.Key);
      else
         return         key_Map_of_Character (Self.Key);
      end if;
   end Image;



   function is_Graphic (Self : in Key) return Boolean
   is
   begin
      return    Self = SPACE
        or else Self = QUOTE
        or else Self = COMMA
        or else Self = MINUS
        or else Self = PERIOD
        or else Self = SLASH
        or else Self in '0' .. '9'
        or else Self = SEMICOLON
        or else Self = EQUALS
        or else Self = LEFTBRACKET
        or else Self = BACKSLASH
        or else Self = RIGHTBRACKET
        or else Self = BACKQUOTE
        or else Self in   a .. z
        or else Self in KP0 .. KP9
        or else Self = KP_PERIOD
        or else Self = KP_DIVIDE
        or else Self = KP_MULTIPLY
        or else Self = KP_MINUS
        or else Self = KP_PLUS
        or else Self = KP_EQUALS;
   end is_Graphic;


end gel.Keyboard;
