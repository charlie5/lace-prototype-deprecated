package lace.text.Cursor
--
-- Provides a cursor for traversing and interrogating text.
--
is
   type Item is tagged private;


   -- Forge
   --

   type Text_view is access constant lace.Text.item;

   function First (Self : in Text_view) return Cursor.item;


   -- Attributes
   --

   function Length (Self : in Item) return Natural;
   --
   -- Returns the length of the remaining text.

   function  has_Element (Self : in     Item) return Boolean;

   function  next_Token  (Self : in out Item;   Delimiter : in Character) return String;
   function  next_Token  (Self : in out item;   Delimiter : in String)    return String;

   procedure skip_Token  (Self : in out Item;   Delimiter : in Character);
   procedure skip_Token  (Self : in out Item;   Delimiter : in String);
   procedure skip_White  (Self : in out Item);

   procedure advance     (Self : in out Item;   Delimiter      : in String;
                                                Repeat         : in Positive := 1;
                                                skip_Delimiter : in Boolean  := True);
   --
   -- Search begins at the cursors current position.
   -- Advances to the position immediately after Delimiter.
   -- Sets Iterator to 0 if Delimiter is not found.
   -- Search is carried out 'Repeat' times.

   function Tokens (Self : in out Item;   Delimiter : in Character) return Text.items_32;
   function Tokens (Self : in out Item;   Delimiter : in Character) return Text.items_1k;
   function Tokens (Self : in out Item;   Delimiter : in Character) return Text.items_8k;


   function get_Integer (Self : in out Item) return Integer;
   --
   -- Skips whitespace and reads the next legal 'integer' value.
   -- Cursor is positioned at the next character following the integer.
   -- Raises no_data_Error if no legal integer exists.

   function get_Real (Self : in out Item) return long_Float;
   --
   -- Skips whitespace and reads the next legal 'real' value.
   -- Cursor is positioned at the next character following the real.
   -- Raises no_data_Error if no legal real exists.

   function Peek (Self : in Item;   Length : in Natural) return String;


   at_end_Error  : exception;
   no_data_Error : exception;



private

   type Item is tagged
      record
         Target  : Text_view;
         Current : Natural  := 0;
      end record;

end lace.text.Cursor;
