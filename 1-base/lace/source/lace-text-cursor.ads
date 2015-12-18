package lace.Text.Cursor
--
-- Provides a cursor for traversing and interrogating text.
--
is

   type Item is tagged private;



   -- Forge
   --

   type Text_view is access all lace.Text.item;

   function First (Self : in Text_view) return Cursor.item;



   -- Attributes
   --

   function Length (Self : in Item) return Natural;
   --
   -- returns the length of the remaining text.



   function  has_Element  (Self : in     Item)                                   return Boolean;

   function  next_Token  (Self : access Item;   Delimiter : in Character) return String;
   function  next_Token  (Self : access item;   Delimiter : in String   ) return String;

   procedure skip_Token  (Self : access Item;   Delimiter : in Character);
   procedure skip_Token  (Self : access Item;   Delimiter : in String   );


   procedure advance     (Self : in out item;   Delimiter      : in     String;
                                                Repeat         : in     Positive := 1;
                                                Skip_Delimiter : in     Boolean  := True);
   --
   -- Search begins at the cursors current position.
   -- Advances to the position immediately after Delimiter.
   -- Sets Iterator to 0 if Delimiter is not found.
   -- Search is carried out 'Repeat' times.


   procedure skip_White (Self : in out Item);



   function Tokens      (Self : access Item;   Delimiter : in Character) return Text.items_32;
   function Tokens      (Self : access Item;   Delimiter : in Character) return Text.items_1k;
   function Tokens      (Self : access Item;   Delimiter : in Character) return Text.items_8k;



   function get_Integer (Self : access Item) return Integer;
   --
   -- Skips whitespace and reads the next legal 'integer' value.
   -- Cursor is positioned at the next character following the integer.
   -- Raises no_data_Error if no legal integer exists.

   function get_Real (Self : access Item) return long_Float;
   --
   -- Skips whitespace and reads the next legal 'real' value.
   -- Cursor is positioned at the next character following the real.
   -- Raises no_data_Error if no legal real exists.



   at_end_Error  : exception;
   no_data_Error : exception;


   function Peek (Self : in item;   Length : in Natural) return String;



private

   type Item is tagged
      record
         Target  : Text_view;
         Current :        Natural       := 0;
      end record;

end lace.Text.Cursor;