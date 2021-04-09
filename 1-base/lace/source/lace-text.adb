with
     lace.Strings.fixed,
     ada.Characters.Latin_1,
     ada.Characters.handling,
     ada.Strings.hash;

package body lace.Text
is
   -- Construction
   --

   function to_Text (From : in String;
                    Trim  : in Boolean := False) return Item
   is
   begin
      return to_Text (From, capacity => From'Length, trim => Trim);
   end to_Text;


   function to_Text (From     : in String;
                     Capacity : in Natural;
                     Trim     : in Boolean := False) return Item
   is
      the_String : constant String := (if Trim then lace.Strings.fixed.Trim (From, ada.Strings.Both) else From);
      Self       : Item (Capacity);
   begin
      Self.Length                  := the_String'Length;
      Self.Data (1 .. Self.Length) := the_String;

      return Self;
   end to_Text;


   -- Attributes
   --

   procedure String_is (Self : in out Item;
                        Now  : in     String)
   is
   begin
      Self.Data (1 .. Now'Length) := Now;
      Self.Length                 := Now'Length;
   end String_is;


   function to_String (Self : in Item) return String
   is
   begin
      return Self.Data (1 .. Self.Length);
   end to_String;


   function is_Empty (Self : in Item) return Boolean
   is
   begin
      return Self.Length = 0;
   end is_Empty;


   function Length (Self : in Item) return Natural
   is
   begin
      return Self.Length;
   end Length;



   function next_Token (Self : in Item;   Delimiter : in     Character;
                                          From      : in out Positive) return String
   is
      Cursor : Positive renames From;
   begin
      if Self.Data (Cursor) = Delimiter then
         Cursor := Cursor + 1;
         return "";
      else
         declare
            First : constant Positive := Cursor;
         begin
            loop
               Cursor := Cursor + 1;

               if Self.Data (Cursor) = Delimiter
               then
                  Cursor := Cursor + 1;
                  return Self.Data (First .. Cursor - 2);

               elsif Cursor = Self.Length
               then
                  Cursor := Cursor + 1;
                  return Self.Data (First .. Cursor - 1);
               end if;
            end loop;
         end;
      end if;
   end next_Token;


   function Lines (Self : in Item) return Text.items_1k
   is
      use ada.Characters.Latin_1;
   begin
      return Tokens (Self, LF);
   end Lines;


   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_1k
   is
      the_Tokens : Text.items_1k (1 .. 4 * 1024);
      Count      : Natural := 0;

      From : aliased Positive := 1;
   begin
      while From <= Self.Length
      loop
         Count              := Count + 1;
         the_Tokens (Count) := to_Text (next_Token (Self,
                                                    Delimiter,
                                                    From),
                                        capacity => 1024,
                                        trim     => Trim);
      end loop;

      if         Self.Length > 0
        and then Self.Data (Self.Length) = Delimiter
      then                                                      -- Handle case where final character is the delimiter.
         Count              := Count + 1;
         the_Tokens (Count) := to_Text ("", capacity => 1024);  -- Add an empty token.
      end if;

      return the_Tokens (1 .. Count);
   end Tokens;



   function Image (Self : in Item) return String
   is
   begin
      return
        Self.Capacity'Image  & " "  &
        Self.Length'Image    & " '" &
        to_String (Self)     & "'";
   end Image;



   function Hashed (Self : in Item) return ada.Containers.Hash_type
   is
   begin
      return ada.strings.Hash (Self.Data (1 .. Self.Length));
   end Hashed;


   overriding
   function "=" (Left, Right : in Item) return Boolean
   is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      return to_String (Left) = to_String (Right);
   end "=";


   function to_Lowercase (Self : in Item) return Item
   is
      use ada.Characters.handling;
      Result : Item := Self;
   begin
      for i in 1 .. Self.Length
      loop
         Result.Data (i) := to_Lower (Self.Data (i));
      end loop;

      return Result;
   end to_Lowercase;


   function mono_Spaced (Self : in Item) return Item
   is
      Result : Item (Self.Capacity);
      Prior  : Character := 'a';
      Length : Natural   := 0;
   begin
      for i in 1 .. Self.Length
      loop
         if    Self.Data (i) = ' '
           and Prior = ' '
         then
            null;
         else
            Length               := Length + 1;
            Result.Data (Length) := Self.Data (i);
            Prior                := Self.Data (i);
         end if;
      end loop;

      Result.Length := Length;
      return Result;
   end mono_Spaced;


   procedure replace (Self : in out Item;   Pattern : in String;
                                            By      : in String)
   is
      Result : Item (Self.Capacity);

      Cursor : Positive := 1;
      First  : Natural  := 1;
      Last   : Natural;
   begin
      loop
         Last := First + Pattern'Length - 1;

         if Last > Self.Length
         then
            Last := Self.Length;
         end if;

         if Self.Data (First .. Last) = Pattern
         then
            Result.Data (Cursor .. Cursor + By'Length - 1) := By;
            Cursor := Cursor + By'Length;
            First  := Last + 1;
         else
            Result.Data (Cursor) := Self.Data (First);
            Cursor := Cursor + 1;
            First  := First  + 1;
         end if;

         exit when First > Self.Length;
      end loop;

      Self.Length                  := Cursor - 1;
      Self.Data (1 .. Self.Length) := Result.Data (1 .. Self.Length);
   end replace;


   function replace (Self : in Item;   Pattern : in String;
                                       By      : in String) return Item
   is
      Text : lace.Text.item := Self;
   begin
      replace (Text, Pattern, By);
      return Text;
   end Replace;



   -- Streams
   --

   function Item_input  (Stream : access Ada.Streams.Root_Stream_Type'Class) return Item
   is
      Capacity : Positive;
      Length   : Natural;
   begin
      Positive'read (Stream, Capacity);
      Natural 'read (Stream, Length);

      declare
         Data : String (1 .. Capacity);
      begin
         String'read (Stream, Data (1 .. Length));

         return (capacity => Capacity,
                 data     => Data,
                 length   => Length);
      end;
   end Item_input;



   procedure Item_output (Stream   : access Ada.Streams.Root_Stream_Type'Class;
                          the_Item : in     Item)
   is
   begin
      Positive'write (Stream, the_Item.Capacity);
      Natural 'write (Stream, the_Item.Length);
      String  'write (Stream, the_Item.Data (1 .. the_Item.Length));
   end Item_output;



   procedure Write (Stream : access ada.streams.Root_Stream_Type'Class;
                    Self   : in     Item)
   is
   begin
      Natural'write (Stream, Self.Length);
      String 'write (Stream, Self.Data (1 .. Self.Length));
   end Write;



   procedure Read (Stream : access ada.streams.Root_Stream_Type'Class;
                   Self   :    out Item)
   is
   begin
      Natural'read (Stream, Self.Length);
      String 'read (Stream, Self.Data (1 .. Self.Length));
   end Read;

end lace.Text;
