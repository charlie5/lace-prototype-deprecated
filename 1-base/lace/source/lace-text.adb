with
     ada.Strings.hash;


package body lace.Text
is

   -- Construction
   --


   function to_Text (From : in String;   Capacity : in Natural) return Item
   is
      Self : Item (Capacity);
   begin
      Self.Data (1 .. From'Length) := From;
      self.Length                  := From'Length;
      return Self;
   end to_Text;


   function to_Text (From : in String) return Item
   is
   begin

      return to_Text (From, capacity => From'Length);
   end to_Text;


   function to_Text_8 (From : in String) return Item
   is
   begin
      return to_Text (From, capacity => 8);
   end to_Text_8;


   function to_Text_8 (From : in Text.item) return Item
   is
   begin
      return to_Text (to_String (From),  capacity => 8);
   end to_Text_8;


   function to_Text_16 (From : in String) return Item
   is
   begin
      return to_Text (From, capacity => 16);
   end to_Text_16;


   function to_Text_16 (From : in Text.item) return Item
   is
   begin
      return to_Text (to_String (From),  capacity => 16);
   end to_Text_16;


   function to_Text_32 (From : in String) return Item
   is
   begin
      return to_Text (From, capacity => 32);
   end to_Text_32;


   function to_Text_32 (From : in Text.item) return Item
   is
   begin
      return to_Text (to_String (From),  capacity => 32);
   end to_Text_32;


   function to_Text_64 (From : in String) return Item
   is
   begin
      return to_Text (From, capacity => 64);
   end to_Text_64;


   function to_Text_64 (From : in Text.item) return Item
   is
   begin
      return to_Text (to_String (From),  capacity => 64);
   end to_Text_64;


   function to_Text_128 (From : in String) return Item
   is
   begin
      return to_Text (From, capacity => 128);
   end to_Text_128;


   function to_Text_128 (From : in Text.item) return Item
   is
   begin
      return to_Text (to_String (From),  capacity => 128);
   end to_Text_128;



   -- Attributes
   --

   procedure String_is (Self : in out Item;   Now : in String)
   is
   begin
      self.Data (1 .. Now'Length) := Now;
      self.Length                 := Now'Length;
   end String_is;


   function to_String (Self : in     Item) return String
   is
   begin
      return self.Data (1 .. self.Length);
   end to_String;


   function is_Empty (Self : in Item) return Boolean
   is
   begin
      return self.Length = 0;
   end is_Empty;


   function Length   (Self : in Item) return Natural
   is
   begin
      return self.Length;
   end Length;



   -- Tokens
   --

   function next_Token (Self : in Item;   Delimiter : in     Character;
                                          From      : access Positive) return String
   is
      self_Current : Positive renames From.all;
   begin
      if self.Data (self_Current) = Delimiter then
         self_Current := self_Current + 1;
         return "";
      else
         declare
            First  : constant Positive := From.all;
         begin
            loop
               self_Current := self_Current + 1;
               if self.Data (self_Current) = Delimiter then
                  self_Current := self_Current + 1;
                  return self.Data (First .. self_Current - 2);
               elsif self_Current = self.Length then
                  self_Current := self_Current + 1;
                  return self.Data (First .. self_Current - 1);
               end if;
            end loop;
         end;
      end if;
   end next_Token;


   function Tokens (Self : in Item;   Delimiter : in Character) return Text.items_1k
   is
      the_Tokens :         Text.Items_1k (1 .. 2 * 1024);
      Count      :         Natural                  := 0;

      From       : aliased Positive                 := 1;
   begin
      while From <= Self.Length loop
         Count              := Count + 1;
         the_Tokens (Count) := to_Text (next_Token (Self, Delimiter, From'access), capacity => 1024);
      end loop;

      if         self.Length > 0
        and then self.Data (self.Length) = Delimiter
      then                                                      -- handle case where final character is the terminator.
         Count              := Count + 1;
         the_Tokens (Count) := to_Text ("", capacity => 1024);  -- add an empty token.
      end if;

      return the_Tokens (1 .. Count);
   end Tokens;


   function Image (Self : in Item) return String
   is
   begin
      return to_String (Self);
   end Image;



   -- Support
   --

   function Hashed (Self : in Item) return ada.containers.hash_type
   is
   begin
      return ada.strings.hash (self.Data (1 .. self.Length));
   end Hashed;


   overriding function "=" (Left, Right : in Item) return Boolean
   is
   begin
      if left.Length /= right.Length then
         return False;
      end if;

      return to_String (left) = to_String (right);
   end "=";



   function  Item_input  (Stream : access Ada.Streams.Root_Stream_Type'Class) return Item
   is
      Capacity : Positive;
      Length   : Natural;
   begin
      Positive'read (Stream, Capacity);
      Natural'read  (Stream, Length);

      declare
         Data : String (1 .. Capacity);
      begin
         String'read (Stream, Data (1 .. Length));

         return (capacity => Capacity,
                 data     => Data,
                 length   => Length);
      end;
   end Item_input;


   procedure Item_output (Stream : access Ada.Streams.Root_Stream_Type'Class;   the_Item : in     Item)
   is
   begin
      Positive'write (Stream, the_Item.Capacity);
      Natural'write  (Stream, the_Item.Length);
      String'write   (Stream, the_Item.Data (1 .. the_Item.Length));
   end Item_output;



   procedure Write (Stream : access ada.streams.Root_Stream_Type'Class;   Self : in Item)
   is
   begin
      Natural'write  (Stream, Self.Length);
      String'write   (Stream, Self.Data (1 .. Self.Length));
   end Write;


   procedure Read (Stream : access ada.streams.Root_Stream_Type'Class;    Self : out Item)
   is
   begin
      Natural'read  (Stream, Self.Length);
      String'read   (Stream, Self.Data (1 .. Self.Length));
   end Read;

end lace.Text;
