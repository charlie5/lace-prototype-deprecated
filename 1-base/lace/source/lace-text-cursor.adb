with
     ada.Characters.latin_1,
     ada.Strings.fixed,
     ada.strings.Maps;


package body lace.text.Cursor
is
   use ada.Strings;

   Integer_Numerals : constant maps.character_Set := maps.to_Set ("+-0123456789");
   Float_Numerals   : constant maps.character_Set := maps.to_Set ("+-0123456789.");


   -- Forge
   --

   function First (Self : in Text_view) return Cursor.item
   is
      the_Cursor : constant Cursor.item := (Self, 1);
   begin
      return the_Cursor;
   end First;


   -- Attributes
   --

   function at_End (Self : in Item) return Boolean
   is
   begin
      return Self.Current = 0;
   end at_End;



   function has_Element (Self : in Item) return Boolean
   is
   begin
      return not at_End (Self)
        and      Self.Current <= Self.Target.Length;
   end has_Element;



   function next_Token (Self : in out Item;   Delimiter : in Character) return String
   is
   begin
      if Self.Target.Data (Self.Current) = Delimiter
      then
         Self.Current := Self.Current + 1;
         return "";
      else
         declare
            use ada.Strings.fixed,
                ada.strings.Maps;

            First : Positive := Self.Current;
            Last  : Natural  := 0;
         begin
            find_Token (Self.Target.Data (First .. Self.Target.Length),
                        to_Set (Delimiter),
                        Outside,
                        First,
                        Last);

            Self.Current := Last + 2;

            return Self.Target.Data (First .. Last);
         end;
      end if;
   end next_Token;



   procedure advance (Self           : in out Item;
                      Delimiter      : in     String;
                      Repeat         : in     Positive := 1;
                      skip_Delimiter : in     Boolean  := True)
   is
   begin
      for Count in 1 .. Repeat
      loop
         declare
            delimiter_Position : Natural;
         begin
            delimiter_Position := fixed.Index (Self.Target.Data,
                                               Delimiter,
                                               from => Self.Current);
            if delimiter_Position = 0
            then
               Self.Current := 0;
               return;
            else
               if skip_Delimiter
               then
                  Self.Current := delimiter_Position + Delimiter'Length;

               elsif Count = Repeat
               then
                  Self.Current := delimiter_Position - 1;

               else
                  Self.Current := delimiter_Position + Delimiter'Length - 1;
               end if;
            end if;
         end;
      end loop;

   exception
      when constraint_Error =>
         raise at_end_Error;
   end advance;



   procedure skip_White (Self : in out Item)
   is
   begin
      while      has_Element (Self)
        and then (    Self.Target.Data (Self.Current) = ' '
                  or  Self.Target.Data (Self.Current) = ada.Characters.Latin_1.CR
                  or  Self.Target.Data (Self.Current) = ada.Characters.Latin_1.LF
                  or  Self.Target.Data (Self.Current) = ada.Characters.Latin_1.HT)
      loop
         Self.Current := Self.Current + 1;
      end loop;
   end skip_White;



   function next_Token (Self      : in out Item;
                        Delimiter : in     String) return String
   is
   begin
      if at_End (Self)
      then
         raise at_end_Error;
      end if;

      declare
         use ada.Strings.fixed;
         delimiter_Position : constant Natural := Index (Self.Target.Data, Delimiter, from => Self.Current);
      begin
         if delimiter_Position = 0
         then
            return the_Token : constant String := Self.Target.Data (Self.Current .. Self.Target.Length)
            do
               Self.Current := 0;
            end return;
         end if;

         return the_Token : constant String := Self.Target.Data (Self.Current .. delimiter_Position - 1)
         do
            Self.Current := delimiter_Position + Delimiter'Length;
         end return;
      end;
   end next_Token;



   procedure skip_Token (Self : in out Item;   Delimiter : in Character)
   is
      ignored_Token : String := Self.next_Token (Delimiter);
   begin
      null;
   end skip_Token;



   procedure skip_Token (Self : in out Item;   Delimiter : in String)
   is
      ignored_Token : String := Self.next_Token (Delimiter);
   begin
      null;
   end skip_Token;



   function Tokens (Self : in out Item;   Delimiter : in String) return Text.items_32
   is
      the_Tokens : Text.items_32 (1 .. 1024);
      Count      : Natural := 0;
   begin
      while Self.has_Element loop
         Count              := Count + 1;
         the_Tokens (Count) := to_Text (Self.next_Token (Delimiter), capacity => 32);
      end loop;

      return the_Tokens (1 .. Count);
   end Tokens;



   function Tokens (Self : in out Item;   Delimiter : in String) return Text.items_1k
   is
      the_Tokens : Text.items_1k (1 .. 1024);
      Count      : Natural := 0;
   begin
      while Self.has_Element
      loop
         Count              := Count + 1;
         the_Tokens (Count) := to_Text (Self.next_Token (Delimiter), capacity => 1024);
      end loop;

      return the_Tokens (1 .. Count);
   end Tokens;



   function Tokens (Self : in out Item;   Delimiter : in String) return Text.items_8k
   is
      the_Tokens : Text.items_8k (1 .. 512);
      Count      : Natural := 0;
   begin
      while Self.has_Element
      loop
         Count := Count + 1;

         declare
            Next : constant String := Self.next_Token (Delimiter);
         begin
            the_Tokens (Count) := to_Text (Next, capacity => 8 * 1024);
         end;
      end loop;

      return the_Tokens (1 .. Count);
   end Tokens;



   function get_Integer (Self : in out Item) return Integer
   is
      use ada.Strings.fixed;

      Text  : String (1 .. Self.Length);
      First : Positive;
      Last  : Natural;
   begin
      Text := Self.Target.Data (Self.Current .. Self.Target.Length);
      find_Token (Text, integer_Numerals, Inside, First, Last);

      if Last = 0 then
         raise No_Data_Error;
      end if;

      Self.Current := Self.Current + Last;

      return Integer'Value (Text (First .. Last));
   end get_Integer;



   function get_Real (Self : in out Item) return long_Float
   is
      use ada.Strings.fixed;

      Text  : String (1 .. Self.Length);
      First : Positive;
      Last  : Natural;
   begin
      Text := Self.Target.Data (Self.Current .. Self.Target.Length);
      find_Token (Text, float_Numerals, Inside, First, Last);

      if Last = 0 then
         raise No_Data_Error;
      end if;

      Self.Current := Self.Current + Last;

      return long_Float'Value (Text (First .. Last));
   end get_Real;



   function Length (Self : in Item) return Natural
   is
   begin
      return Self.Target.Length - Self.Current + 1;
   end Length;



   function Peek (Self : in Item;   Length : in Natural) return String
   is
      Last : Natural := Self.Current + Length - 1;
   begin
      if at_End (Self)
      then
         return "";
      end if;

      Last := Natural'Min (Last, Self.Target.Length);

      return Self.Target.Data (Self.Current .. Last);
   end Peek;

end lace.text.Cursor;
