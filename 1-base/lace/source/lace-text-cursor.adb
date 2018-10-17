with
     ada.Characters.latin_1,
     ada.Strings.fixed,
     ada.Strings.Maps;


package body lace.Text.Cursor
is
   use Ada.Strings;


   Integer_Numerals : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("+-0123456789");
   Float_Numerals   : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("+-0123456789.");


   -- Construction
   --

   function First (Self : in Text_view) return Cursor.item
   is
   begin
      declare
         the_Cursor : constant Cursor.item := (Self, 1);
      begin
         return the_Cursor;
      end;
   end First;


   -- Attributes
   --

   function At_End (Self : in item) return Boolean
   is
   begin
      return Self.Current = 0;
   end At_End;



   function has_Element (Self : in     Item) return Boolean
   is
   begin
      return not at_End (Self)
        and      Self.Current <= Self.Target.Length;
   end has_Element;



   function next_Token (Self : access Item;   Delimiter : in Character) return String
   is
   begin
      if self.Target.Data (self.Current) = Delimiter
      then
         self.Current := self.Current + 1;
         return "";
      else
         declare
            use Ada.Strings.fixed,  Ada.strings.Maps;

            First  : Positive := Self.Current;
            Last   : Natural  := 0;
         begin
            find_Token (self.Target.Data (First .. self.Target.Length),   to_Set (Delimiter),
                        Outside,
                        First, Last);

            self.Current := Last + 2;

            return self.Target.Data (First .. Last);
         end;
      end if;

   end next_Token;



   -- Search begins at current cursor position.
   -- Advances to the position immediately after Delimiter.
   -- Sets Iterator to 0 if Delimiter is not found.
   -- Search is carried out 'Repeat' times.

   procedure Advance (Self      : in out item;
                      Delimiter : in     String;
                      Repeat    : in     Positive := 1;
                      Skip_Delimiter : in     Boolean := True)
   is
   begin
      for Count in 1 .. Repeat
      loop
         declare
            Delimiter_Position : Natural;
         begin
            Delimiter_Position := Ada.Strings.Fixed.Index (self.target.data, Delimiter, from => self.Current);

            if Delimiter_Position = 0
            then
               Self.Current := 0;
               return;
            else
               if Skip_Delimiter
               then
                  Self.Current := Delimiter_Position + Delimiter'Length;

               elsif Count = Repeat
               then
                  Self.Current := Delimiter_Position - 1;

               else
                  Self.Current := Delimiter_Position + Delimiter'Length - 1;
               end if;
            end if;
         end;
      end loop;

   exception
      when Constraint_Error =>
         raise At_End_Error;
   end Advance;


   procedure skip_White (Self : in out Item)
   is
   begin
      while      has_Element (Self)
        and then (Self.Target.Data (Self.Current) = ' '
                  or  Self.Target.Data (Self.Current) = ada.Characters.Latin_1.CR
                  or  Self.Target.Data (Self.Current) = ada.Characters.Latin_1.LF
                  or  Self.Target.Data (Self.Current) = ada.Characters.Latin_1.HT)
      loop
         Self.Current := Self.Current + 1;
      end loop;
   end skip_White;


   function next_Token (Self      : access item;
                        Delimiter : in     String) return String
   is
      use Ada.Strings.Fixed;
   begin
      if At_End (Self.all)
      then
         raise At_End_Error;
      end if;

      declare
         Delimiter_Position : constant Natural := Index (self.Target.Data, Delimiter, from => self.Current);
      begin
         if Delimiter_Position = 0
         then
            return the_Token : constant String := self.Target.Data (self.Current .. self.Target.Length)
            do
               Self.Current := 0;
            end return;
         end if;

         return the_Token : constant String := self.Target.Data (self.Current .. Delimiter_Position - 1)
         do
            Self.Current := Delimiter_Position + Delimiter'Length;
         end return;
      end;
   end next_Token;



   procedure skip_Token  (Self : access Item;   Delimiter : in Character)
   is
      ignored_Token : String := self.next_Token (Delimiter);
   begin
      null;
   end skip_Token;



   procedure skip_Token  (Self : access Item;   Delimiter : in String)
   is
      ignored_Token : String := self.next_Token (Delimiter);
   begin
      null;
   end skip_Token;


   function Tokens (Self : access Item;   Delimiter : in Character) return Text.items_32
   is
      the_Tokens : Text.Items_32 (1 .. 1024);
      Count      : Natural                  := 0;
   begin
      while Self.has_Element loop
         Count              := Count + 1;
         the_Tokens (Count) := to_Text (self.next_Token (Delimiter), capacity => 32);
      end loop;

      return the_Tokens (1 .. Count);
   end Tokens;


   function Tokens (Self : access Item;   Delimiter : in Character) return Text.items_1k
   is
      the_Tokens : Text.Items_1k (1 .. 1024);
      Count      : Natural                  := 0;
   begin
      while Self.has_Element loop
         Count              := Count + 1;
         the_Tokens (Count) := to_Text (self.next_Token (Delimiter), capacity => 1024);
      end loop;

      return the_Tokens (1 .. Count);
   end Tokens;


   function Tokens (Self : access Item;   Delimiter : in Character) return Text.items_8k
   is
      the_Tokens : Text.Items_8k (1 .. 512);
      Count      : Natural                  := 0;
   begin
      while Self.has_Element loop
         Count              := Count + 1;

         declare
            Next : constant String := self.next_Token (Delimiter);
         begin
            the_Tokens (Count) := to_Text (Next, capacity => 8 * 1024);
         end;
      end loop;

      return the_Tokens (1 .. Count);
   end Tokens;


   function Get_Integer (Self : access item) return Integer
   is
      use Ada.Strings.Fixed;

      Text  : String (1 .. self.Length);
      First : Positive;
      Last  : Natural;
   begin
      Text := self.target.Data (self.Current .. self.target.Length);
      find_Token (Text, integer_Numerals, Inside, First, Last);

      if Last = 0 then
         raise No_Data_Error;
      end if;

      Self.Current := Self.Current + Last;

      return Integer'Value (Text (First .. Last));
   end get_Integer;



   function get_Real (Self : access item) return long_Float
   is
      use Ada.Strings.Fixed;

      Text  : String (1 .. self.Length);
      First : Positive;
      Last  : Natural;
   begin
      Text := self.target.Data (self.Current .. self.target.Length);
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
      return self.Target.Length - self.Current + 1;
   end Length;



   function Peek (Self : in item;   Length : in Natural) return String
   is
      Last : Natural := Self.Current + Length - 1;
   begin
      if at_End (Self)
      then
         return "";
      end if;

      Last := Natural'Min (Last, Self.Target.Length);

      return self.Target.Data (Self.Current .. Last);
   end Peek;

end lace.Text.Cursor;

