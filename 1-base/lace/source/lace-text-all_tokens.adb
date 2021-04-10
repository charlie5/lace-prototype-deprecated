package body lace.Text.all_Tokens
is

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



   generic
      Text_Capacity : Positive;
      type Component  is private;
      type Array_type is array (Positive range <>) of Component;

      with function any_to_Text (From : in String;   Capacity : in Natural;
                                                     Trim     : in Boolean := False) return Component;

   function any_Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                          Trim      : in Boolean   := False) return Array_Type;



   function any_Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                          Trim      : in Boolean   := False) return Array_Type
   is
      max_Size   : constant          := 4 * 1024;
      the_Tokens :          Array_type (1 .. max_Size);
      Count      :          Natural  := 0;
      From       : aliased  Positive := 1;
   begin
      while From <= Self.Length
      loop
         Count              := Count + 1;
         the_Tokens (Count) := any_to_Text (next_Token (Self,
                                                        Delimiter,
                                                        From),
                                            capacity => Text_Capacity,
                                            trim     => Trim);
      end loop;

      if         Self.Length > 0
        and then Self.Data (Self.Length) = Delimiter
      then                                                      -- Handle case where final character is the delimiter.
         Count              := Count + 1;
         the_Tokens (Count) := any_to_Text ("", capacity => Text_Capacity);  -- Add an empty token.
      end if;

      return the_Tokens (1 .. Count);
   end any_Tokens;



   function Tokens_2 is new any_Tokens (Text_Capacity => 2,
                                        Component     => Text.item_2,
                                        Array_type    => Text.items_2,
                                        any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_2
     renames Tokens_2;



   function Tokens_4 is new any_Tokens (Text_Capacity => 4,
                                        Component     => Text.item_4,
                                        Array_type    => Text.items_4,
                                        any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_4
     renames Tokens_4;



   function Tokens_8 is new any_Tokens (Text_Capacity => 8,
                                        Component     => Text.item_8,
                                        Array_type    => Text.items_8,
                                        any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_8
     renames Tokens_8;



   function Tokens_16 is new any_Tokens (Text_Capacity => 16,
                                         Component     => Text.item_16,
                                         Array_type    => Text.items_16,
                                         any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_16
     renames Tokens_16;



   function Tokens_32 is new any_Tokens (Text_Capacity => 32,
                                         Component     => Text.item_32,
                                         Array_type    => Text.items_32,
                                         any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_32
     renames Tokens_32;



   function Tokens_64 is new any_Tokens (Text_Capacity => 64,
                                         Component     => Text.item_64,
                                         Array_type    => Text.items_64,
                                         any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_64
     renames Tokens_64;



   function Tokens_128 is new any_Tokens (Text_Capacity => 128,
                                          Component     => Text.item_128,
                                          Array_type    => Text.items_128,
                                          any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_128
     renames Tokens_128;



   function Tokens_256 is new any_Tokens (Text_Capacity => 256,
                                          Component     => Text.item_256,
                                          Array_type    => Text.items_256,
                                          any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_256
     renames Tokens_256;



   function Tokens_512 is new any_Tokens (Text_Capacity => 512,
                                          Component     => Text.item_512,
                                          Array_type    => Text.items_512,
                                          any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_512
     renames Tokens_512;



   function Tokens_1k is new any_Tokens (Text_Capacity => 1024,
                                         Component     => Text.item_1k,
                                         Array_type    => Text.items_1k,
                                         any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_1k
     renames Tokens_1k;



   function Tokens_2k is new any_Tokens (Text_Capacity => 2 * 1024,
                                         Component     => Text.item_2k,
                                         Array_type    => Text.items_2k,
                                         any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_2k
     renames Tokens_2k;



   function Tokens_4k is new any_Tokens (Text_Capacity => 4 * 1024,
                                         Component     => Text.item_4k,
                                         Array_type    => Text.items_4k,
                                         any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_4k
     renames Tokens_4k;



   function Tokens_8k is new any_Tokens (Text_Capacity => 8 * 1024,
                                         Component     => Text.item_8k,
                                         Array_type    => Text.items_8k,
                                         any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_8k
     renames Tokens_8k;



   function Tokens_16k is new any_Tokens (Text_Capacity => 16 * 1024,
                                          Component     => Text.item_16k,
                                          Array_type    => Text.items_16k,
                                          any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_16k
     renames Tokens_16k;



   function Tokens_32k is new any_Tokens (Text_Capacity => 32 * 1024,
                                          Component     => Text.item_32k,
                                          Array_type    => Text.items_32k,
                                          any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_32k
     renames Tokens_32k;



   function Tokens_64k is new any_Tokens (Text_Capacity => 64 * 1024,
                                          Component     => Text.item_64k,
                                          Array_type    => Text.items_64k,
                                          any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_64k
     renames Tokens_64k;



   function Tokens_128k is new any_Tokens (Text_Capacity => 128 * 1024,
                                           Component     => Text.item_128k,
                                           Array_type    => Text.items_128k,
                                           any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_128k
     renames Tokens_128k;



   function Tokens_256k is new any_Tokens (Text_Capacity => 256 * 1024,
                                           Component     => Text.item_256k,
                                           Array_type    => Text.items_256k,
                                           any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_256k
     renames Tokens_256k;



   function Tokens_512k is new any_Tokens (Text_Capacity => 512,
                                           Component     => Text.item_512k,
                                           Array_type    => Text.items_512k,
                                           any_to_Text   => to_Text);

   function Tokens (Self : in Item;   Delimiter : in Character := ' ';
                                      Trim      : in Boolean   := False) return Text.items_512k
     renames Tokens_512k;


end lace.Text.all_Tokens;
