with
     lace.Text.Cursor;

package body lace.Text.utility
is

   function replace (Self : in Text.item;   Pattern : in String;
                                            By      : in String) return Text.item
   is
      use lace.Text.Cursor;
      Cursor     :          Text.Cursor.item := First (Self'Unrestricted_Access);
      the_Tokens : constant Text.items_1k    := lace.Text.Cursor.Tokens (Cursor, delimiter => Pattern);
      Size       :          Natural          := 0;

   begin
      for Each of the_Tokens
      loop
         Size := Size + Each.Length;
      end loop;

      Size := Size + (the_Tokens'Length - 1) * By'Length;

      declare
         First  : Positive := 1;
         Last   : Natural;
         Result : Text.item (Capacity => Natural'Max (Size, Self.Capacity));
      begin
         for Each of the_Tokens
         loop
            Last                        := First + Each.Length - 1;
            Result.Data (First .. Last) := Each.Data (1 .. Each.Length);

            exit when Last = Size;

            First                       := Last  + 1;
            Last                        := First + By'Length - 1;
            Result.Data (First .. Last) := By;

            First := Last + 1;
         end loop;

         Result.Length := Size;

         return Result;
      end;
   end replace;


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

   exception
      when Constraint_Error =>
         raise Text.Error with "'replace' fails ~ insufficient capacity";
   end replace;


end lace.Text.utility;
