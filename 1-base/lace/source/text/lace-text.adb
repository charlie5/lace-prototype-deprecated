with
     lace.Strings.fixed,
     ada.Characters.handling,
     ada.Strings.Hash;


package body lace.Text
is
   ---------------
   -- Construction
   --

   function to_Text (From : in String;
                     Trim : in Boolean := False) return Item
   is
   begin
      return to_Text (From,
                      Capacity => From'Length,
                      Trim     => Trim);
   end to_Text;



   function to_Text (From     : in String;
                     Capacity : in Natural;
                     Trim     : in Boolean := False) return Item
   is
      the_String : constant String := (if Trim then lace.Strings.fixed.Trim (From, ada.Strings.Both)
                                               else From);
      Self       : Item (Capacity);
   begin
      Self.Length                  := the_String'Length;
      Self.Data (1 .. Self.Length) := the_String;

      return Self;
   end to_Text;



   function "+" (From : in String) return Item
   is
   begin
      return to_Text (From);
   end "+";


   -------------
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



   function Image (Self : in Item) return String
   is
   begin
      return
        "(Capacity =>"  & Self.Capacity'Image  & "," &
        " Length =>"    & Self.Length  'Image  & "," &
        " Data => '"    & to_String (Self)     & "')";
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
      if Left.Length /= Right.Length
      then
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


   ----------
   -- Streams
   --

   function Item_input (Stream : access ada.Streams.root_Stream_type'Class) return Item
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

         return (Capacity => Capacity,
                 Data     => Data,
                 Length   => Length);
      end;
   end Item_input;



   procedure Item_output (Stream   : access ada.Streams.root_Stream_type'Class;
                          the_Item : in     Item)
   is
   begin
      Positive'write (Stream, the_Item.Capacity);
      Natural 'write (Stream, the_Item.Length);
      String  'write (Stream, the_Item.Data (1 .. the_Item.Length));
   end Item_output;



   procedure Write (Stream : access ada.Streams.root_Stream_type'Class;
                    Self   : in     Item)
   is
   begin
      Natural'write (Stream, Self.Length);
      String 'write (Stream, Self.Data (1 .. Self.Length));
   end Write;



   procedure Read (Stream : access ada.Streams.root_Stream_type'Class;
                   Self   :    out Item)
   is
   begin
      Natural'read (Stream, Self.Length);
      String 'read (Stream, Self.Data (1 .. Self.Length));
   end Read;


end lace.Text;
