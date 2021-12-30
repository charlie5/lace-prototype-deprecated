with
     ada.Containers,
     ada.Streams;


package lace.Text
--
-- Models a string of text characters.
--
is
   pragma Pure;

   type Item (Capacity : Natural) is private;

   function Image (Self : in Item) return String;

   Error : exception;


   --------------
   -- Stock Items
   --

   subtype Item_2   is Item (Capacity =>   2);
   subtype Item_4   is Item (Capacity =>   4);
   subtype Item_8   is Item (Capacity =>   8);
   subtype Item_16  is Item (Capacity =>  16);
   subtype Item_32  is Item (Capacity =>  32);
   subtype Item_64  is Item (Capacity =>  64);
   subtype Item_128 is Item (Capacity => 128);
   subtype Item_256 is Item (Capacity => 256);
   subtype Item_512 is Item (Capacity => 512);

   subtype Item_1k   is Item (Capacity =>       1024);
   subtype Item_2k   is Item (Capacity =>   2 * 1024);
   subtype Item_4k   is Item (Capacity =>   4 * 1024);
   subtype Item_8k   is Item (Capacity =>   8 * 1024);
   subtype Item_16k  is Item (Capacity =>  16 * 1024);
   subtype Item_32k  is Item (Capacity =>  32 * 1024);
   subtype Item_64k  is Item (Capacity =>  64 * 1024);
   subtype Item_128k is Item (Capacity => 128 * 1024);
   subtype Item_256k is Item (Capacity => 256 * 1024);
   subtype Item_512k is Item (Capacity => 512 * 1024);

   subtype Item_1m   is Item (Capacity =>       1024 * 1024);
   subtype Item_2m   is Item (Capacity =>   2 * 1024 * 1024);
   subtype Item_4m   is Item (Capacity =>   4 * 1024 * 1024);
   subtype Item_8m   is Item (Capacity =>   8 * 1024 * 1024);
   subtype Item_16m  is Item (Capacity =>  16 * 1024 * 1024);
   subtype Item_32m  is Item (Capacity =>  32 * 1024 * 1024);
   subtype Item_64m  is Item (Capacity =>  64 * 1024 * 1024);
   subtype Item_128m is Item (Capacity => 128 * 1024 * 1024);
   subtype Item_256m is Item (Capacity => 256 * 1024 * 1024);
   subtype Item_512m is Item (Capacity => 512 * 1024 * 1024);


   ---------------
   -- Stock Arrays
   --

   type Items_2   is array (Positive range <>) of aliased Item_2;
   type Items_4   is array (Positive range <>) of aliased Item_4;
   type Items_8   is array (Positive range <>) of aliased Item_8;
   type Items_16  is array (Positive range <>) of aliased Item_16;
   type Items_32  is array (Positive range <>) of aliased Item_32;
   type Items_64  is array (Positive range <>) of aliased Item_64;
   type Items_128 is array (Positive range <>) of aliased Item_128;
   type Items_256 is array (Positive range <>) of aliased Item_256;
   type Items_512 is array (Positive range <>) of aliased Item_512;

   type Items_1k   is array (Positive range <>) of aliased Item_1k;
   type Items_2k   is array (Positive range <>) of aliased Item_2k;
   type Items_4k   is array (Positive range <>) of aliased Item_4k;
   type Items_8k   is array (Positive range <>) of aliased Item_8k;
   type Items_16k  is array (Positive range <>) of aliased Item_16k;
   type Items_32k  is array (Positive range <>) of aliased Item_32k;
   type Items_64k  is array (Positive range <>) of aliased Item_64k;
   type Items_128k is array (Positive range <>) of aliased Item_128k;
   type Items_256k is array (Positive range <>) of aliased Item_256k;
   type Items_512k is array (Positive range <>) of aliased Item_512k;

   type Items_1m   is array (Positive range <>) of aliased Item_1m;
   type Items_2m   is array (Positive range <>) of aliased Item_2m;
   type Items_4m   is array (Positive range <>) of aliased Item_4m;
   type Items_8m   is array (Positive range <>) of aliased Item_8m;
   type Items_16m  is array (Positive range <>) of aliased Item_16m;
   type Items_32m  is array (Positive range <>) of aliased Item_32m;
   type Items_64m  is array (Positive range <>) of aliased Item_64m;
   type Items_128m is array (Positive range <>) of aliased Item_128m;
   type Items_256m is array (Positive range <>) of aliased Item_256m;
   type Items_512m is array (Positive range <>) of aliased Item_512m;


   ---------------
   -- Construction
   --

   function to_Text (From     : in String;
                     Trim     : in Boolean := False) return Item;

   function to_Text (From     : in String;
                     Capacity : in Natural;
                     Trim     : in Boolean := False) return Item;

   function "+"     (From     : in String)           return Item;


   -------------
   -- Attributes
   --

   procedure String_is (Self : in out Item;   Now : in String);
   function  to_String (Self : in     Item)     return String;
   function  "+"       (Self : in     Item)     return String renames to_String;

   function  is_Empty  (Self : in Item) return Boolean;
   function  Length    (Self : in Item) return Natural;
   function  Hashed    (Self : in Item) return ada.Containers.Hash_type;

   overriding
   function  "=" (Left, Right : in Item) return Boolean;

   function  to_Lowercase (Self : in Item) return Item;
   function  mono_Spaced  (Self : in Item) return Item;



private

   type Item (Capacity : Natural) is
      record
         Length : Natural := 0;
         Data   : String (1 .. Capacity);
      end record;


   ----------
   -- Streams
   --

   function  Item_input  (Stream : access ada.Streams.root_Stream_type'Class)              return Item;
   procedure Item_output (Stream : access ada.Streams.root_Stream_type'Class;   the_Item : in     Item);

   procedure read  (Stream : access ada.Streams.root_Stream_type'Class;   Self :    out Item);
   procedure write (Stream : access ada.Streams.root_Stream_type'Class;   Self : in     Item);

   for Item'input  use Item_input;
   for Item'output use Item_output;

   for Item'write  use write;
   for Item'read   use read;

end lace.Text;
