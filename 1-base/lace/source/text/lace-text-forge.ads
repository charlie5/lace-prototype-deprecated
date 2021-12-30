
package lace.Text.forge
--
-- Provides constructors for Text.
--
is
   --------
   -- Files
   --

   type Filename is new String;

   function to_String (Filename : in forge.Filename) return String;
   function to_Text   (Filename : in forge.Filename) return Item;


   --------------
   -- Stock Items
   --

   function to_Text_2 (From : in String)       return Item_2;
   function to_Text_2 (From : in Text.item)    return Item_2;

   function to_Text_4 (From : in String)       return Item_4;
   function to_Text_4 (From : in Text.item)    return Item_4;

   function to_Text_8 (From : in String)       return Item_8;
   function to_Text_8 (From : in Text.item)    return Item_8;

   function to_Text_16 (From : in String)      return Item_16;
   function to_Text_16 (From : in Text.item)   return Item_16;

   function to_Text_32 (From : in String)      return Item_32;
   function to_Text_32 (From : in Text.item)   return Item_32;

   function to_Text_64 (From : in String)      return Item_64;
   function to_Text_64 (From : in Text.item)   return Item_64;

   function to_Text_128 (From : in String)     return Item_128;
   function to_Text_128 (From : in Text.item)  return Item_128;

   function to_Text_256 (From : in String)     return Item_256;
   function to_Text_256 (From : in Text.item)  return Item_256;

   function to_Text_512 (From : in String)     return Item_512;
   function to_Text_512 (From : in Text.item)  return Item_512;

   function to_Text_1k (From : in String)      return Item_1k;
   function to_Text_1k (From : in Text.item)   return Item_1k;

   function to_Text_2k (From : in String)      return Item_2k;
   function to_Text_2k (From : in Text.item)   return Item_2k;

   function to_Text_4k (From : in String)      return Item_4k;
   function to_Text_4k (From : in Text.item)   return Item_4k;

   function to_Text_8k (From : in String)      return Item_8k;
   function to_Text_8k (From : in Text.item)   return Item_8k;

   function to_Text_16k (From : in String)     return Item_16k;
   function to_Text_16k (From : in Text.item)  return Item_16k;

   function to_Text_32k (From : in String)     return Item_32k;
   function to_Text_32k (From : in Text.item)  return Item_32k;

   function to_Text_64k (From : in String)     return Item_64k;
   function to_Text_64k (From : in Text.item)  return Item_64k;

   function to_Text_128k (From : in String)    return Item_128k;
   function to_Text_128k (From : in Text.item) return Item_128k;

   function to_Text_256k (From : in String)    return Item_256k;
   function to_Text_256k (From : in Text.item) return Item_256k;

   function to_Text_512k (From : in String)    return Item_512k;
   function to_Text_512k (From : in Text.item) return Item_512k;

   function to_Text_1m (From : in String)      return Item_1m;
   function to_Text_1m (From : in Text.item)   return Item_1m;

   function to_Text_2m (From : in String)      return Item_2m;
   function to_Text_2m (From : in Text.item)   return Item_2m;

   function to_Text_4m (From : in String)      return Item_4m;
   function to_Text_4m (From : in Text.item)   return Item_4m;

   function to_Text_8m (From : in String)      return Item_8m;
   function to_Text_8m (From : in Text.item)   return Item_8m;

   function to_Text_16m (From : in String)     return Item_16m;
   function to_Text_16m (From : in Text.item)  return Item_16m;

   function to_Text_32m (From : in String)     return Item_32m;
   function to_Text_32m (From : in Text.item)  return Item_32m;

   function to_Text_64m (From : in String)     return Item_64m;
   function to_Text_64m (From : in Text.item)  return Item_64m;

   function to_Text_128m (From : in String)    return Item_128m;
   function to_Text_128m (From : in Text.item) return Item_128m;

   function to_Text_256m (From : in String)    return Item_256m;
   function to_Text_256m (From : in Text.item) return Item_256m;

   function to_Text_512m (From : in String)    return Item_512m;
   function to_Text_512m (From : in Text.item) return Item_512m;

end lace.Text.forge;
