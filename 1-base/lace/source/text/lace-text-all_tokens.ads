package lace.Text.all_Tokens
is
   default_Max : constant := 4 * 1024;


   ----------------------
   -- Character Delimiter
   --
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_2;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_4;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_8;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_16;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_32;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_64;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_128;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_256;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_512;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_1k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_2k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_4k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_8k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_16k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_32k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_64k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_128k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_256k;
   function Tokens (Self : in Item;   Delimiter  : in Character := ' ';
                                      Trim       : in Boolean   := False;
                                      max_Tokens : in Positive  := default_Max) return Text.items_512k;

   -------------------
   -- String Delimiter
   --
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_2;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_4;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_8;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_16;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_32;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_64;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_128;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_256;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_512;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_1k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_2k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_4k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_8k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_16k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_32k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_64k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_128k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_256k;
   function Tokens (Self : in Item;   Delimiter  : in String;
                                      Trim       : in Boolean  := False;
                                      max_Tokens : in Positive := default_Max) return Text.items_512k;

end lace.Text.all_Tokens;
