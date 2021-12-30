with
     lace.Text.all_Tokens,
     ada.Characters.latin_1;


package body lace.Text.all_Lines
is
   use lace.Text.all_Tokens,
       ada.Characters.latin_1;


   function Lines (Self : in Item) return Text.items_2
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_4
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_8
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_16
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_32
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_64
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_128
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_256
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_512
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_1k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_2k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_4k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_8k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_16k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_32k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_64k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_128k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_256k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


   function Lines (Self : in Item) return Text.items_512k
   is
   begin
      return Tokens (Self, LF);
   end Lines;


end lace.Text.all_Lines;
