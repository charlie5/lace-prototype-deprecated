package body lace.fast_Pool
is

   type Views is array (1 .. pool_Size) of View;


   protected Pool
   is
      entry new_Item (the_Item :    out View);
      entry free     (the_Item : in     View);
   private
      Available : Views;
      Count     : Natural := 0;
   end Pool;


   protected body Pool
   is
      entry new_Item (the_Item : out View)
        when True
      is
      begin
         if Count = 0
         then
            the_Item := new Item;
         else
            the_Item := Available (Count);
            Count     := Count - 1;
         end if;
      end new_Item;


      entry free (the_Item : in View)
        when True
      is
      begin
         Count             := Count + 1;
         Available (Count) := the_Item;
      end free;
   end Pool;



   function new_Item return View
   is
      Self : View;
   begin
      Pool.new_Item (Self);
      return Self;
   end new_Item;



   procedure free (Self : in out View)
   is
   begin
      Pool.free (Self);
      Self := null;
   end free;

end lace.fast_Pool;
