generic
   type Item is private;
   type View is access all Item;

   pool_Size : Positive := 5_000;

package lace.fast_Pool
is

   function  new_Item     return View;
   procedure free (Self : in out View);

end lace.fast_Pool;
