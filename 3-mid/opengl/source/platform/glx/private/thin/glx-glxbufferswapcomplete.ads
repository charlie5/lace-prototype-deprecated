with
     Interfaces.C,
     System;


package glx.GLXBufferSwapComplete
is

   type Item is record
      the_type   : aliased Interfaces.C.int;
      serial     : aliased Interfaces.C.unsigned_long;
      send_event : aliased glx.Bool;
      display    :         System.Address;
      drawable   : aliased glx.Drawable;
      event_type : aliased Interfaces.C.int;
      ust        : aliased Interfaces.Integer_64;
      msc        : aliased Interfaces.Integer_64;
      sbc        : aliased Interfaces.Integer_64;
   end record;

   type Items is
     array (Interfaces.C.size_t range <>)
            of aliased glx.GLXBufferSwapComplete.Item;


   type Pointer is access all glx.GLXBufferSwapComplete.Item;

   type Pointers is
     array (Interfaces.C.size_t range <>)
            of aliased glx.GLXBufferSwapComplete.Pointer;


   type Pointer_Pointer is access all glx.GLXBufferSwapComplete.Pointer;

   function construct return  glx.GLXBufferSwapComplete.item;



private

   pragma Import (C, construct, "Ada_new_GLXBufferSwapComplete");

end glx.GLXBufferSwapComplete;
