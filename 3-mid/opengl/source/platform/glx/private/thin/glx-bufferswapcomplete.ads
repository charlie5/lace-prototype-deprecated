package glx.BufferSwapComplete
is
   type Item is
      record
         the_Type   : aliased C.int;
         Serial     : aliased C.unsigned_long;
         send_Event : aliased Bool;
         Display    :         System.Address;
         Drawable   : aliased glx.Drawable;
         Event_type : aliased C.int;
         UST        : aliased Integer_64;
         MSC        : aliased Integer_64;
         SBC        : aliased Integer_64;
      end record;

   type Items is array (C.size_t range <>) of aliased BufferSwapComplete.item;


   type Pointer  is access all BufferSwapComplete.item;
   type Pointers is array (C.size_t range <>) of aliased BufferSwapComplete.Pointer;

   type Pointer_Pointer is access all BufferSwapComplete.Pointer;

   function Construct return BufferSwapComplete.item;



private

   pragma Import (C, Construct, "Ada_new_GLXBufferSwapComplete");

end glx.BufferSwapComplete;
