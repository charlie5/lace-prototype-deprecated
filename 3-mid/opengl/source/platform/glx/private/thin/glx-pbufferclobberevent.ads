package glx.PbufferClobberEvent
is
   type Item is
      record
         Event_Type  : aliased C.int;
         Draw_Type   : aliased C.int;
         Serial      : aliased C.unsigned_long;
         Send_Event  : aliased glx.Bool;
         Display     :         System.Address;
         Drawable    : aliased glx.Drawable;
         Buffer_Mask : aliased C.unsigned;
         aux_Buffer  : aliased C.unsigned;
         X           : aliased C.int;
         Y           : aliased C.int;
         Width       : aliased C.int;
         Height      : aliased C.int;
         Count       : aliased C.int;
      end record;

   type Pointer         is access all Item;
   type Pointer_Pointer is access all Pointer;

   type Items    is array (C.size_t range <>) of aliased Item;
   type Pointers is array (C.size_t range <>) of aliased Pointer;

end glx.PbufferClobberEvent;
