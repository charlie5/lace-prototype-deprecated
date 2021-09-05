with
     glx.BufferSwapComplete,
     glx.PbufferClobberEvent;

package glx.Event
is
   type long_Array   is array (C.size_t range <>) of aliased C.Long;

   type Kind is (pBufferClobber,
                 BufferSwapComplete,
                 Pad);

   type Item (Kind : Event.Kind := Event.Kind'First) is
      record
         case Kind is
         when pBufferClobber     => pBufferClobber     : aliased glx.PBufferClobberEvent.item;
         when BufferSwapComplete => BufferSwapComplete : aliased glx.BufferSwapComplete .item;
         when Pad                => Pad                : aliased long_Array (0 .. 23);
         end case;
      end record
   with unchecked_Union;

   type Pointer         is access all Item;
   type Pointer_Pointer is access all Pointer;

   type Items    is array (C.size_t range <>) of aliased Item;
   type Pointers is array (C.size_t range <>) of aliased Pointer;

end glx.Event;
