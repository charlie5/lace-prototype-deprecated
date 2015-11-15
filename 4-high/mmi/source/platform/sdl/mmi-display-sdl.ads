with mmi.Screen.sdl;


package mmi.Display.sdl
--
--
--
is
--     pragma pure;

   type Item is new mmi.Display.item with private;
   type View is access all Item'Class;




   -- define/destroy
   --

   procedure define  (Self : in out Item);
   procedure destroy (Self : in out Item);


   package Forge
   is
      function new_Display return Display.view;
   end Forge;




   -- attributes
   --

   function Screens      (Self : access Item) return Screen.views;
   function screen_Count (Self : access Item) return Natural;



   -- operations
   --

   procedure emit_Events  (Self : in out Item);





private


   type Item is new mmi.Display.item with
      record
         Screens      : mmi.Screen.sdl.items (1 .. 5);
         screen_Count : Natural;
      end record;


end mmi.Display.sdl;
