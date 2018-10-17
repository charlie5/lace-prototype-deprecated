private
with lumen.Window;



package mmi.Window.lumen
--
--
--
is

   type Item is new mmi.Window.item with private;
   type View is access all Item'Class;




   --- construction
   --

   procedure define  (Self : in View;   Title  : in String;
                                            Width  : in Natural;
                                            Height : in Natural);
   overriding
   procedure destroy (Self : in out Item);


   package Forge
   is
      function new_Window (Title  : in String;
                           Width  : in Natural;
                           Height : in Natural) return Window.lumen.view;
   end Forge;




   --- attributes
   --




   --- operations
   --

   overriding
   procedure emit_Events  (Self : in out Item);
   overriding
   procedure enable_GL    (Self : in     Item);
   overriding
   procedure disable_GL   (Self : in     Item);
   overriding
   procedure swap_GL      (Self : in out Item);



private


   type Item is new mmi.Window.item with
      record
         window_Handle : standard.lumen.Window.Window_Handle;
      end record;


end mmi.Window.lumen;
