private
with lumen.Window;

package gel.Window.lumen
--
-- Provides a window which uses 'Lumen' as the backend.
--
is
   type Item is new gel.Window.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
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



   --------------
   --- Attributes
   --

   -- Nil.


   --------------
   --- Operations
   --

   overriding
   procedure emit_Events (Self : in out Item);
   overriding
   procedure enable_GL   (Self : in     Item);
   overriding
   procedure disable_GL  (Self : in     Item);
   overriding
   procedure swap_GL     (Self : in out Item);



private

   type Item is new gel.Window.item with
      record
         Window_handle : standard.lumen.Window.Window_handle;
      end record;

end gel.Window.lumen;
