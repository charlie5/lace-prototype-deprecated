package gel.Dolly.simple
--
--  Provides a simple camera dolly.
--
is
   type Item is new gel.Dolly.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   overriding
   procedure define  (Self : in out Item);
   overriding
   procedure destroy (Self : in out Item);


   --------------
   --- Operations
   --

   overriding
   procedure freshen (Self : in out Item);



private

   type Direction_Flags is array (Direction) of Boolean;

   type Item is new gel.Dolly.item with null record;

end gel.Dolly.simple;
