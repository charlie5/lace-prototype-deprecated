with
     gel.Sprite;

package gel.Dolly.following
--
--  Provides a camera dolly which follows a sprite.
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
   --- Attributes
   --

   overriding
   procedure allow_linear_Motion  (Self : in out Item;   Allow : in Boolean := True);
   overriding
   procedure allow_orbital_Motion (Self : in out Item;   Allow : in Boolean := True);

   procedure Offset_is (Self : in out Item;   Now : in math.Vector_3);
   function  Offset    (Self : in     Item)     return math.Vector_3;


   --------------
   --- Operations
   --

   overriding
   procedure freshen (Self : in out Item);

   procedure follow  (Self : in out Item;   the_Sprite : in gel.Sprite.view);



private

   type Item is new gel.Dolly.item with
      record
         Sprite               : gel.Sprite.view;
         sprite_Offset        : math.Vector_3 := (0.0, 30.0, 0.0);

         allow_linear_Motion  : Boolean       := True;
         allow_orbital_Motion : Boolean       := True;

         camera_x_Spin        : math.Real     := 0.0;
         camera_y_Spin        : math.Real     := 0.0;
         camera_z_Spin        : math.Real     := 0.0;
      end record;

end gel.Dolly.following;
