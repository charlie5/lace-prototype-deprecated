with
     gel.Sprite,
     gel.World,
     ada.unchecked_Deallocation;

package body gel.Joint
is

   function to_GEL (the_Joint : standard.physics.Joint.view) return gel.Joint.view
   is
   begin
      return gel.Joint.view (the_Joint.user_Data);
   end to_GEL;


   ---------
   --- Forge
   --

   procedure define (Self : access Item;   Sprite_A, Sprite_B : access gel.Sprite.item'class)
   is
   begin
      Self.Sprite_A := Sprite_A;
      Self.Sprite_B := Sprite_B;
   end define;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Joint.item'Class, Joint.view);
   begin
      if Self /= null then
         Self.destroy;
      end if;

      deallocate (Self);
   end free;


   --------------
   --- Attributes
   --

   function Sprite_A (Self : in Item'Class) return access gel.Sprite.item'class
   is
   begin
      return Self.Sprite_A;
   end Sprite_A;



   function Sprite_B (Self : in Item'Class) return access gel.Sprite.item'class
   is
   begin
      return Self.Sprite_B;
   end Sprite_B;


   ----------
   --- Hinges
   --

   function local_Anchor_on_A (Self : in Item) return Vector_3
   is
   begin
      return Self.local_Anchor_on_A;
   end local_Anchor_on_A;



   function local_Anchor_on_B (Self : in Item) return Vector_3
   is
   begin
      return Self.local_Anchor_on_B;
   end local_Anchor_on_B;



   procedure local_Anchor_on_A_is (Self : out Item;   Now : in Vector_3)
   is
   begin
      Self.local_Anchor_on_A := Now;

      if Self.Sprite_A.World /= null
      then
         Self.Sprite_A.World.set_local_Anchor_on_A (for_Joint => Self'unchecked_Access,
                                                    To        => Now);
      end if;
   end local_Anchor_on_A_is;



   procedure local_Anchor_on_B_is (Self : out Item;   Now : in Vector_3)
   is
   begin
      Self.local_Anchor_on_B := Now;

      if Self.Sprite_B.World /= null
      then
         Self.Sprite_B.World.set_local_Anchor_on_B (for_Joint => Self'unchecked_Access,
                                                    To        => Now);
      end if;
   end local_Anchor_on_B_is;



   function reaction_Force (Self : in Item'Class) return Vector_3
   is
   begin
      return Self.Physics.reaction_Force;
   end reaction_Force;



   function reaction_Torque (Self : in Item'Class) return Real
   is
   begin
      return Self.Physics.reaction_Torque;
   end reaction_Torque;


end gel.Joint;
