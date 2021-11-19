package body gel.Dolly.following
is

   overriding
   procedure define  (Self : in out Item)
   is
   begin
      null;
   end define;



   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end destroy;


   --------------
   --- Attributes
   --

   procedure follow (Self : in out Item;   the_Sprite : in gel.Sprite.view)
   is
   begin
      Self.Sprite := the_Sprite;
   end follow;



   overriding
   procedure allow_linear_Motion  (Self : in out Item;   Allow : in Boolean := True)
   is
   begin
      Self.allow_linear_Motion := Allow;
   end allow_linear_Motion;



   overriding
   procedure allow_orbital_Motion (Self : in out Item;   Allow : in Boolean := True)
   is
   begin
      Self.allow_orbital_Motion := Allow;
   end allow_orbital_Motion;



   function Offset (Self : in Item) return math.Vector_3
   is
   begin
      return Self.sprite_Offset;
   end Offset;



   procedure Offset_is (Self : in out Item;   Now : in math.Vector_3)
   is
   begin
      Self.sprite_Offset := Now;
   end Offset_is;


   --------------
   --- Operations
   --

   overriding
   procedure freshen (Self : in out Item)
   is
      use Math,
          linear_Algebra_3D;

      Speed           :          math.Real  renames Self.Speed;
      the_sprite_Site : constant math.Vector_3   := Self.Sprite.Site;
      the_Camera      : constant gel.Camera.view := Self.Cameras.first_Element;

   begin
      --  Linear motion.
      --
      if Self.allow_linear_Motion
      then
         if Self.Motion (Forward)  then   Self.sprite_Offset := Self.sprite_Offset - the_Camera.Spin * (0.0, 0.0, 0.1 * Speed);   end if;
         if Self.Motion (Backward) then   Self.sprite_Offset := Self.sprite_Offset + the_Camera.Spin * (0.0, 0.0, 0.1 * Speed);   end if;

         if Self.Motion (Up)       then   Self.sprite_Offset := Self.sprite_Offset + the_Camera.Spin * (0.0, 0.1 * Speed, 0.0);   end if;
         if Self.Motion (Down)     then   Self.sprite_Offset := Self.sprite_Offset - the_Camera.Spin * (0.0, 0.1 * Speed, 0.0);   end if;
      end if;

      --  Orbit.
      --
      if Self.allow_orbital_Motion
      then
         if Self.Motion (Left)
         then
            Self.camera_y_Spin := Self.camera_y_Spin - 0.01 * Speed;
            Self.sprite_Offset := y_Rotation_from (-0.01 * Speed) * Self.sprite_Offset;

            the_Camera.Spin_is (xyz_Rotation (Self.camera_x_Spin,
                                              Self.camera_y_Spin,
                                              Self.camera_z_Spin));
         end if;

         if Self.Motion (Right)
         then
            Self.camera_y_Spin := Self.camera_y_Spin + 0.01 * Speed;
            Self.sprite_Offset := y_Rotation_from (0.01 * Speed) * Self.sprite_Offset;

            the_Camera.Spin_is (xyz_Rotation (Self.camera_x_Spin,
                                              Self.camera_y_Spin,
                                              Self.camera_z_Spin));
         end if;
      end if;

      the_Camera.Site_is (the_sprite_Site + Self.sprite_Offset);
   end freshen;


end gel.Dolly.following;
