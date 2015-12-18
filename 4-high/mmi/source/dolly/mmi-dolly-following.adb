with
     float_math.Algebra.linear.d3;


package body mmi.Dolly.following
is
   use      Math,
            math.Algebra.linear,
            math.Algebra.linear.d3;

   use type math.Real;



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




   --  Attributes
   --


   procedure follow (Self : in out Item;   the_Sprite : in mmi.Sprite.view)
   is
   begin
      Self.Sprite := the_Sprite;
   end follow;



   overriding
   procedure allow_linear_Motion  (Self : in out Item;   Allow : in Boolean)
   is
   begin
      Self.allow_linear_Motion := Allow;
   end allow_linear_Motion;



   overriding
   procedure allow_orbital_Motion (Self : in out Item;   Allow : in Boolean)
   is
   begin
      Self.allow_orbital_Motion := Allow;
   end allow_orbital_Motion;



   function Offset (Self : in Item) return Vector_3
   is
   begin
      return Self.sprite_Offset;
   end Offset;



   procedure Offset_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.sprite_Offset := Now;
   end Offset_is;




   --  Operations
   --

   overriding
   procedure freshen (Self : in out Item)
   is
      use math.Vectors;

      Speed           :          Real       renames Self.Speed;
      the_sprite_Site : constant math.Vector_3   := Self.Sprite.Site;

      the_Camera      : constant mmi.Camera.view := Self.Cameras.first_Element;

   begin
      --  Linear motion.
      --
      if Self.allow_linear_Motion
      then
         if Self.Motion (forward)  then   Self.sprite_Offset := Self.sprite_Offset - the_Camera.world_Rotation * (0.0, 0.0, 0.1 * Speed);   end if;
         if Self.Motion (backward) then   Self.sprite_Offset := Self.sprite_Offset + the_Camera.world_Rotation * (0.0, 0.0, 0.1 * Speed);   end if;

         if Self.Motion (up)       then   Self.sprite_Offset := Self.sprite_Offset + the_Camera.world_Rotation * (0.0, 0.1 * Speed, 0.0);   end if;
         if Self.Motion (down)     then   Self.sprite_Offset := Self.sprite_Offset - the_Camera.world_Rotation * (0.0, 0.1 * Speed, 0.0);   end if;
      end if;

      --  Orbit.
      --
      if Self.allow_orbital_Motion
      then
         if Self.Motion (left)
         then
            Self.camera_y_Spin := Self.camera_y_Spin - 0.01 * Speed;
            Self.sprite_Offset := y_Rotation_from (-0.01 * Speed) * Self.sprite_Offset;

            the_Camera.world_Rotation_is (xyz_Rotation (Self.camera_x_Spin,
                                                        Self.camera_y_Spin,
                                                        Self.camera_z_Spin));
         end if;

         if Self.Motion (right)
         then
            Self.camera_y_Spin := Self.camera_y_Spin + 0.01 * Speed;
            Self.sprite_Offset := y_Rotation_from (0.01 * Speed) * Self.sprite_Offset;

            the_Camera.world_Rotation_is (xyz_Rotation (Self.camera_x_Spin,
                                                        Self.camera_y_Spin,
                                                        Self.camera_z_Spin));
         end if;
      end if;

      the_Camera.Site_is (the_sprite_Site + Self.sprite_Offset);
   end freshen;


end mmi.Dolly.following;
