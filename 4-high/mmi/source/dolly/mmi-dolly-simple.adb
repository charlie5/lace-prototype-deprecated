with
     float_math.Algebra.linear.d3;


package body mmi.Dolly.simple
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




   --  Operations
   --

   overriding
   procedure freshen (Self : in out Item)
   is
      Speed         : constant Real := Self.Speed * Self.speed_Multiplier;
      rotate_Factor : constant Real := 0.04;
      orbit_Factor  : constant Real := 0.08;
   begin
      --  Linear Motion
      --

      if Self.Motion (forward)  then   Self.Camera.Site_is (Self.Camera.Site - forward_Direction (Self.Camera.world_Rotation) * Speed);   end if;
      if Self.Motion (backward) then   Self.Camera.Site_is (Self.Camera.Site + forward_Direction (Self.Camera.world_Rotation) * Speed);   end if;

      if Self.Motion (left)     then   Self.Camera.Site_is (Self.Camera.Site - right_Direction   (Self.Camera.world_Rotation) * Speed);   end if;
      if Self.Motion (right)    then   Self.Camera.Site_is (Self.Camera.Site + right_Direction   (Self.Camera.world_Rotation) * Speed);   end if;

      if Self.Motion (up)       then   Self.Camera.Site_is (Self.Camera.Site + up_Direction      (Self.Camera.world_Rotation) * Speed);   end if;
      if Self.Motion (down)     then   Self.Camera.Site_is (Self.Camera.Site - up_Direction      (Self.Camera.world_Rotation) * Speed);   end if;


      --  Angular Spin
      --

      if Self.Spin (left)       then   Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * y_Rotation_from (-rotate_Factor));     end if;
      if Self.Spin (right)      then   Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * y_Rotation_from ( rotate_Factor));     end if;

      if Self.Spin (forward)    then   Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * x_Rotation_from ( rotate_Factor));     end if;
      if Self.Spin (backward)   then   Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * x_Rotation_from (-rotate_Factor));     end if;

      if Self.Spin (up)         then   Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * z_Rotation_from (-rotate_Factor));     end if;
      if Self.Spin (down)       then   Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * z_Rotation_from ( rotate_Factor));     end if;


      --  Orbit
      --

      if Self.Orbit (left)
      then
         Self.Camera.Site_is           (Self.Camera.Site           * y_Rotation_from (orbit_Factor * Speed));
         Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * y_Rotation_from (orbit_Factor * Speed));
      end if;

      if Self.Orbit (right)
      then
         Self.Camera.Site_is           (Self.Camera.Site           * y_Rotation_from (-orbit_Factor * Speed));
         Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * y_Rotation_from (-orbit_Factor * Speed));
      end if;


      if Self.Orbit (forward)
      then
         Self.Camera.Site_is           (Self.Camera.Site           * x_Rotation_from (-orbit_Factor * Speed));
         Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * x_Rotation_from (-orbit_Factor * Speed));
      end if;

      if Self.Orbit (backward)
      then
         Self.Camera.Site_is           (Self.Camera.Site           * x_Rotation_from (orbit_Factor * Speed));
         Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * x_Rotation_from (orbit_Factor * Speed));
      end if;


      if Self.Orbit (up)
      then
         Self.Camera.Site_is           (Self.Camera.Site           * z_Rotation_from (-orbit_Factor * Speed));
         Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * z_Rotation_from (-orbit_Factor * Speed));
      end if;

      if Self.Orbit (down)
      then
         Self.Camera.Site_is           (Self.Camera.Site           * z_Rotation_from (orbit_Factor * Speed));
         Self.Camera.world_Rotation_is (Self.Camera.world_Rotation * z_Rotation_from (orbit_Factor * Speed));
      end if;
   end freshen;


end mmi.Dolly.simple;

