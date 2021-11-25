package body gel.Dolly.simple
is

   overriding
   procedure define (Self : in out Item)
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
   --  Operations
   --

   overriding
   procedure freshen (Self : in out Item)
   is
      use Math,
          linear_Algebra_3D;

      Speed         : constant Real := Self.Speed * Self.Multiplier;
      rotate_Factor : constant Real := 0.04;
      orbit_Factor  : constant Real := 0.08;

      initial_Site  : constant Vector_3   := Self.Cameras.first_Element.Site;
      initial_Spin  : constant Matrix_3x3 := Self.Cameras.first_Element.Spin;

      new_Site      : Vector_3;
      new_Spin      : Matrix_3x3;

      site_Updated  : Boolean := False;
      spin_Updated  : Boolean := False;

      procedure update_Site (To : in Vector_3)
      is
      begin
         new_Site     := To;
         site_Updated := True;
      end update_Site;

      procedure update_Spin (To : in math.Matrix_3x3)
      is
      begin
         new_Spin     := To;
         spin_Updated := True;
      end update_Spin;

   begin
      --  Linear Motion
      --

      if Self.Motion (Forward)  then   update_Site (initial_Site - forward_Direction (initial_Spin) * Speed);   end if;
      if Self.Motion (Backward) then   update_Site (initial_Site + forward_Direction (initial_Spin) * Speed);   end if;

      if Self.Motion (Left)     then   update_Site (initial_Site - right_Direction   (initial_Spin) * Speed);   end if;
      if Self.Motion (Right)    then   update_Site (initial_Site + right_Direction   (initial_Spin) * Speed);   end if;

      if Self.Motion (Up)       then   update_Site (initial_Site + up_Direction      (initial_Spin) * Speed);   end if;
      if Self.Motion (Down)     then   update_Site (initial_Site - up_Direction      (initial_Spin) * Speed);   end if;

      --  Angular Spin
      --

      if Self.Spin (Left)       then   update_Spin (y_Rotation_from (-rotate_Factor) * initial_Spin);     end if;
      if Self.Spin (Right)      then   update_Spin (y_Rotation_from ( rotate_Factor) * initial_Spin);     end if;

      if Self.Spin (Forward)    then   update_Spin (x_Rotation_from ( rotate_Factor) * initial_Spin);     end if;
      if Self.Spin (Backward)   then   update_Spin (x_Rotation_from (-rotate_Factor) * initial_Spin);     end if;

      if Self.Spin (Up)         then   update_Spin (z_Rotation_from (-rotate_Factor) * initial_Spin);     end if;
      if Self.Spin (Down)       then   update_Spin (z_Rotation_from ( rotate_Factor) * initial_Spin);     end if;

      --  Orbit
      --

      if Self.Orbit (Left)
      then
         update_Site (initial_Site * y_Rotation_from (orbit_Factor * Speed));
         update_Spin (initial_Spin * y_Rotation_from (orbit_Factor * Speed));
      end if;

      if Self.Orbit (Right)
      then
         update_Site (initial_Site * y_Rotation_from (-orbit_Factor * Speed));
         update_Spin (initial_Spin * y_Rotation_from (-orbit_Factor * Speed));
      end if;


      if Self.Orbit (Forward)
      then
         update_Site (initial_Site * x_Rotation_from (-orbit_Factor * Speed));
         update_Spin (initial_Spin * x_Rotation_from (-orbit_Factor * Speed));
      end if;

      if Self.Orbit (Backward)
      then
         update_Site (initial_Site * x_Rotation_from (orbit_Factor * Speed));
         update_Spin (initial_Spin * x_Rotation_from (orbit_Factor * Speed));
      end if;


      if Self.Orbit (Up)
      then
         update_Site (initial_Site * z_Rotation_from (-orbit_Factor * Speed));
         update_Spin (initial_Spin * z_Rotation_from (-orbit_Factor * Speed));
      end if;

      if Self.Orbit (Down)
      then
         update_Site (initial_Site * z_Rotation_from (orbit_Factor * Speed));
         update_Spin (initial_Spin * z_Rotation_from (orbit_Factor * Speed));
      end if;


      -- Update each camera with new site and spin.
      --
      declare
         use camera_Vectors;
         the_Camera : gel.Camera.view;
         Cursor     : camera_Vectors.Cursor := Self.Cameras.First;
      begin
         while has_Element (Cursor)
         loop
            the_Camera := Element (Cursor);

            if site_Updated
            then
               the_Camera.Site_is (new_Site);
            end if;

            if spin_Updated
            then
               the_Camera.Spin_is (new_Spin);
            end if;

            next (Cursor);
         end loop;
      end;

   end freshen;


end gel.Dolly.simple;

