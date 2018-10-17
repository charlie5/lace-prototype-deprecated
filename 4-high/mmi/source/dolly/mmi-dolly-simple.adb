with
     float_math.Algebra.linear.d3;


package body mmi.Dolly.simple
is

   use Math,
       math.Algebra.linear,
       math.Algebra.linear.d3;


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

      initial_Site : constant math.Vector_3   := Self.Cameras.first_Element.Site;
      initial_Spin : constant math.Matrix_3x3 := Self.Cameras.first_Element.world_Rotation;

      new_Site     : math.Vector_3;
      new_Spin     : math.Matrix_3x3;

      site_Updated : Boolean := False;
      spin_Updated : Boolean := False;

      procedure update_Site (To : in math.Vector_3)
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

      if Self.Motion (forward)  then   update_Site (initial_Site - forward_Direction (initial_Spin) * Speed);   end if;
      if Self.Motion (backward) then   update_Site (initial_Site + forward_Direction (initial_Spin) * Speed);   end if;

      if Self.Motion (left)     then   update_Site (initial_Site - right_Direction   (initial_Spin) * Speed);   end if;
      if Self.Motion (right)    then   update_Site (initial_Site + right_Direction   (initial_Spin) * Speed);   end if;

      if Self.Motion (up)       then   update_Site (initial_Site + up_Direction      (initial_Spin) * Speed);   end if;
      if Self.Motion (down)     then   update_Site (initial_Site - up_Direction      (initial_Spin) * Speed);   end if;


      --  Angular Spin
      --

      if Self.Spin (left)       then   update_Spin (initial_Spin * y_Rotation_from (-rotate_Factor));     end if;
      if Self.Spin (right)      then   update_Spin (initial_Spin * y_Rotation_from ( rotate_Factor));     end if;

      if Self.Spin (forward)    then   update_Spin (initial_Spin * x_Rotation_from ( rotate_Factor));     end if;
      if Self.Spin (backward)   then   update_Spin (initial_Spin * x_Rotation_from (-rotate_Factor));     end if;

      if Self.Spin (up)         then   update_Spin (initial_Spin * z_Rotation_from (-rotate_Factor));     end if;
      if Self.Spin (down)       then   update_Spin (initial_Spin * z_Rotation_from ( rotate_Factor));     end if;


      --  Orbit
      --

      if Self.Orbit (left)
      then
         update_Site (initial_Site * y_Rotation_from (orbit_Factor * Speed));
         update_Spin (initial_Spin * y_Rotation_from (orbit_Factor * Speed));
      end if;

      if Self.Orbit (right)
      then
         update_Site (initial_Site * y_Rotation_from (-orbit_Factor * Speed));
         update_Spin (initial_Spin * y_Rotation_from (-orbit_Factor * Speed));
      end if;


      if Self.Orbit (forward)
      then
         update_Site (initial_Site * x_Rotation_from (-orbit_Factor * Speed));
         update_Spin (initial_Spin * x_Rotation_from (-orbit_Factor * Speed));
      end if;

      if Self.Orbit (backward)
      then
         update_Site (initial_Site * x_Rotation_from (orbit_Factor * Speed));
         update_Spin (initial_Spin * x_Rotation_from (orbit_Factor * Speed));
      end if;


      if Self.Orbit (up)
      then
         update_Site (initial_Site * z_Rotation_from (-orbit_Factor * Speed));
         update_Spin (initial_Spin * z_Rotation_from (-orbit_Factor * Speed));
      end if;

      if Self.Orbit (down)
      then
         update_Site (initial_Site * z_Rotation_from (orbit_Factor * Speed));
         update_Spin (initial_Spin * z_Rotation_from (orbit_Factor * Speed));
      end if;


      -- Update each camera with new site and spin.
      --
      declare
         use camera_Vectors;
         the_Camera : mmi.Camera.view;
         Cursor     : camera_Vectors.Cursor := Self.Cameras.First;
      begin
         while Has_Element (Cursor)
         loop
            the_Camera := Element (Cursor);

            if site_Updated
            then
               the_Camera.Site_is (new_Site);
            end if;

            if spin_Updated
            then
               the_Camera.world_Rotation_is (new_Spin);
            end if;

            next (Cursor);
         end loop;
      end;

   end freshen;


end mmi.Dolly.simple;

