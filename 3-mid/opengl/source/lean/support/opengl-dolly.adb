with
     ada.Text_IO;

package body openGL.Dolly
is

   procedure Speed_is (Self : in out Item;   Now : in Real)
   is
   begin
      Self.Speed := Now;
   end Speed_is;



   procedure evolve (Self : in out Item)
   is
      use linear_Algebra_3d,
          ada.Text_IO;

      Command : Character;
      Avail   : Boolean;
   begin
      get_Immediate (Command, Avail);

      if Avail
      then
         case Command
         is
            when 'q'    =>   Self.quit_Requested := True;

               -- Linear Motion.
               --
            when 'a'    =>   Self.Camera.Site_is (Self.Camera.Site -   right_Direction (Self.Camera.Spin) * Self.Speed);
            when 's'    =>   Self.Camera.Site_is (Self.Camera.Site +   right_Direction (Self.Camera.Spin) * Self.Speed);
            when 'w'    =>   Self.Camera.Site_is (Self.Camera.Site - forward_Direction (Self.Camera.Spin) * Self.Speed);
            when 'z'    =>   Self.Camera.Site_is (Self.Camera.Site + forward_Direction (Self.Camera.Spin) * Self.Speed);
            when 'e'    =>   Self.Camera.Site_is (Self.Camera.Site +      up_Direction (Self.Camera.Spin) * Self.Speed);
            when 'd'    =>   Self.Camera.Site_is (Self.Camera.Site -      up_Direction (Self.Camera.Spin) * Self.Speed);

               -- Orbital motion.
               --
            when 'A'    =>   Self.Camera.Site_is (Self.Camera.Site * y_Rotation_from (to_Radians (-5.0)));
                             Self.Camera.Spin_is (Self.Camera.Spin * y_Rotation_from (to_Radians (-5.0)));
            when 'S'    =>   Self.Camera.Site_is (Self.Camera.Site * y_Rotation_from (to_Radians ( 5.0)));
                             Self.Camera.Spin_is (Self.Camera.Spin * y_Rotation_from (to_Radians ( 5.0)));

            when 'E'    =>   Self.Camera.Site_is (Self.Camera.Site * x_Rotation_from (to_Radians (-5.0)));
                             Self.Camera.Spin_is (Self.Camera.Spin * x_Rotation_from (to_Radians (-5.0)));
            when 'D'    =>   Self.Camera.Site_is (Self.Camera.Site * x_Rotation_from (to_Radians ( 5.0)));
                             Self.Camera.Spin_is (Self.Camera.Spin * x_Rotation_from (to_Radians ( 5.0)));

            when 'W'    =>   Self.Camera.Site_is (Self.Camera.Site * z_Rotation_from (to_Radians (-5.0)));
                             Self.Camera.Spin_is (Self.Camera.Spin * z_Rotation_from (to_Radians (-5.0)));
            when 'Z'    =>   Self.Camera.Site_is (Self.Camera.Site * z_Rotation_from (to_Radians ( 5.0)));
                             Self.Camera.Spin_is (Self.Camera.Spin * z_Rotation_from (to_Radians ( 5.0)));

            when others =>   null;
         end case;

         Self.last_Character := Command;
      end if;
   end evolve;



   function quit_Requested (Self : in Item) return Boolean
   is
   begin
      return Self.quit_Requested;
   end quit_Requested;



   procedure get_last_Character (Self : in out Item;   the_Character : out Character;
                                                       Available     : out Boolean)
   is
      use ada.Characters;
   begin
      if Self.last_Character = latin_1.NUL
      then
         Available := False;
      else
         Available           := True;
         the_Character       := Self.last_Character;
         Self.last_Character := latin_1.NUL;
      end if;
   end get_last_Character;


end openGL.Dolly;
