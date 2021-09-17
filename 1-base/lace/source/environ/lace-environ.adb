package body lace.Environ
is

   function to_octal_Mode (Permissions : in permission_Set) return String
   is
      function octal_Permissions (Bit_3, Bit_2, Bit_1 : in Boolean) return String
      is
      begin
         if Bit_3 then
            if Bit_2 then
               if Bit_1 then   return "7";
               else            return "6";
               end if;
            else
               if Bit_1 then   return "5";
               else            return "4";
               end if;
            end if;
         else
            if Bit_2 then
               if Bit_1 then   return "3";
               else            return "2";
               end if;
            else
               if Bit_1 then   return "1";
               else            return "0";
               end if;
            end if;
         end if;
      end octal_Permissions;

   begin
      return
          octal_Permissions (Permissions (set_User_ID), Permissions (set_Group_ID), False)
        & octal_Permissions (Permissions (owner_Read),  Permissions (owner_Write),  Permissions (owner_Execute))
        & octal_Permissions (Permissions (group_Read),  Permissions (group_Write),  Permissions (group_Execute))
        & octal_Permissions (Permissions (others_Read), Permissions (others_Write), Permissions (others_Execute));
   end to_octal_Mode;

end lace.Environ;
