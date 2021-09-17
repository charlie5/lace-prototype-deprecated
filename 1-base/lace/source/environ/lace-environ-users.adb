with
     lace.Environ.OS_Commands,

     posix.user_Database,
     posix.process_Identification;

package body lace.Environ.Users
is
   function "+" (Source : in unbounded_String) return String
                 renames to_String;



   function to_User (Name : in String) return User
   is
   begin
      return (Name => to_unbounded_String (Name));
   end to_User;


   function Name (Self : in User) return String
   is
   begin
      return to_String (Self.Name);
   end Name;


   procedure add_User (Self  : in User;
                       Super : in Boolean := False)
   is
      use lace.Environ.OS_Commands;
   begin
      if Super
      then
         declare
            Output : constant String := run_OS ("useradd " & (+Self.Name) & " -m -G sudo -G root");
         begin
            if Output /= ""
            then
               raise Error with Output;
            end if;
         end;
      else
         declare
            Output : constant String := run_OS ("useradd " & (+Self.Name) & " -m");
         begin
            if Output /= ""
            then
               raise Error with Output;
            end if;
         end;
      end if;
   end add_User;


   procedure rid_User (Self : in User)
   is
      use lace.Environ.OS_Commands;
      Output : constant String := run_OS ("userdel -r " & (+Self.Name));
   begin
      if Output /= ""
      then
         raise Error with Output;
      end if;
   end rid_User;


   procedure switch_to (Self : in User)
   is
      use Posix,
          posix.User_Database,
          posix.Process_Identification;

      User_in_DB : constant User_Database_Item := get_User_Database_Item (to_Posix_String (+Self.Name));
      ID         : constant User_ID            := User_ID_of (User_in_DB);
   begin
      set_User_ID (ID);
   end switch_to;


   function current_User return User
   is
      use Posix,
          posix.process_Identification;
   begin
      return to_User (to_String (get_Login_Name));
   end current_User;


   function home_Folder (Self : in User := current_User) return Paths.Folder
   is
      use Paths,
          Posix,
          posix.User_Database;

      User_in_DB : constant User_Database_Item := get_User_Database_Item (to_Posix_String (+Self.Name));
   begin
      return to_Folder (to_String (initial_Directory_of (User_in_DB)));
   end home_Folder;


end lace.Environ.Users;
