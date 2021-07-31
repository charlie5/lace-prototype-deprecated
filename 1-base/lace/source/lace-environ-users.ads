with
    lace.Environ.Paths;

private
with
     ada.Strings.unbounded;

package lace.Environ.Users
--
-- Models operating system users.
--
is
   type User is private;

   function  to_User      (Name : in String) return User;
   function  "+"          (Name : in String) return User renames to_User;

   function  Name         (Self : in User)                 return String;
   function  current_User                                  return User;
   function  home_Folder  (Self : in User := current_User) return Paths.Folder;

   procedure add_User     (Self  : in User;
                           Super : in Boolean := False);
   procedure rid_User     (Self  : in User);
   procedure switch_to    (Self  : in User);



private

   use ada.Strings.unbounded;

   type User is
      record
         Name : unbounded_String;
      end record;

end lace.Environ.Users;
