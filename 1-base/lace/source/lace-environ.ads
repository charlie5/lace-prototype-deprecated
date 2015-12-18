with
     posix.Permissions;


package lace.Environ
--
-- A singleton which models an operating system environment.
--
is

   --- OS Commands
   --
   function Path_to         (the_Command : in String)       return String;

   function shell_Output_of (the_Command : in String)       return String;
   function       Output_of (the_Command : in String;
                             Input       : in String := "") return String;


   --- Users
   --
   procedure add_User       (Name     : in String;   Super : in Boolean := False);
   procedure rid_User       (Name     : in String);

   procedure set_Password   (for_User : in String);
   procedure switch_to_User (Named    : in String);

   function  current_User                           return String;
   function  home_Folder    (user_Name : in String) return String;



   --- Paths
   --
   procedure link (From, To : in String);



   --- Folders
   --
   function  current_Folder return String;

   procedure   goto_Folder (Named     : in String;
                            Lock      : in Boolean);     -- When true, blocks further folder changes until 'unlock_Folder' is called.
   procedure unlock_Folder;


   procedure remove_Folder (Named     : in String);
   procedure verify_Folder (Named     : in String);


   procedure change_Mode   (Folder    : in String;
                            To        : in String);

   procedure change_Owner  (Folder         : in String;
                            To             : in String);

   function Exists (Folder : in String) return Boolean;


   --- Files
   --
   procedure save          (the_Text      : in String;
                            to_Filename   : in String);

   procedure touch         (fileName      : in String);
   function  to_octal_Mode (Self          : in posix.Permissions.Permission_Set) return String;
   function  Expand        (the_File_GLOB : in String) return String;
   procedure decompress    (the_Filename  : in String);


   --- Exceptions
   --
   Error : exception;

end lace.Environ;
