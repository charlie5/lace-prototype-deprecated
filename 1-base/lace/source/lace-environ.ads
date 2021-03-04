with
     posix.Permissions,
     ada.Streams;

private
with
     ada.Containers.indefinite_Vectors;

package lace.Environ
--
-- A singleton which models an operating system environment.
--
is

   subtype Data is ada.Streams.Stream_Element_Array;

   type Context is limited private;


   --- OS Commands
   --
   function Path_to         (Command : in String)       return String;

   function shell_Output_of (Command : in String)       return String;
   function       Output_of (Command : in String;
                             Input   : in String := "") return String;

   procedure run_OS (command_Line : in String;
                     Input        : in String := "");
   --
   -- Discards any output. Error is raised when the command fails.

   function  run_OS (command_Line : in String;
                     Input        : in String := "") return String;
   --
   -- Returns any output. Error is raised when the command fails.

   function  run_OS (command_Line : in String;
                     Input        : in String := "") return Data;
   --
   -- Returns any output. Error is raised when the command fails.


   --- Users
   --
   procedure add_User       (Name  : in String;
                             Super : in Boolean := False);
   procedure rid_User       (Name  : in String);

   procedure set_Password   (User  : in String);
   procedure switch_to_User (Named : in String);

   function  current_User                           return String;
   function  home_Folder    (user_Name : in String) return String;


   --- Paths
   --
   procedure link (From, To : in String);



   --- Folders
   --
   function current_Folder return String;

   procedure   goto_Folder (Named  : in String;
                            Lock   : in Boolean := False);  -- Blocks further folder changes until 'unlock_Folder' is called.
   procedure unlock_Folder;

   procedure   push_Folder (Context     : in out Environ.Context;
                            goto_Folder : in     String);
   procedure    pop_Folder (Context     : in out Environ.Context);

   procedure    rid_Folder (Named  : in String);
   procedure verify_Folder (Named  : in String);
   --
   -- Ensure that the folder exists.

   procedure change_Mode   (Folder : in String;
                            To     : in String);

   procedure change_Owner  (Folder : in String;
                            To     : in String);

   function  Exists        (Folder : in String) return Boolean;


   --- Files
   --
   procedure save (the_Text : in String;
                   Filename : in String;
                   Binary   : in Boolean := False);

   procedure save (the_Data : in Data;
                   Filename : in String);

   procedure move_File     (Named    : in String;   To : in String);
   procedure  rid_File     (Named    : in String);

   procedure touch         (Filename    : in String);
   function  to_octal_Mode (Permissions : in posix.Permissions.Permission_Set) return String;
   function  Expand        (File_GLOB   : in String) return String;

   procedure   compress    (Filename : in String);
   procedure decompress    (Filename : in String);


   --- Exceptions
   --
   Error : exception;



private

   use ada.Containers;

   package String_Vectors is new indefinite_Vectors (Positive, String);
   subtype String_Vector  is String_Vectors.Vector;

   type Context is limited
      record
         folder_Stack : String_Vector;
      end record;

end lace.Environ;
