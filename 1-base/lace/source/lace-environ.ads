with
     posix.Permissions,
     ada.Calendar,
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

   -- 'run_OS'
   --

   procedure run_OS (command_Line : in String;
                     Input        : in String := "");
   --
   -- Discards any output. Error is raised when the command fails.

   function  run_OS (command_Line : in String;
                     Input        : in String := "") return Data;
   --
   -- Returns any output. Error is raised when the command fails.

   function  run_OS (command_Line : in String;
                     Input        : in String  := "";
                     add_Errors   : in Boolean := True) return String;
   --
   -- Returns any output. Error output is appended if add_Errors is true.


   --- Users
   --
   procedure add_User       (Name  : in String;
                             Super : in Boolean := False);
   procedure rid_User       (Name  : in String);

   procedure set_Password   (User  : in String);
   procedure switch_to_User (Named : in String);

   function  current_User                                        return String;
   function  home_Folder (user_Name : in String := current_User) return String;


   --- Paths
   --
   procedure link (From, To : in String);



   --- Folders
   --
   type Folder is new String;

   function "+" (Folder : in Environ.Folder) return String;
   function "+" (From   : in String)         return Folder;

   function current_Folder return Folder;

   procedure   goto_Folder (Named  : in Folder;
                            Lock   : in Boolean := False);  -- Blocks further folder changes until 'unlock_Folder' is called.
   procedure unlock_Folder;

   procedure   push_Folder (Context     : in out Environ.Context;
                            goto_Folder : in     Folder);
   --
   -- Store the current folder and move to the 'goto_Folder'.

   procedure    pop_Folder (Context     : in out Environ.Context);
   --
   -- Return to the previously pushed folder.

   procedure    rid_Folder (Named  : in Folder);
   procedure   copy_Folder (Named  : in Folder;   To : in Folder);
   procedure   move_Folder (Named  : in Folder;   To : in Folder);
   procedure rename_Folder (Named  : in Folder;   To : in Folder);
   procedure verify_Folder (Named  : in Folder);
   --
   -- Ensure that the folder exists.

   procedure change_Mode  (Folder : in environ.Folder;
                           To     : in String);

   procedure change_Owner (Folder : in environ.Folder;
                           To     : in String);

   function  Exists    (Folder : in environ.Folder) return Boolean;
   function  is_Folder (Folder : in environ.Folder) return Boolean;
   function  is_Empty  (Folder : in environ.Folder) return Boolean;

   function  contents_Count (Folder  : in environ.Folder;
                             Recurse : in Boolean := False) return Natural;
   --
   -- Does not include the "." and ".." folders.

   function  modification_Time (Folder : in environ.Folder) return ada.Calendar.Time;


   --- Files
   --
   type File is new String;

   function "+" (File : in Environ.File) return String;
   function "+" (From : in String)       return File;

   procedure save (the_Text : in String;
                   Filename : in File;
                   Binary   : in Boolean := False);

   procedure save (the_Data : in Data;
                   Filename : in File);

   function  load (Filename : in File) return String;
   function  load (Filename : in File) return Data;

   procedure copy_File  (Named : in File;     To : in File);
   procedure copy_Files (Named : in String;   To : in Folder);
   --
   -- 'Named' can contain an asterix GLOB such as "*" or "*.txt".

   procedure move_File  (Named : in File;     To : in File);
   procedure move_Files (Named : in String;   To : in Folder);
   --
   -- 'Named' can contain an asterix GLOB such as "*" or "*.txt".

   procedure  rid_File  (Named : in File);
   procedure  rid_Files (Named : in String);
   --
   -- 'Named' can contain an asterix GLOB such as "*" or "*.txt".


   procedure append_File (Named : in File;   To : in File);

   procedure touch         (Filename    : in File);
   function  to_octal_Mode (Permissions : in posix.Permissions.Permission_Set) return String;
   function  expand_GLOB   (GLOB        : in String) return String;


   --- Compression
   --
   type           compress_Format is (Tar, Tar_Bz2, Tar_Gz, Tar_Xz, Bz2, Gz, Xz);
   subtype folder_compress_Format is compress_Format range Tar .. Tar_Xz;

   type compress_Level is range 1 .. 9;                           -- Higher levels result in higher compression.

   procedure   compress (Path       : in String;                  -- Folder or file name.
                         the_Format : in compress_Format := Tar_Xz;
                         the_Level  : in compress_Level  := 6);
   procedure decompress (Filename   : in File);

   function  format_Suffix (Format : compress_Format) return String;

   --- Exceptions
   --
   Error : exception;



private

   use ada.Containers;

   package Folder_Vectors is new indefinite_Vectors (Positive, Folder);
   subtype Folder_Vector  is Folder_Vectors.Vector;

   type Context is limited
      record
         folder_Stack : Folder_Vector;
      end record;

end lace.Environ;
