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
   --- General
   --

   use posix.Permissions;

   function to_octal_Mode (Permissions : in Permission_Set) return String;
   function expand_GLOB   (GLOB        : in String)         return String;

   subtype Data is ada.Streams.Stream_Element_Array;

   type Context is limited private;

   type File    is new String;
   type Folder  is new String;
   type Path    is new String;
   --
   -- A path can be either a file or a folder.


   --- Paths
   --

   function "+" (Path : in environ.Path) return String;
   function "+" (From : in String)       return Path;

   procedure link (From, To : in Path);

   procedure change_Mode  (Path : in environ.Path;
                           To   : in String);

   procedure change_Owner (Path : in environ.Path;
                           To   : in String);

   function  Exists      (Path : in environ.Path) return Boolean;
   function  Parent      (Path : in environ.Path) return Folder;
   function  Simple      (Path : in environ.Path) return environ.Path;
   function  modify_Time (Path : in environ.Path) return ada.Calendar.Time;

   function  is_Folder   (Path : in environ.Path) return Boolean;
   function  is_File     (Path : in environ.Path) return Boolean;
   function  is_Special  (Path : in environ.Path) return Boolean;

   function  is_Absolute (Path : in environ.Path) return Boolean;
   function  is_Relative (Path : in environ.Path) return Boolean;


   --- Folders
   --

   function "+" (Folder : in environ.Folder) return String;
   function "+" (From   : in String)         return Folder;

   function "+" (Left : in Folder;   Right : in Folder) return Folder;
   function "+" (Left : in Folder;   Right : in File)   return File;

   function current_Folder return Folder;

   procedure   goto_Folder (Named : in Folder;
                            Lock  : in Boolean := False);   -- Blocks further folder changes until 'unlock_Folder' is called.
   procedure unlock_Folder;

   procedure   push_Folder (Context     : in out environ.Context;
                            goto_Folder : in     Folder);
   --
   -- Store the current folder and move to the 'goto_Folder'.

   procedure    pop_Folder (Context     : in out environ.Context);
   --
   -- Return to the previously pushed folder.

   procedure    rid_Folder (Named : in Folder);
   procedure   copy_Folder (Named : in Folder;   To : in Folder);
   procedure   move_Folder (Named : in Folder;   To : in Folder);
   procedure rename_Folder (Named : in Folder;   To : in Folder);
   procedure verify_Folder (Named : in Folder);
   --
   -- Ensure that the folder exists.

   function  is_Empty       (Folder  : in environ.Folder)   return Boolean;

   function  contents_Count (Folder  : in environ.Folder;
                             Recurse : in Boolean := False) return Natural;
   --
   -- Does not include the "." and ".." folders.


   --- Files
   --

   function "+" (File : in environ.File) return String;
   function "+" (From : in String)       return File;

   procedure save (the_Text : in String;
                   Name     : in File;
                   Binary   : in Boolean := False);

   procedure save (the_Data : in Data;
                   Name     : in File);

   function  load (Name : in File) return String;
   function  load (Name : in File) return Data;

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
   procedure touch       (Named : in File);


   --- Compression
   --

   type           compress_Format is (Tar, Tar_Bz2, Tar_Gz, Tar_Xz, Bz2, Gz, Xz);
   subtype folder_compress_Format is compress_Format range Tar .. Tar_Xz;

   type compress_Level is range 1 .. 9;     -- Higher levels result in higher compression.

   procedure   compress (Path       : in environ.Path;
                         the_Format : in compress_Format := Tar_Xz;
                         the_Level  : in compress_Level  := 6);
   procedure decompress (Filename   : in File);

   function  format_Suffix (Format : compress_Format) return String;


   --- Users
   --

   type User is new String;

   procedure add_User (Name  : in User;
                       Super : in Boolean := False);
   procedure rid_User (Name  : in User);

   procedure set_Password   (Name  : in User);
   procedure switch_to_User (Named : in User);

   function  current_User    return User;
   function  home_Folder (Name : in User := current_User) return Folder;


   --- OS Commands
   --

   function Path_to (Command : in String) return Folder;

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
