with
     ada.Calendar;

private
with
     ada.Strings.unbounded,
     ada.Containers.indefinite_Vectors;

package lace.Environ.Paths
--
-- A singleton which models an operating system environment.
--
is

   function expand_GLOB (GLOB : in String) return String;


   ---------
   --- Paths
   --
   type Path is abstract tagged private;


   function  to_String    (Self : in Path'Class) return String;
   function  "+"          (Self : in Path'Class) return String renames to_String;

   procedure change_Mode  (Self : in Path;   To : in String);
   procedure change_Owner (Self : in Path;   To : in String);
   procedure link         (Self : in Path;   To : in Path);

   function  Exists       (Self : in Path) return Boolean;
   function  modify_Time  (Self : in Path) return ada.Calendar.Time;
   function  Name         (Self : in Path) return String;
   function  Simple       (Self : in Path) return String;

   function  is_Folder    (Self : in Path) return Boolean;
   function  is_File      (Self : in Path) return Boolean;
   function  is_Special   (Self : in Path) return Boolean;

   function  is_Absolute  (Self : in Path) return Boolean;
   function  is_Relative  (Self : in Path) return Boolean;


   -----------
   --- Folders
   --
   type Folder is new Path with private;

   no_Folder : constant Folder;

   function to_Folder (Name : in String) return Folder;
   function "+"       (Name : in String) return Folder renames to_Folder;

   function "+" (Left  : in Folder;
                 Right : in Folder) return Folder;

   function current_Folder return Folder;


   procedure  go_to_Folder (Self : in Folder;
                            Lock : in Boolean := False);                       -- When true, blocks further folder changes until 'unlock_Folder' is called.
   procedure unlock_Folder;


   procedure    rid_Folder (Self : in Folder);
   procedure   copy_Folder (Self : in Folder;   To : in Folder);
   procedure   move_Folder (Self : in Folder;   To : in Folder);
   procedure rename_Folder (Self : in Folder;   To : in Folder);
   procedure ensure_Folder (Self : in Folder);                                 -- Ensure that the folder exists.

   function  is_Empty       (Self    : in Folder)           return Boolean;
   function  contents_Count (Self    : in Folder;                              -- Does not include the "." and ".." folders.
                             Recurse : in Boolean := False) return Natural;

   function  Parent   (Self : in Path'Class)                     return Folder;     -- Returns 'no_Folder' if 'Self' has no parent.
   function  Relative (Self : in Folder;   To : in Folder'Class) return Folder;


   -------------------
   --- Folder Contexts
   --
   type folder_Context is limited private;

   procedure push_Folder (Context     : in out folder_Context;
                          goto_Folder : in     Folder'Class);
   --
   -- Store the current folder and move to the 'goto_Folder'.

   procedure pop_Folder  (Context     : in out folder_Context);
   --
   -- Return to the previously pushed folder.

   procedure pop_All     (Context     : in out folder_Context);
   --
   -- Return to the initial current folder.


   ---------
   --- Files
   --
   type File           is new Path with private;
   type File_Extension is new String;

   function  to_File (Name : in String) return File;
   function  "+"     (Name : in String) return File renames to_File;

   function  "+" (Left  : in Folder'Class;
                  Right : in File  'Class) return File;

   function  "+" (Left  : in File'Class;
                  Right : in File_Extension) return File;

   function  Extension (Self : in File) return File_Extension;

   procedure save (Self     : in File;
                   Text     : in String;
                   Binary   : in Boolean := False);

   procedure save (Self : in File;
                   Data : in environ.Data);

   function  load (Self : in File) return String;
   function  load (Self : in File) return Data;

   procedure copy_File  (Self : in File;     To : in File);
   procedure copy_Files (Named : in String;   To : in Folder);
   --
   -- 'Named' can contain an asterix GLOB such as "*" or "*.txt".

   procedure move_File  (Self : in File;     To : in File);
   procedure move_Files (Named : in String;   To : in Folder);
   --
   -- 'Named' can contain an asterix GLOB such as "*" or "*.txt".

   procedure  rid_File  (Self  : in File);
   procedure  rid_Files (Named : in String);
   --
   -- 'Named' can contain an asterix GLOB such as "*" or "*.txt".

   procedure append      (Self : in File;   Text : in String);
   procedure append_File (Self : in File;   To   : in File);
   procedure touch       (Self : in File);

   function  Relative      (Self : in File;   To : in Folder'Class) return File;
   function  rid_Extension (Self : in File)                         return File;


   --- Compression
   --
   type           compress_Format is (Tar, Tar_Bz2, Tar_Gz, Tar_Xz, Bz2, Gz, Xz);
   subtype folder_compress_Format is compress_Format range Tar .. Tar_Xz;

   type compress_Level is range 1 .. 9;     -- Higher levels result in higher compression.

   procedure   compress (the_Path   : in Path'Class;
                         the_Format : in compress_Format := Tar_Xz;
                         the_Level  : in compress_Level  := 6);

   procedure decompress (Name       : in File);

   function  format_Suffix (Format  : in compress_Format) return String;



private

   use ada.Strings.unbounded;

   type Path is abstract tagged
      record
         Name : unbounded_String;
      end record;

   type Folder is new Path with null record;
   type File   is new Path with null record;


   no_Folder : constant Folder := (Name => null_unbounded_String);


   --- Folder Contexts
   --
   use ada.Containers;

   package Folder_Vectors is new indefinite_Vectors (Positive, Folder);
   subtype Folder_Vector  is     Folder_Vectors.Vector;

   type folder_Context is limited
      record
         folder_Stack : Folder_Vector;
      end record;


end lace.Environ.Paths;
