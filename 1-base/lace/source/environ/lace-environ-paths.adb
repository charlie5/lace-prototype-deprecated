with
     lace.Environ.OS_Commands,
     lace.Text.utility,
     posix.file_Status,
     posix.Calendar,

     shell.Directory_Iteration,

     lace.Text.all_Tokens,

     ada.Strings.fixed,
     ada.Characters.handling,
     ada.Directories,
     ada.Direct_IO,
     ada.Tags,
     ada.Text_IO,
     ada.IO_Exceptions;

package body lace.Environ.Paths
is
   -----------
   --- General
   --

   function "+" (Source : in unbounded_String) return String
                 renames to_String;


   function expand_GLOB (GLOB : in String) return String
   is
      use ada.Text_IO;

      FileName : constant File     := +"/tmp/lace_environ_temporary_shell.sh";
      File     :          File_Type;
   begin
      create   (File, out_File, +Filename);
      put_Line (File, "echo " & GLOB);
      close    (File);

      change_Mode (Path (Filename), to => "a+rwx");

      declare
         use lace.Environ.OS_Commands;
         Output : constant String := run_OS ("bash " & (+Filename));
      begin
         rid_File (Filename);
         return Output;
      end;
   end expand_GLOB;



   ---------
   --- Paths
   --

   function to_String (Self : in Path'Class) return String
   is
   begin
      return to_String (Self.Name);
   end to_String;


   procedure check (Self : in Path'Class)
   is
      use ada.Tags,
          ada.Strings.fixed,
          ada.Characters.handling;

      Tag_full_Name : constant String := to_Lower (ada.Tags.expanded_Name (Self'Tag));
      Tag_Name      : constant String := (if Self'Tag = Folder'Tag then Tail (Tag_full_Name, 6)
                                                                   else Tail (Tag_full_Name, 4));
   begin
      if Self.Name = ""
      then
         raise Error with "No " & Tag_Name & " specified.";
      end if;

      if not Exists (Self)
      then
         raise Error with Tag_Name & " '" & (+Self) & "' does not exist.";
      end if;
   end check;


   procedure link (Self, To : in Path)
   is
   begin
      check (Self);

      declare
         use lace.Environ.OS_Commands;
         Output : constant String := run_OS (  "ln -s "
                                             & (+Self)
                                             & " "
                                             & (+To));
      begin

         if Output /= ""
         then
            raise Error with Output;
         end if;
      end;
   end link;


   procedure change_Mode (Self : in Path;
                          To   : in String)
   is
   begin
      check (Self);

      declare
         use lace.Environ.OS_Commands;
         Output : constant String := run_OS ("chmod -R " & To & " " & (+Self));
      begin
         if Output /= ""
         then
            raise Error with Output;
         end if;
      end;
   end change_Mode;


   procedure change_Owner (Self : in Path;
                           To   : in String)
   is
   begin
      check (Self);

      declare
         use lace.Environ.OS_Commands;
         Output : constant String := run_OS ("chown -R " & To & " " & (+Self));
      begin
         if Output /= ""
         then
            raise Error with Output;
         end if;
      end;
   end change_Owner;


   function Exists (Self : in Path) return Boolean
   is
   begin
      if +Self = ""
      then
         raise Error with "No path specified.";
      end if;

      return ada.Directories.Exists (+Self);
   end Exists;


   function is_Folder (Self : in Path) return Boolean
   is
      use ada.Directories;
   begin
      check (Self);
      return Kind (+Self) = Directory;
   end is_Folder;


   function is_File (Self : in Path) return Boolean
   is
      use ada.Directories;
   begin
      check (Self);
      return Kind (+Self) = Ordinary_File;
   end is_File;


   function is_Special (Self : in Path) return Boolean
   is
      use ada.Directories;
   begin
      check (Self);
      return Kind (+Self) = Special_File;
   end is_Special;


   function is_Absolute (Self : in Path) return Boolean
   is
   begin
      if Length (Self.Name) = 0
      then
         return False;
      end if;

      return Element (Self.Name, 1) = '/';
   end is_Absolute;


   function is_Relative (Self : in Path) return Boolean
   is
   begin
      return not is_Absolute (Self);
   end is_Relative;


   function modify_Time (Self : in Path) return ada.Calendar.Time
   is
   begin
      check (Self);

      declare
         use POSIX,
             POSIX.Calendar,
             POSIX.File_Status;

         the_Status : constant Status     := get_File_Status (pathname => to_POSIX_String (+Self));
         Time       : constant POSIX_Time := last_modification_Time_of (the_Status);
      begin
         return to_Time (Time);
      end;
   end modify_Time;


   function Parent (Self : in Path'Class) return Folder
   is
   begin
      declare
         use ada.Strings;
         Index : constant Natural := fixed.Index (+Self, "/", going => Backward);
      begin
         if    Index = 0
         then
            return no_Folder;

         elsif Index = 1
         then
            return +"/";
         end if;

         declare
            Result : Folder;
         begin
            Result.Name := Head (Self.Name,
                                 Count => Index - 1);
            return Result;
         end;
      end;
   end Parent;



   function Name (Self : in Path) return String
   is
   begin
   return +Self.Name;
   end Name;


   function Simple (Self : in Path) return String
   is
   begin
      check (Self);

      declare
         use ada.Strings;
         Idx  : constant Natural := Index  (Self.Name, "/", going => Backward);
         Last : constant Natural := Length (Self.Name);
      begin
         if Idx = 0
         then
            return +Self;
         else
            return Slice (Self.Name, Low  => Idx + 1,
                                     High => Last);
         end if;
      end;
   end Simple;


   -----------
   --- Folders
   --

   function to_Folder (Name : in String) return Folder
   is
   begin
      return (Name => To_Unbounded_String (Name));
   end to_Folder;


   function "+" (Left : in Folder;   Right : in Folder) return Folder
   is
      R_Folder : constant String := (if Right.is_Absolute then  Right.Simple
                                                          else +Right);
      Result   : Folder;
   begin
      Result.Name := Left.Name;
      append (Result.Name, "/" & R_Folder);

      return Result;
   end "+";


   function "+" (Left  : in Folder'Class;
                 Right : in File  'Class) return File
   is
      R_File : constant String := (if Right.is_Absolute then  Right.Simple
                                                        else +Right);
      Result : File;
   begin
      Result.Name := Left.Name;
      append (Result.Name, "/" & R_File);

      return Result;
   end "+";


   function current_Folder return Folder
   is
   begin
      return +ada.Directories.current_Directory;
   end current_Folder;


   protected folder_Lock
   is
      entry change (To : in Folder);
      procedure clear;
   private
      Locked : Boolean := False;
   end folder_Lock;


   protected body folder_Lock
   is
      entry change (To : in Folder)
      when not Locked
      is
      begin
         check (To);
         ada.Directories.set_Directory (+To);
         Locked := True;
      end change;

      procedure clear
      is
      begin
         Locked := False;
      end clear;
   end folder_Lock;


   procedure go_to_Folder (Self  : in Folder;
                           Lock   : in Boolean := False)
   is
   begin
      check (Self);

      if Lock
      then
         folder_Lock.change (Self);
      else
         ada.Directories.set_Directory (+Self);
      end if;
   end go_to_Folder;


   procedure unlock_Folder
   is
   begin
      folder_Lock.clear;
   end unlock_Folder;


   function contents_Count (Self  : in Folder;
                            Recurse : in Boolean := False) return Natural
   is
      use Shell.Directory_Iteration,
          Ada.Directories;

      Count : Natural := 0;
   begin
      check (Self);

      for Each of To_Directory (+Self, Recurse)
      loop
         declare
            Name : constant String := Simple_Name (Each);
         begin
            if not (Name = "." or Name = "..")
            then
               Count := Count + 1;
            end if;
         end;
      end loop;

      return Count;
   end contents_Count;


   function is_Empty (Self : in Folder) return Boolean
   is
   begin
      check (Self);
      return contents_Count (Self) = 0;
   end is_Empty;


   procedure rid_Folder (Self : in Folder)
   is
   begin
      check (Self);
      ada.Directories.delete_Tree (+Self);
   exception
      when ada.IO_Exceptions.name_Error =>
         null;
   end rid_Folder;


   procedure copy_Folder (Self : in Folder;   To : in Folder)
   is
      use lace.Environ.OS_Commands;
   begin
      check (Self);
      check (To);

      run_OS ("cp -fr " & (+Self) & " " & (+To));
   end copy_Folder;


   procedure move_Folder (Self : in Folder;   To : in Folder)
   is
      use lace.Environ.OS_Commands;
   begin
      check (Self);
      check (To);

      run_OS ("mv " & (+Self) & " " & (+To));
   end move_Folder;


   procedure rename_Folder (Self  : in Folder;   To : in Folder)
   is
   begin
      check (Self);

      ada.Directories.rename (+Self, +To);
   end rename_Folder;


   procedure ensure_Folder (Self  : in Folder)
   is
   begin
      if Self.Name = ""
      then
         raise Error with "No folder specified.";
      end if;

      ada.Directories.create_Path (+Self);
   end ensure_Folder;


   function Relative (Self : in Folder;   To : in Folder'Class) return Folder
   is
      use lace.Text,
          lace.Text.utility;
      Filename        : constant lace.Text.item := to_Text (+Self);
      relative_Folder : constant lace.Text.item := replace (Filename, pattern => +To & "/",
                                                                      by      => "");
   begin
      return to_Folder (+relative_Folder);
   end Relative;


   -------------------
   --- Folder Contexts
   --

   procedure push_Folder (Context     : in out folder_Context;
                          goto_Folder : in     Folder'Class)
   is
   begin
      check (goto_Folder);

      Context.folder_Stack.append (current_Folder);
      go_to_Folder (goto_Folder);
   end push_Folder;


   procedure pop_Folder (Context : in out folder_Context)
   is
   begin
      if Context.folder_Stack.is_Empty
      then
         raise Error with "'pop_Folder': No prior folder exists.";
      end if;

      declare
         prior_Folder : constant Folder := Context.folder_Stack.last_Element;
      begin
         Context.folder_Stack.delete_Last;
         go_to_Folder (prior_Folder);
      end;
   end pop_Folder;


   procedure pop_All (Context : in out folder_Context)
   is
   begin
      if Context.folder_Stack.is_Empty
      then
         raise Error with "'pop_All': No initial folder exists.";
      end if;

      go_to_Folder (Context.folder_Stack.Element (1));
      Context.folder_Stack.clear;
   end pop_All;


   ---------
   --- Files
   --

   function to_File (Name : in String) return File
   is
      Self : File;
   begin
      set_unbounded_String (Self.Name, Name);
      return Self;
   end to_File;


   function "+" (Left  : in File'Class;
                 Right : in File_Extension) return File
   is
   begin
      return to_File (+Left & "." & String (Right));
   end "+";


   function Extension (Self : in File) return File_Extension
   is
      use ada.Directories;
   begin
      return File_Extension (Extension (+Self.Name));
   end Extension;


   procedure save (Self   : in File;
                   Text   : in String;
                   Binary : in Boolean := False)
   is
   begin
      if Binary
      then
         declare
            type binary_String is new String (Text'Range);
            package Binary_IO  is new ada.Direct_IO (binary_String);
            use     Binary_IO;
            File :  File_Type;
         begin
            create (File, out_File, +Self);
            write  (File, binary_String (Text));
            close  (File);
         end;
      else
         declare
            use ada.Text_IO;
            File : File_Type;
         begin
            create (File, out_File, +Self);
            put    (File, Text);
            close  (File);
         end;
      end if;
   end save;


   procedure save (Self : in File;
                   Data : in environ.Data)
   is
   begin
      check (Self);
      declare
         type Element_Array is new environ.Data  (Data'Range);
         package Binary_IO  is new ada.Direct_IO (Element_Array);
         use     Binary_IO;
         File :  File_Type;
      begin
         create (File, out_File, +Self);
         write  (File, Element_Array (Data));
         close  (File);
      end;
   end save;


   function load (Self : in File) return String
   is
      use type ada.Directories.File_Size;
      Size : ada.Directories.File_Size;
   begin
      check (Self);
      Size := ada.Directories.Size (+Self);

      if Size = 0
      then
         return "";
      end if;

      declare
         type my_String is new String (1 .. Natural (Size));

         package String_IO is new ada.Direct_IO (my_String);
         use     String_IO;

         File   : File_Type;
         Result : my_String;
      begin
         open  (File, in_File, +Self);
         read  (File, Result);
         close (File);

         return String (Result);
      end;

   exception
      when ada.IO_Exceptions.Name_Error =>
         raise Error with "Cannot load missing file: '" & (+Self) & "'";
   end load;


   function load (Self : in File) return Data
   is
   begin
      check (Self);
      declare
         use ada.Streams;
         Size : constant ada.Directories.File_Size := ada.Directories.Size (+Self);

         type Element_Array is new Data (0 .. Stream_Element_Offset (Size) - 1);

         package Binary_IO  is new ada.Direct_IO (Element_Array);
         use     Binary_IO;

         File   : Binary_IO.File_Type;
         Result : Element_Array;
      begin
         open  (File, out_File, +Self);
         read  (File, Result);
         close (File);

         return Data (Result);
      end;

   exception
      when ada.IO_Exceptions.Name_Error =>
         raise Error with "Cannot load missing file: '" & (+Self) & "'";
   end load;


   procedure copy_File (Self : in File;   To : in File)
   is
   begin
      check (Self);
      check (To);

      ada.Directories.copy_File (+Self, +To);
   end copy_File;


   procedure copy_Files (Named : in String;   To : in Folder)
   is
      use lace.Text,
          lace.Text.all_Tokens,
          ada.Strings.fixed;

      all_Files : constant String        := (if Index (Named, "*") /= 0 then Expand_GLOB (Named)
                                                                        else Named);
      file_List : constant Text.items_1k := Tokens (to_Text (all_Files));
   begin
      check (To);

      for Each of file_List
      loop
         declare
            use ada.Directories;
            Name : constant String := +Each;
         begin
            if Kind (Name) = Directory
            then
               copy_Folder (+Name,
                            To);
            else
               copy_File (to_File (Name),
                          To + to_File (simple_Name (Name)));
            end if;
         end;
      end loop;
   end copy_Files;


   procedure move_File (Self : in File;   To : in File)
   is
   begin
      check (Self);
      check (To);

      -- 'Ada.Directories.Rename' fails when the file is moved across a device.
      -- For instance     Rename ("/tmp/a_file", "/home/user/a_file");

      ada.Directories.copy_File (+Self, +To);
      rid_File (Self);
   end move_File;


   procedure move_Files (Named : in String;   To : in Folder)
   is
   begin
      check (To);

      declare
         use lace.Text,
             lace.Text.all_Tokens,
             ada.Strings.fixed;

         all_Files : constant String        := (if Index (Named, "*") /= 0 then Expand_GLOB (Named)
                                                                           else Named);
         file_List : constant Text.items_1k := Tokens (to_Text (all_Files));
      begin
         for Each of file_List
         loop
            if +Each /= +To   -- Don't move a directory to a subdirectory of itself.
            then
               declare
                  use ada.Directories;
                  Name : constant String := +Each;
               begin
                  if Kind (Name) = Directory
                  then
                     move_Folder (+Name,
                                  To);
                  else
                     move_File (to_File (Name),
                                To + to_File (simple_Name (Name)));
                  end if;
               end;
            end if;
         end loop;
      end;
   end move_Files;


   procedure append (Self : in File;   Text : in String)
   is
   begin
      check (Self);

      declare
         use ada.Text_IO;
         Target : File_type;
      begin
         open  (Target, append_File, Name => +Self);
         put   (Target, Text);
         close (Target);
      end;
   end append;


   procedure append_File (Self : in File;   To : in File)
   is
   begin
      check (Self);
      check (To);

      declare
         use ada.Text_IO;

         Text   : constant String   := load (Self);
         Target :          File_type;
      begin
         open  (Target, append_File, Name => +To);
         put   (Target, Text);
         close (Target);
      end;
   end append_File;


   procedure rid_File (Self : in File)
   is
   begin
      check (Self);
      ada.Directories.delete_File (+Self);
   end rid_File;


   procedure rid_Files (Named : in String)
   is
      use lace.Text,
          lace.Text.all_Tokens,
          ada.Strings.fixed;

      all_Files : constant String        := (if Index (Named, "*") /= 0 then Expand_GLOB (Named)
                                                                        else Named);
      file_List : constant Text.items_1k := Tokens (to_Text (all_Files));
   begin
      for Each of file_List
      loop
         check    (to_File (+Each));
         rid_File (to_File (+Each));
      end loop;
   end rid_Files;


   procedure touch (Self : in File)
   is
      use lace.Environ.OS_Commands;
      Output : constant String := run_OS ("touch " & (+Self));
   begin
      if Output /= ""
      then
         raise Error with Output;
      end if;
   end touch;


   function Relative (Self : in File;   To : in Folder'Class) return File
   is
      use lace.Text,
          lace.Text.utility;
      Filename      : constant lace.Text.item := to_Text (+Self);
      relative_File : constant lace.Text.item := replace (Filename, pattern => +To & "/",
                                                                    by      => "");
   begin
      return to_File (+relative_File);
   end Relative;


   function rid_Extension (Self : in File) return File
   is
      use ada.Directories;

      Parent : constant Folder := Self.Parent;
      Name   : constant String := base_Name (+Self.Name);
   begin
      return Parent + to_File (Name);
   end rid_Extension;



   ---------------
   --- Compression
   --

   procedure compress (the_Path   : in Path'Class;
                       the_Format : in compress_Format := Tar_Xz;
                       the_Level  : in compress_Level  := 6)
   is
      use lace.Environ.OS_Commands;

      function level_Flag return String
      is
         use ada.Strings,
             ada.Strings.fixed;
      begin
         return " -"
              & Trim (compress_Level'Image (the_Level),
                      Left)
              & " ";
      end level_Flag;

   begin
      check (the_Path);

      case the_Format
      is
         when Tar |Tar_Bz2 | Tar_Gz | Tar_Xz =>
            declare
               Options : constant String := (case the_Format
                                             is
                                                when Tar     => "-cf",
                                                when Tar_Bz2 => "-cjf",
                                                when Tar_Gz  => "-czf",
                                                when Tar_Xz  => "-cJf",
                                                when others  => raise program_Error);
               Output  : constant String := run_OS (  "tar " & Options
                                                    & " "    & (+the_Path) & format_Suffix (the_Format)
                                                    & " "    & (+the_Path));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Gz =>
            declare
               Output : constant String := run_OS (  "gzip --force --keep --rsyncable"
                                                   & level_Flag
                                                   & " " & (+the_Path));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Bz2 =>
            declare
               Output : constant String := run_OS (  "bzip2 --force --keep"
                                                   & level_Flag
                                                   & " " & (+the_Path));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Xz =>
            declare
               Output : constant String := run_OS ("xz --force --keep --threads=0 " & (+the_Path));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;
      end case;
   end compress;


   procedure decompress (Name : in File)
   is
      use lace.Environ.OS_Commands;
   begin
      check (Name);

      declare
         use ada.Strings.fixed;

         the_Format : constant compress_Format := (if    Tail (+Name, 4) = ".tar"     then Tar
                                                   elsif Tail (+Name, 8) = ".tar.bz2" then Tar_Bz2
                                                   elsif Tail (+Name, 7) = ".tar.gz"
                                                      or Tail (+Name, 4) = ".tgz"     then Tar_Gz
                                                   elsif Tail (+Name, 7) = ".tar.xz"  then Tar_Xz
                                                   elsif Tail (+Name, 3) = ".gz"      then Gz
                                                   elsif Tail (+Name, 4) = ".bz2"     then Bz2
                                                   elsif Tail (+Name, 3) = ".xz"      then Xz
                                                   else  raise Error with "Unknown decompress format: " & (+Name));
      begin
         case the_Format
         is
         when Tar |Tar_Bz2 | Tar_Gz | Tar_Xz =>
            declare
               Options : aliased constant String := (case the_Format
                                                     is
                                                        when Tar     => "-xf",
                                                        when Tar_Bz2 => "-xjf",
                                                        when Tar_Gz  => "-xzf",
                                                        when Tar_Xz  => "-xJf",
                                                        when others  => raise program_Error);
               Output  : constant String := run_OS ("tar " & Options & " " & (+Name));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Gz =>
            declare
               Output : constant String := run_OS ("gunzip --force --keep " & (+Name));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Bz2 =>
            declare
               Output : constant String := run_OS ("bunzip2 --force --keep " & (+Name));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Xz =>
            declare
               Output : constant String := run_OS ("xz --decompress --force --keep " & (+Name));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;
         end case;
      end;

   end decompress;


   function format_Suffix (Format : compress_Format) return String
   is
   begin
      case Format
      is
         when Tar     =>   return ".tar";
         when Tar_Bz2 =>   return ".tar.bz2";
         when Tar_Gz  =>   return ".tar.gz";
         when Tar_Xz  =>   return ".tar.xz";
         when Bz2     =>   return ".bz2";
         when Gz      =>   return ".gz";
         when Xz      =>   return ".xz";
      end case;
   end format_Suffix;


end lace.Environ.Paths;
