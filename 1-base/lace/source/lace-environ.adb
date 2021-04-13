with
     posix.user_Database,
     posix.process_Identification,
     posix.file_Status,
     posix.Calendar,

     shell.Commands,
     shell.Directory_Iteration,

     lace.Text.all_Tokens,

     ada.Strings.fixed,
     ada.Strings.Maps,
     ada.Characters.latin_1,
     ada.Directories,
     ada.environment_Variables,
     ada.Direct_IO,
     ada.Text_IO,
     ada.IO_Exceptions,
     ada.Exceptions;

package body lace.Environ
is

   function Path_to (Command : in String) return String
   is
   begin
      return run_OS ("which " & Command);
   end Path_to;


   procedure run_OS (command_Line : in String;
                     Input        : in String := "")
   is
      use Shell;
   begin
      Commands.run (command_Line, +Input);
   exception
      when E : Commands.command_Error =>
         raise Error with ada.Exceptions.Exception_Message (E);
   end run_OS;


   function run_OS (command_Line : in String;
                    Input        : in String  := "";
                    add_Errors   : in Boolean := True) return String
   is
      use Shell,
          Shell.Commands;

      function trim_LF (Source : in String) return String
      is
         use ada.Strings.fixed,
             ada.Strings.Maps,
             ada.Characters;

         LF_Set : constant Character_Set := to_Set (Latin_1.LF);
      begin
         return trim (Source, LF_Set, LF_Set);
      end trim_LF;

      Results : constant Command_Results := run (command_Line, +Input);
      Output  : constant String          := +Output_of (Results);
   begin
      if add_Errors
      then
         return trim_LF (Output & (+Errors_of (Results)));
      else
         return trim_LF (Output);
      end if;

   exception
      when E : Commands.command_Error =>
         raise Error with ada.Exceptions.Exception_Message (E);
   end run_OS;


   function run_OS (command_Line : in String;
                    Input        : in String := "") return Data
   is
      use Shell,
          Shell.Commands;
      the_Command : Command := to_Command (command_Line);
   begin
      return Output_of (run (The_Command, +Input));
   exception
      when E : command_Error =>
         raise Error with ada.Exceptions.Exception_Message (E);
   end run_OS;


   function expand_GLOB (GLOB : in String) return String
   is
      use ada.Text_IO;

      FileName : constant File     := "/tmp/lace_environ_temporary_shell.sh";
      File     :          File_Type;
   begin
      create   (File, out_File, +Filename);
      put_Line (File, "echo " & GLOB);
      close    (File);

      change_Mode (Path (Filename), to => "a+rwx");

      declare
         Output : constant String := run_OS ("bash " & (+Filename));
      begin
         rid_File (Filename);
         return Output;
      end;
   end expand_GLOB;


   --- Users
   --

   procedure set_Password (Name : in User)
   is
      Output : constant String := run_OS ("passwd " & String (Name));
   begin
      if Output /= ""
      then
         raise Error with Output;
      end if;
   end set_Password;


   procedure add_User (Name  : in User;
                       Super : in Boolean := False)
   is
   begin
      if Super
      then
         declare
            Output : constant String := run_OS ("useradd " & String (Name) & " -m -G sudo -G root");
         begin
            if Output /= ""
            then
               raise Error with Output;
            end if;
         end;
      else
         declare
            Output : constant String := run_OS ("useradd " & String (Name) & " -m");
         begin
            if Output /= ""
            then
               raise Error with Output;
            end if;
         end;
      end if;
   end add_User;


   procedure rid_User (Name : in User)
   is
      Output : constant String := run_OS ("userdel -r " & String (Name));
   begin
      if Output /= ""
      then
         raise Error with Output;
      end if;
   end rid_User;


   procedure change_Mode (Path : in environ.Path;
                          To   : in String)
   is
      Output : constant String := run_OS ("chmod -R " & To & " " & String (Path));
   begin
      if Output /= ""
      then
         raise Error with Output;
      end if;
   end change_Mode;


   procedure change_Owner (Path : in environ.Path;
                           To   : in String)
   is
      Output : constant String := run_OS ("chown -R " & To & " " & String (Path));
   begin
      if Output /= ""
      then
         raise Error with Output;
      end if;
   end change_Owner;


   function Exists (Folder : in environ.Folder) return Boolean
   is
   begin
      return ada.Directories.Exists (+Folder);
   end Exists;


   function is_Folder (Folder : in environ.Folder) return Boolean
   is
      use ada.Directories;
   begin
      return Kind (+Folder) = Directory;
   end is_Folder;


   function contents_Count (Folder  : in environ.Folder;
                            Recurse : in Boolean := False) return Natural
   is
      use Shell.Directory_Iteration,
          Ada.Directories;

      Count : Natural := 0;
   begin
      for Each of To_Directory (+Folder, Recurse)
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


   function is_Empty (Folder : in environ.Folder) return Boolean
   is
   begin
      return contents_Count (Folder) = 0;
   end is_Empty;


   function modification_Time (Folder : in environ.Folder) return ada.Calendar.Time
   is
      use POSIX,
          POSIX.Calendar,
          POSIX.File_Status;

      the_Status : constant Status     := get_File_Status (pathname => to_POSIX_String (+Folder));
      Time       : constant POSIX_Time := last_modification_Time_of (the_Status);
   begin
      return to_Time (Time);
   end modification_Time;


   procedure touch (Filename : in File)
   is
      Output : constant String := run_OS ("touch " & (+Filename));
   begin
      if Output /= ""
      then
         raise Error with Output;
      end if;
   end touch;


   procedure switch_to_User (Named : in User)
   is
      use Posix,
          posix.User_Database,
          posix.Process_Identification;

      User_in_DB : constant User_Database_Item := get_User_Database_Item (to_Posix_String (String (Named)));
      ID         : constant User_ID            := User_ID_of (User_in_DB);
   begin
      set_User_ID (ID);
   end switch_to_User;


   --- Files
   --

   function "+" (File : in Environ.File) return String
   is
   begin
      return String (File);
   end "+";


   function "+" (From : in String) return File
   is
   begin
      return File (From);
   end "+";


   procedure save (the_Text : in String;
                   Filename : in File;
                   Binary   : in Boolean := False)
   is
   begin
      if Binary
      then
         declare
            type binary_String is new String (the_Text'Range);
            package Binary_IO  is new ada.Direct_IO (binary_String);
            use     Binary_IO;
            File :  File_Type;
         begin
            create (File, out_File, +Filename);
            write  (File, binary_String (the_Text));
            close  (File);
         end;
      else
         declare
            use ada.Text_IO;
            File : File_Type;
         begin
            create (File, out_File, +Filename);
            put    (File, the_Text);
            close  (File);
         end;
      end if;
   end save;


   procedure save (the_Data : in Data;
                   Filename : in File)
   is
      type Element_Array is new Data (the_Data'Range);
      package Binary_IO  is new ada.Direct_IO (Element_Array);
      use     Binary_IO;
      File :  File_Type;
   begin
      create (File, out_File, +Filename);
      write  (File, Element_Array (the_Data));
      close  (File);
   end save;


   function load (Filename : in File) return String
   is
   begin
      declare
         Size : constant ada.Directories.File_Size := ada.Directories.Size (+Filename);

         type my_String is new String (1 .. Natural (Size) - 1);

         package String_IO is new ada.Direct_IO (my_String);
         use     String_IO;

         File   : File_Type;
         Result : my_String;
      begin
         open  (File, in_File, +Filename);
         read  (File, Result);
         close (File);

         return String (Result);
      end;

   exception
      when ada.IO_Exceptions.Name_Error =>
         raise Error with "Cannot load missing file: '" & (+Filename) & "'";
   end load;


   function load (Filename : in File) return Data
   is
   begin
      declare
         use ada.Streams;
         Size : constant ada.Directories.File_Size := ada.Directories.Size (+Filename);

         type Element_Array is new Data (0 .. Stream_Element_Offset (Size) - 1);

         package Binary_IO  is new ada.Direct_IO (Element_Array);
         use     Binary_IO;

         File   : Binary_IO.File_Type;
         Result : Element_Array;
      begin
         open  (File, out_File, +Filename);
         read  (File, Result);
         close (File);

         return Data (Result);
      end;

   exception
      when ada.IO_Exceptions.Name_Error =>
         raise Error with "Cannot load missing file: '" & (+Filename) & "'";
   end load;


   procedure copy_File (Named : in File;   To : in File)
   is
   begin
      ada.Directories.copy_File (+Named, +To);
   end copy_File;


   procedure copy_Files (Named : in String;   To : in Folder)
   is
      use lace.Text,
          lace.Text.all_Tokens,
          ada.Directories,
          ada.Strings.fixed;

      all_Files : constant String        := (if Index (Named, "*") /= 0 then Expand_GLOB (Named)
                                                                        else Named);
      file_List : constant Text.items_1k := Tokens (to_Text (all_Files));
   begin
      verify_Folder (To);

      for Each of file_List
      loop
         if is_Folder (+(+Each))
         then
            copy_Folder (+(+Each), To);
         else
            Environ.copy_File (File (+Each),
                               File (+To & "/" & simple_Name (+Each)));
         end if;
      end loop;
   end copy_Files;


   procedure move_File (Named : in File;   To : in File)
   is
   begin
      -- 'Ada.Directories.Rename' fails when the file is moved across a device.
      -- For instance     Rename ("/tmp/a_file", "/home/user/a_file");

      ada.Directories.copy_File (+Named, +To);
      rid_File (Named);
   end move_File;


   procedure move_Files (Named : in String;   To : in Folder)
   is
      use lace.Text,
          lace.Text.all_Tokens,
          ada.Directories,
          ada.Strings.fixed;

      all_Files : constant String        := (if Index (Named, "*") /= 0 then Expand_GLOB (Named)
                                                                        else Named);
      file_List : constant Text.items_1k := Tokens (to_Text (all_Files));
   begin
      for Each of file_List
      loop
         if +Each /= +To   -- Don't move a directory to a subdirectory of itself.
         then
            if is_Folder (+(+Each))
            then
               move_Folder (+(+Each), To);
            else
               move_File (File (+Each),
                          File (+To & "/" & simple_Name (+Each)));
            end if;
         end if;
      end loop;
   end move_Files;


   procedure append_File (Named : in File;   To : in File)
   is
      use ada.Text_IO;

      Data   : constant String   := load (Named);
      Target :          File_type;
   begin
      open  (Target, append_File, name => +To);
      put   (Target, Data);
      close (Target);
   end append_File;


   procedure rid_File (Named  : in File)
   is
   begin
      ada.Directories.delete_File (+Named);
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
         rid_File (File (+Each));
      end loop;
   end rid_Files;


   procedure compress (Path       : in String;
                       the_Format : in compress_Format := Tar_Xz;
                       the_Level  : in compress_Level  := 6)
   is
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
               Output  : constant String := run_OS ("tar " & Options
                                                    & " "  & Path & format_Suffix (the_Format)
                                                    & " "  & Path);
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
                                                   & " " & Path);
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Bz2 =>
            declare
               Output : constant String := run_OS ("bzip2 --force --keep"
                                                   & level_Flag
                                                   & " " & Path);
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Xz =>
            declare
               Output : constant String := run_OS ("xz --force --keep --threads=0 " & Path);
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;
      end case;
   end compress;


   procedure decompress (Filename : in File)
   is
      use ada.Strings.fixed;

      the_Format : constant compress_Format := (if    Tail (+Filename, 4) = ".tar"     then Tar
                                                elsif Tail (+Filename, 8) = ".tar.bz2" then Tar_Bz2
                                                elsif Tail (+Filename, 7) = ".tar.gz"
                                                   or Tail (+Filename, 4) = ".tgz"     then Tar_Gz
                                                elsif Tail (+Filename, 7) = ".tar.xz"  then Tar_Xz
                                                elsif Tail (+Filename, 3) = ".gz"      then Gz
                                                elsif Tail (+Filename, 4) = ".bz2"     then Bz2
                                                elsif Tail (+Filename, 3) = ".xz"      then Xz
                                                else  raise Error with "Unknown decompress format: " & (+Filename));
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
               Output  : constant String := run_OS ("tar " & Options & " " & (+Filename));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Gz =>
            declare
               Output : constant String := run_OS ("gunzip --force --keep " & (+Filename));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Bz2 =>
            declare
               Output : constant String := run_OS ("bunzip2 --force --keep " & (+Filename));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Xz =>
            declare
               Output : constant String := run_OS ("xz --decompress --force --keep " & (+Filename));
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;
      end case;

   end decompress;


   function format_Suffix (Format : Environ.compress_Format) return String
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


   --- Paths
   --

   procedure link (From, To : in Path)
   is
      Output : constant String := run_OS ("ln -s " & String (From) & " " & String (To));
   begin
      if Output /= ""
      then
         raise Error with Output;
      end if;
   end link;


   --- Folders
   --

   function "+" (Folder : in Environ.Folder) return String
   is
   begin
      return String (Folder);
   end "+";


   function "+" (From : in String) return Folder
   is
   begin
      return Folder (From);
   end "+";


   function current_Folder return Folder
   is
   begin
      return +ada.Directories.current_Directory;
   end current_Folder;



   protected folder_Lock
   is
      entry change (To : in String);
      procedure clear;
   private
      Locked : Boolean := False;
   end folder_Lock;


   protected body folder_Lock
   is
      entry change (To : in String)
      when not Locked
      is
      begin
         ada.Directories.set_Directory (To);
         Locked := True;
      end change;

      procedure clear
      is
      begin
         Locked := False;
      end clear;
   end folder_Lock;


   procedure goto_Folder (Named  : in Folder;
                          Lock   : in Boolean := False)
   is
   begin
      if Lock
      then
         folder_Lock.change (+Named);
      else
         ada.Directories.set_Directory (+Named);
      end if;
   end goto_Folder;


   procedure unlock_Folder
   is
   begin
      folder_Lock.clear;
   end unlock_Folder;


   procedure push_Folder (Context     : in out Environ.Context;
                          goto_Folder : in     Folder)
   is
   begin
      Context.folder_Stack.append (current_Folder);
      Environ.goto_Folder (goto_Folder);
   end push_Folder;


   procedure pop_Folder (Context : in out Environ.Context)
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
         goto_Folder (prior_Folder);
      end;
   end pop_Folder;


   --- Users
   --

   function current_User return User
   is
      use Posix,
          posix.process_Identification;
   begin
      return User (to_String (get_Login_Name));
   end current_User;


   function home_Folder (user_Name : in User := current_User) return Folder
   is
      use Posix,
          posix.User_Database;

      User_in_DB : constant User_Database_Item := get_User_Database_Item (to_Posix_String (String (user_Name)));
   begin
      return Folder (to_String (initial_Directory_of (User_in_DB)));
   end home_Folder;


   procedure rid_Folder (Named  : in Folder)
   is
   begin
      ada.Directories.delete_Tree (+Named);
   exception
      when ada.IO_Exceptions.name_Error =>
         null;
   end rid_Folder;


   procedure copy_Folder (Named  : in Folder;   To : in Folder)
   is
   begin
      run_OS ("cp -fr " & (+Named) & " " & (+To));
   end copy_Folder;


   procedure move_Folder (Named  : in Folder;   To : in Folder)
   is
   begin
      run_OS ("mv " & (+Named) & " " & (+To));
   end move_Folder;


   procedure rename_Folder (Named  : in Folder;   To : in Folder)
   is
   begin
      ada.Directories.rename (+Named, +To);
   end rename_Folder;


   procedure verify_Folder (Named  : in Folder)
   is
   begin
      ada.Directories.create_Path (+Named);
   end verify_Folder;



   function to_octal_Mode (Permissions : in posix.Permissions.permission_Set) return String
   is
      use posix.Permissions;

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
