with
     posix.user_Database,
     posix.process_Identification,
     posix.file_Status,
     posix.Calendar,

     gnat.Expect,
     gnat.OS_Lib,
     gnat.Strings,

     shell.Commands,
     shell.Directory_Iteration,

     lace.Text.all_Tokens,

     ada.Strings.fixed,
     ada.Directories,
     ada.environment_Variables,
     ada.Direct_IO,
     ada.Text_IO,
     ada.IO_Exceptions,
     ada.Exceptions;

package body lace.Environ
is

   function argument_String_to_List (Arg_String : String) return gnat.OS_Lib.Argument_List_Access
   is
      use gnat.OS_Lib;

      Max_Args : constant Integer                      := Arg_String'Length;
      New_Argv :          Argument_List (1 .. Max_Args);
      New_Argc :          Natural                      := 0;
      Idx      :          Integer                      := Arg_String'First;
   begin
      loop
         exit when Idx > Arg_String'Last;

         declare
            Quoted  :          Boolean := False;
            Backqd  :          Boolean := False;
            Old_Idx : constant Integer := Idx;
         begin
            loop
               --  An unquoted space is the end of an argument.
               --
               if         not (Backqd or Quoted)
                 and then Arg_String (Idx) = ' '
               then
                  exit;

               --  Start of a quoted string.
               --
               elsif      not (Backqd or Quoted)
                 and then Arg_String (Idx) = '"'
               then
                  Quoted := True;

               --  End of a quoted string and end of an argument.
               --
               elsif      (Quoted and not Backqd)
                 and then Arg_String (Idx) = '"'
               then
                  Idx := Idx + 1;
                  exit;

               --  Following character is backquoted.
               --
               elsif Arg_String (Idx) = '\'
               then
                  Backqd := True;

               --  Turn off backquoting after advancing one character.
               --
               elsif Backqd
               then
                  Backqd := False;
               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument.
            --
            New_Argc            := New_Argc + 1;
            New_Argv (New_Argc) := new String'(Arg_String (Old_Idx .. Idx - 1));

            --  Skip extraneous spaces.
            --
            while      Idx             <= Arg_String'Last
              and then Arg_String (Idx) = ' '
            loop
               Idx := Idx + 1;
            end loop;
         end;
      end loop;

      return new Argument_List'(New_Argv (1 .. New_Argc));
   end argument_String_to_List;


   function Path_to (Command : in String) return String   -- tbd: use os_lib procedure instead !
   is
      use gnat.Expect,
          gnat.OS_Lib;

      Args : Argument_List_Access := Argument_String_To_List (Command);

      Status : aliased  Integer;
      Output : constant String := get_Command_Output (command    => "/usr/bin/which",
                                                      arguments  => Args.all,
                                                      input      => "",
                                                      status     => Status'Access,
                                                      err_to_out => True);
   begin
      free (Args);
      return Output;
   end Path_to;


   function shell_Output_of (Command : in String) return String
   is
      use ada.Environment_Variables,
          ada.Strings.fixed,
          ada.Text_IO;

      Path     : constant String   := Current_Folder & "/.lace-bin/";
      FileName : constant String   := "lace_environ_temporary_shell.sh";   -- tbd: Add a unique number here so simultaneous calls don't tread on each other.
      File     :          File_Type;
   begin
      verify_Folder (Path);

      if Index (Value ("PATH"), Path) = 0
      then
         set ("PATH",
              Path & ":" & Value ("PATH"));
      end if;

      create   (File, out_File, Path & Filename);
      put_Line (File, Command);
      close    (File);

      change_Mode (Path & Filename, to => "a+rwx");

      return Output_of (command => Filename);
   end shell_Output_of;


   function Output_of (Command : in String;
                       Input   : in String := "") return String
   is
      use ada.Strings.fixed;

      pipe_Index  : constant Natural := Index (Command, "|");
      space_Index : constant Natural := Index (Command, " ");

      function command_Name return String
      is
      begin
         if space_Index = 0
         then
            return Command;
         else
            return Command (Command'First .. space_Index - 1);
         end if;
      end command_Name;

   begin
      if pipe_Index = 0
      then
         declare
            use gnat.Expect,
                gnat.OS_Lib;

            function Arguments return String
            is
            begin
               if space_Index = 0
               then
                  return "";
               else
                  return Command (space_Index + 1 .. Command'Last);
               end if;
            end Arguments;

            arg_List : Argument_List_Access := Argument_String_to_List (Arguments);

            Status : aliased  Integer;
            Output : constant String := get_Command_Output (command    => Path_to (command_Name),
                                                            arguments  => arg_List.all,
                                                            input      => Input,
                                                            status     => Status'Access,
                                                            err_to_out => True);
         begin
            free (arg_List);

            if Status /= 0
            then
               raise Error with command_Name & ": (" & Integer'Image (Status) & ") " & Output;
            end if;

            return Output;
         end;

      else
         declare
            Command_1 : constant String := Command (Command'First  .. pipe_Index - 1);
            Command_2 : constant String := Command (pipe_Index + 1 .. Command'Last);
         begin
            return Output_of (Command_2, input => Output_of (Command_1));
         end;
      end if;

   end Output_of;


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
      Results : constant Command_Results := run (command_Line, +Input);
      Output  : constant String          := +Output_of (Results);
   begin
      if add_Errors
      then
         return Output & (+Errors_of (Results));
      else
         return Output;
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
      use gnat.Expect,
          ada.Text_IO;

      Path     : constant String := "/tmp/";
      FileName : constant String := "lace_environ_temporary_shell.sh";

      File     : File_Type;
   begin
      create   (File, out_File, Path & Filename);
      put_Line (File, "echo " & GLOB);
      close    (File);

      change_Mode (Path & Filename, to => "a+rwx");

      declare
         use gnat.Strings;

         Arg    : String_access   := new String'(Path & Filename);

         Status : aliased  Integer;
         Output : constant String := get_Command_Output (command    => Path_to ("bash"),
                                                         arguments  => (1 => Arg),
                                                         input      => "",
                                                         status     => Status'Access,
                                                         err_to_out => True);
      begin
         rid_File (Arg.all);
         free     (Arg);

         if Status /= 0
         then
            raise Error with "bash error: (" & Integer'Image (Status) & ") " & Output;
         end if;

         return Output;
      end;
   end expand_GLOB;


   procedure set_Password (User : in String)
   is
      use gnat.Expect,
          gnat.Strings;

      Username : String_access := new String' (User);

      Status   : aliased  Integer;
      Output   : constant String := get_Command_Output (command    => "/bin/passwd",
                                                        arguments  => (1 => Username),
                                                        input      => "",
                                                        status     => Status'Access,
                                                        err_to_out => True);
   begin
      free (Username);

      if Status /= 0
      then
         raise Error with "passwd: (" & Integer'Image (Status) & ") " & Output;
      end if;
   end set_Password;


   procedure add_User (Name  : in String;
                       Super : in Boolean := False)
   is
      use gnat.Expect,
          gnat.Strings;

      Status : aliased Integer;

      Username : String_access := new String'(Name);
      Arg_1    : String_access := new String'("-m");
   begin
      if Super
      then
         declare
            Arg_2 : String_access := new String'("-G");
            Arg_3 : String_access := new String'("sudo");
            Arg_4 : String_access := new String'("-G");
            Arg_5 : String_access := new String'("root");

            Output : constant String := get_Command_Output (command    => "/usr/sbin/useradd",
                                                            arguments  => (1 => Username,
                                                                           2 => Arg_1,
                                                                           3 => Arg_2,
                                                                           4 => Arg_3,
                                                                           5 => Arg_4,
                                                                           6 => Arg_5),
                                                            input      => "",
                                                            status     => Status'Access,
                                                            err_to_out => True);
         begin
            free (Username);
            free (Arg_1);
            free (Arg_2);
            free (Arg_3);
            free (Arg_4);
            free (Arg_5);

            if Status /= 0
            then
               raise Error with "useradd: (" & Integer'Image (Status) & ") " & Output;
            end if;
         end;
      else
         declare
            Output : constant String := get_Command_Output (command    => "/usr/sbin/useradd",
                                                            arguments  => (1 => Username,
                                                                           2 => Arg_1),
                                                            input      => "",
                                                            status     => Status'Access,
                                                            err_to_out => True);
         begin
            free (Username);
            free (Arg_1);

            if Status /= 0
            then
               raise Error with "useradd: (" & Integer'Image (Status) & ") " & Output;
            end if;
         end;
      end if;

   end add_User;


   procedure rid_User (Name : in String)
   is
      use gnat.Expect,
          gnat.Strings;

      Username : String_access := new String' (Name);
      Arg_1    : String_access := new String' ("-r");

      Status   : aliased  Integer;
      Output   : constant String := get_Command_Output (command    => "/usr/sbin/userdel",
                                                        arguments  => (1 => Username,
                                                                       2 => Arg_1),
                                                        input      => "",
                                                        status     => Status'Access,
                                                        err_to_out => True);
   begin
      free (Username);
      free (Arg_1);

      if Status /= 0
      then
         raise Error with "userdel: (" & Integer'Image (Status) & ") " & Output;
      end if;
   end rid_User;


   procedure change_Mode (Folder : in String;
                          To     : in String)
   is
      use gnat.Expect,
          gnat.Strings;

      the_Folder : String_access := new String'(Folder);
      Arg_1      : String_access := new String'("-R");
      Arg_2      : String_access := new String'(To);

      Status : aliased  Integer;
      Output : constant String := get_Command_Output (command    => "/bin/chmod",
                                                      arguments  => (1 => Arg_1,
                                                                     2 => Arg_2,
                                                                     3 => the_Folder),
                                                      input      => "",
                                                      status     => Status'Access,
                                                      err_to_out => True);
   begin
      free (the_Folder);
      free (Arg_1);
      free (Arg_2);

      if Status /= 0
      then
         raise Error with "chmod: (" & Integer'Image (Status) & ") " & Output;
      end if;
   end change_Mode;


   procedure change_Owner (Folder : in String;
                           To     : in String)
   is
      use gnat.Expect,
          gnat.Strings;

      the_Folder : String_access := new String'(Folder);
      Arg_1      : String_access := new String'("-R");
      Arg_2      : String_access := new String'(To);

      Status : aliased  Integer;
      Output : constant String := get_Command_Output (command    => "/bin/chown",
                                                      arguments  => (1 => Arg_1,
                                                                     2 => Arg_2,
                                                                     3 => the_Folder),
                                                      input      => "",
                                                      status     => Status'Access,
                                                      err_to_out => True);
   begin
      free (the_Folder);
      free (Arg_1);
      free (Arg_2);

      if Status /= 0
      then
         raise Error with "chown: (" & Integer'Image (Status) & ") " & Output;
      end if;
   end change_Owner;


   function Exists (Folder : in String) return Boolean
   is
   begin
      return ada.Directories.Exists (Folder);
   end Exists;


   function is_Folder (Folder : in String) return Boolean
   is
      use ada.Directories;
   begin
      return Kind (Folder) = Directory;
   end is_Folder;


   function contents_Count (Folder : in String;   Recurse : in Boolean := False) return Natural
   is
      use Shell.Directory_Iteration,
          Ada.Directories;

      Count : Natural := 0;
   begin
      for Each of To_Directory (Folder, Recurse)
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


   function is_Empty (Folder : in String) return Boolean
   is
   begin
      return contents_Count (Folder) = 0;
   end is_Empty;


   function modification_Time (Folder : in String) return ada.Calendar.Time
   is
      use POSIX,
          POSIX.Calendar,
          POSIX.File_Status;

      the_Status : constant Status     := get_File_Status (pathname => to_POSIX_String (Folder));
      Time       : constant POSIX_Time := last_modification_Time_of (the_Status);
   begin
      return to_Time (Time);
   end modification_Time;


   procedure touch (Filename : in String)
   is
      use gnat.Expect,
          gnat.Strings;

      the_Filename :          String_access := new String'(Filename);
      Status       : aliased  Integer;
      Output       : constant String        := get_Command_Output (command    => "/usr/bin/touch",
                                                                   arguments  => (1 => the_Filename),
                                                                   input      => "",
                                                                   status     => Status'Access,
                                                                   err_to_out => True);
   begin
      free (the_Filename);

      if Status /= 0
      then
         raise Error with "touch: (" & Integer'Image (Status) & ") " & Output;
      end if;
   end touch;


   procedure switch_to_User (Named : in String)
   is
      use Posix,
          posix.User_Database,
          posix.Process_Identification;

      User_in_DB : constant User_Database_Item := get_User_Database_Item (to_Posix_String (Named));
      ID         : constant User_ID            := User_ID_of (User_in_DB);
   begin
      set_User_ID (ID);
   end switch_to_User;


   procedure save (the_Text : in String;
                   Filename : in String;
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
            create (File, out_File, Filename);
            write  (File, binary_String (the_Text));
            close  (File);
         end;
      else
         declare
            use ada.Text_IO;
            File : File_Type;
         begin
            create (File, out_File, Filename);
            put    (File, the_Text);
            close  (File);
         end;
      end if;
   end save;


   procedure save (the_Data : in Data;
                   Filename : in String)
   is
      type Element_Array is new Data (the_Data'Range);
      package Binary_IO  is new ada.Direct_IO (Element_Array);
      use     Binary_IO;
      File :  File_Type;
   begin
      create (File, out_File, Filename);
      write  (File, Element_Array (the_Data));
      close  (File);
   end save;


   function load (Filename : in String) return String
   is
   begin
      declare
         Size : constant ada.Directories.File_Size := ada.Directories.Size (Filename);

         type my_String is new String (1 .. Natural (Size) - 1);

         package String_IO is new ada.Direct_IO (my_String);
         use     String_IO;

         File   : File_Type;
         Result : my_String;
      begin
         open  (File, in_File, Filename);
         read  (File, Result);
         close (File);

         return String (Result);
      end;

   exception
      when ada.IO_Exceptions.Name_Error =>
         raise Error with "Cannot load missing file: '" & Filename & "'";
   end load;


   function load (Filename : in String) return Data
   is
   begin
      declare
         use ada.Streams;
         Size : constant ada.Directories.File_Size := ada.Directories.Size (Filename);

         type Element_Array is new Data (0 .. Stream_Element_Offset (Size) - 1);

         package Binary_IO  is new ada.Direct_IO (Element_Array);
         use     Binary_IO;

         File   : Binary_IO.File_Type;
         Result : Element_Array;
      begin
         open  (File, out_File, Filename);
         read  (File, Result);
         close (File);

         return Data (Result);
      end;

   exception
      when ada.IO_Exceptions.Name_Error =>
         raise Error with "Cannot load missing file: '" & Filename & "'";
   end load;


   procedure copy_File (Named : in String;   To : in String)
   is
   begin
      ada.Directories.copy_File (Named, To);
   end copy_File;


   procedure copy_Files (Named : in String;   To : in String)
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
         if is_Folder (+Each)
         then
            copy_Folder (+Each, To);
         else
            Environ.copy_File (+Each, To & "/" & simple_Name (+Each));
         end if;
      end loop;
   end copy_Files;


   procedure move_File (Named : in String;   To : in String)
   is
   begin
      -- 'Ada.Directories.Rename' fails when the file is moved across a device.
      -- For instance     Rename ("/tmp/a_file", "/home/user/a_file");

      ada.Directories.copy_File (Named, To);
      rid_File  (Named);
   end move_File;


   procedure move_Files (Named : in String;   To : in String)
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
         if +Each /= To   -- Don't move a directory to a subdirectory of itself.
         then
            if is_Folder (+Each)
            then
               move_Folder (+Each, To);
            else
               move_File (+Each, To & "/" & simple_Name (+Each));
            end if;
         end if;
      end loop;
   end move_Files;


   procedure append_File (Named : in String;   To : in String)
   is
      use ada.Text_IO;

      Data   : constant String   := load (Named);
      Target :          File_type;
   begin
      open  (Target, append_File, name => To);
      put   (Target, Data);
      close (Target);
   end append_File;


   procedure rid_File (Named  : in String)
   is
   begin
      ada.Directories.delete_File (Named);
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
         rid_File (+Each);
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


   procedure decompress (Filename : in String)
   is
      use ada.Strings.fixed;

      the_Format : constant compress_Format := (if    Tail (Filename, 4) = ".tar"     then Tar
                                       elsif Tail (Filename, 8) = ".tar.bz2" then Tar_Bz2
                                       elsif Tail (Filename, 7) = ".tar.gz"
                                          or Tail (Filename, 4) = ".tgz"     then Tar_Gz
                                       elsif Tail (Filename, 7) = ".tar.xz"  then Tar_Xz
                                       elsif Tail (Filename, 3) = ".gz"      then Gz
                                       elsif Tail (Filename, 4) = ".bz2"     then Bz2
                                       elsif Tail (Filename, 3) = ".xz"      then Xz
                                       else  raise Error with "Unknown decompress format: " & Filename);
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
               Output  : constant String := run_OS ("tar " & Options & " " & Filename);
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Gz =>
            declare
               Output : constant String := run_OS ("gunzip --force --keep " & Filename);
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Bz2 =>
            declare
               Output : constant String := run_OS ("bunzip2 --force --keep " & Filename);
            begin
               if Output /= ""
               then
                  raise Error with Output;
               end if;
            end;

         when Xz =>
            declare
               Output : constant String := run_OS ("xz --decompress --force --keep " & Filename);
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

   procedure link (From, To : in String)
   is
      Unused : String := Output_of ("ln -s " & From & " " & To);
   begin
      null;
   end link;


   --- Folders
   --

   function current_Folder return String
   is
   begin
      return ada.Directories.current_Directory;
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


   procedure goto_Folder (Named : in String;
                          Lock  : in Boolean := False)
   is
   begin
      if Lock
      then
         folder_Lock.change (Named);
      else
         ada.Directories.set_Directory (Named);
      end if;
   end goto_Folder;


   procedure unlock_Folder
   is
   begin
      folder_Lock.clear;
   end unlock_Folder;


   procedure push_Folder (Context     : in out Environ.Context;
                          goto_Folder : in     String)
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
         prior_Folder : constant String := Context.folder_Stack.last_Element;
      begin
         Context.folder_Stack.delete_Last;
         goto_Folder (prior_Folder);
      end;
   end pop_Folder;


   --- Users
   --

   function current_User return String
   is
      use Posix,
          posix.process_Identification;
   begin
      return to_String (get_Login_Name);
   end current_User;


   function home_Folder (user_Name : in String := current_User) return String
   is
      use Posix,
          posix.User_Database;

      User_in_DB : constant User_Database_Item := get_User_Database_Item (to_Posix_String (user_Name));
   begin
      return to_String (initial_Directory_of (User_in_DB));
   end home_Folder;


   procedure rid_Folder (Named : in String)
   is
   begin
      ada.Directories.delete_Tree (Named);
   exception
      when ada.IO_Exceptions.name_Error =>
         null;
   end rid_Folder;


   procedure copy_Folder (Named  : in String;   To : in String)
   is
   begin
      run_OS ("cp -fr " & Named & " " & To);
   end copy_Folder;


   procedure move_Folder (Named  : in String;   To : in String)
   is
   begin
      run_OS ("mv " & Named & " " & To);
   end move_Folder;


   procedure rename_Folder (Named  : in String;   To : in String)
   is
   begin
      ada.Directories.rename (Named, To);
   end rename_Folder;


   procedure verify_Folder (Named : in String)
   is
   begin
      ada.Directories.create_Path (Named);
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
