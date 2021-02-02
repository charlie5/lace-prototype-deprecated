with
     posix.user_Database,
     posix.process_Identification,

     gnat.Expect,
     gnat.OS_Lib,
     gnat.Strings,

     ada.Strings.fixed,
     ada.Directories,
     ada.environment_Variables,
     ada.Direct_IO,
     ada.Text_IO;


package body lace.Environ
is
   use ada.Text_IO;


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
          ada.Strings.fixed;

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



   function Expand (File_GLOB : in String) return String
   is
      use gnat.Expect;

      Path     : constant String := "/usr/local/bin/";
      FileName : constant String := "lace_environ_temporary_shell.sh";

      File     : File_Type;
   begin
      create   (File, out_File, Path & Filename);
      put_Line (File, "echo " & File_GLOB);
      close    (File);

      change_Mode (Path & Filename, to => "a+rwx");

      declare
         use gnat.Strings;

         Arg : String_access := new String'(Path & Filename);

         Status : aliased  Integer;
         Output : constant String := get_Command_Output (command    => Path_to ("bash"),
                                                         arguments  => (1 => Arg),
                                                         input      => "",
                                                         status     => Status'Access,
                                                         err_to_out => True);
      begin
         free (Arg);

         if Status /= 0
         then
            raise Error with "bash: (" & Integer'Image (Status) & ") " & Output;
         end if;

         return Output;
      end;
   end Expand;



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
            package Binary_IO is new ada.Direct_IO (binary_String);
            use Binary_IO;
            File : Binary_IO.File_Type;
         begin
            create (File, out_File, Filename);
            write  (File, binary_String (the_Text));
            close  (File);
         end;
      else
         declare
            File : File_Type;
         begin
            create (File, out_File, Filename);
            put    (File, the_Text);
            close  (File);
         end;
      end if;
   end save;



   procedure decompress (Filename : in String)
   is
      use ada.Strings.fixed,
          gnat.Expect,
          gnat.Strings;
   begin
      if Tail (Filename, 7) = ".tar.gz"
      then
         declare
            tar_Options  : String_access := new String'("-xf");
            tar_Filename : String_access := new String'(Filename);

            Status : aliased  Integer;
            Output : constant String := get_Command_Output (command    => "tar",
                                                            arguments  => (1 => tar_Options,
                                                                           2 => tar_Filename),
                                                            input      => "",
                                                            status     => Status'Access,
                                                            err_to_out => True);
         begin
            free (tar_Options);
            free (tar_Filename);

            if Status /= 0
            then
               raise Error with "tar: (" & Integer'Image (Status) & ") " & Output;
            end if;
         end;

      elsif Tail (Filename, 8) = ".tar.bz2"
      then
         declare
            bunzip_Filename : String_access := new String'(Filename);

            Status : aliased  Integer;
            Output : constant String := get_Command_Output (command    => "bunzip2",
                                                            arguments  => (1 => bunzip_Filename),
                                                            input      => "",
                                                            status     => Status'Access,
                                                            err_to_out => True);
         begin
            free (bunzip_Filename);

            if Status /= 0
            then
               raise Error with "bunzip2: (" & Integer'Image (Status) & ") " & Output;
            end if;
         end;

         declare
            tar_Options  : String_access := new String'("-xf");
            tar_Filename : String_access := new String'(Head (Filename, Filename'Length - 4));

            Status : aliased  Integer;
            Output : constant String := get_Command_Output (command    => "gtar",
                                                            arguments  => (1 => tar_Options,
                                                                           2 => tar_Filename),
                                                            input      => "",
                                                            status     => Status'Access,
                                                            err_to_out => True);
         begin
            free (tar_Options);
            free (tar_Filename);

            if Status /= 0
            then
               raise Error with "gtar: (" & Integer'Image (Status) & ") " & Output;
            end if;
         end;

      elsif Tail (Filename, 4) = ".tgz"
      then
         declare
            gunzip_Filename : String_access := new String'(Filename);

            Status : aliased  Integer;
            Output : constant String := get_Command_Output (command    => "gunzip",
                                                            arguments  => (1 => gunzip_Filename),
                                                            input      => "",
                                                            status     => Status'Access,
                                                            err_to_out => True);
         begin
            free (gunzip_Filename);

            if Status /= 0
            then
               raise Error with "gunzip: (" & Integer'Image (Status) & ") " & Output;
            end if;
         end;

         declare
            tar_Options  : String_access := new String'("-xf");
            tar_Filename : String_access := new String'(Head (Filename, Filename'Length - 4) & ".tar");

            Status : aliased  Integer;
            Output : constant String := get_Command_Output (command    => "gtar",
                                                            arguments  => (1 => tar_Options,
                                                                           2 => tar_Filename),
                                                            input      => "",
                                                            status     => Status'Access,
                                                            err_to_out => True);
         begin
            free (tar_Options);
            free (tar_Filename);

            if Status /= 0
            then
               raise Error with "gtar: (" & Integer'Image (Status) & ") " & Output;
            end if;
         end;

      end if;

   end decompress;


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



   function current_User return String
   is
      use Posix,
          posix.process_Identification;
   begin
      return to_String (get_Login_Name);
   end current_User;



   function home_Folder (user_Name : in String) return String
   is
      use Posix,
          posix.User_Database;

      User_in_DB : constant User_Database_Item := get_User_Database_Item (to_Posix_String (user_Name));
   begin
      return to_String (initial_Directory_of (User_in_DB));
   end home_Folder;



   procedure remove_Folder (Named : in String)
   is
   begin
      ada.Directories.delete_Tree (Named);
   end remove_Folder;



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
