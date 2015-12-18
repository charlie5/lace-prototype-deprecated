with
     posix.User_Database,
     posix.Process_Identification,

     gnat.Expect,

     ada.Strings.fixed,
     ada.Directories,
     ada.environment_Variables,
     ada.Text_IO,

     system.OS_Lib,
     system.Strings;


package body lace.Environ
is

   use ada.Text_IO;


   function Argument_String_To_List (Arg_String : String) return System.OS_Lib.Argument_List_Access
   is
      use System.OS_Lib;

      Max_Args : constant Integer                      := Arg_String'Length;
      New_Argv :          Argument_List (1 .. Max_Args);
      New_Argc :          Natural                      := 0;
      Idx      :          Integer;

   begin
      Idx := Arg_String'First;

      loop
         exit when Idx > Arg_String'Last;

         declare
            Quoted  : Boolean := False;
            Backqd  : Boolean := False;
            Old_Idx : Integer;

         begin
            Old_Idx := Idx;

            loop
               --  An unquoted space is the end of an argument

               if         not (Backqd or Quoted)
                 and then Arg_String (Idx) = ' '
               then
                  exit;

               --  Start of a quoted string

               elsif      not (Backqd or Quoted)
                 and then Arg_String (Idx) = '"'
               then
                  Quoted := True;

               --  End of a quoted string and end of an argument

               elsif      (Quoted and not Backqd)
                 and then Arg_String (Idx) = '"'
               then
                  Idx := Idx + 1;
                  exit;

               --  Following character is backquoted

               elsif Arg_String (Idx) = '\'
               then
                  Backqd := True;

               --  Turn off backquoting after advancing one character

               elsif Backqd
               then
                  Backqd := False;

               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument

            New_Argc            := New_Argc + 1;
            New_Argv (New_Argc) := new String'(Arg_String (Old_Idx .. Idx - 1));

            --  Skip extraneous spaces

            while      Idx             <= Arg_String'Last
              and then Arg_String (Idx) = ' '
            loop
               Idx := Idx + 1;
            end loop;
         end;
      end loop;


      return new Argument_List' (New_Argv (1 .. New_Argc));
   end Argument_String_To_List;





   function Path_to (the_Command : in String) return String   -- tbd: use os_lib procedure instead !
   is
      use GNAT.Expect,  System.OS_Lib;

      the_Status     : aliased  Integer;
      the_Args       :          Argument_List_Access := Argument_String_To_List (the_Command);
      command_Output : constant String               := get_Command_Output (command    => "/usr/bin/which",
                                                                            arguments  => the_Args.all,
                                                                            input      => "",
                                                                            status     => the_Status'Access,
                                                                            err_to_out => True);
   begin
      free (the_Args);
      return command_Output;
   end Path_to;




   function shell_Output_of (the_Command : in String) return String
   is
      use ada.Environment_Variables,
          ada.Strings.Fixed;

      the_Path     : constant String               := Current_Folder & "/.lace-bin/";
      the_FileName : constant String               := "lace_environ_temporary_shell.sh";    -- tbd: Add a unique number here so simultaneous calls don't tread on each other.
      the_File     :          ada.Text_IO.File_Type;

   begin
      verify_Folder (the_Path);

      if Index (Value ("PATH"), the_Path) = 0
      then
         set ("PATH",
              the_Path & ":" & Value ("PATH"));
      end if;

      create   (the_File,  out_File,  the_Path & the_Filename);
      put_Line (the_File,  the_Command);
      close    (the_File);

      change_Mode (the_Path & the_Filename, to => "a+rwx");

      return Output_of (the_command => the_Filename);
   end shell_Output_of;





   function Output_of (the_Command : in String;
                       Input       : in String := "") return String
   is
      use ada.Strings.Fixed;

      pipe_Index  : constant Natural := Index (the_Command, "|");
      the_Status  : aliased  Integer;
      space_Index : constant Natural := Index (the_Command, " ");


      function Command return String
      is
      begin
         if space_Index = 0
         then
            return the_Command;
         else
            return the_Command (the_Command'First .. space_Index-1);
         end if;
      end Command;

   begin
      if pipe_Index = 0
      then
         declare
            use GNAT.Expect, System.OS_Lib;

            function Arguments return String
            is
            begin
               if space_Index = 0
               then
                  return "";
               else
                  return the_Command (space_Index+1 .. the_Command'Last);
               end if;
            end Arguments;

            arg_List   :          Argument_List_Access := Argument_String_To_List (Arguments);
            the_Output : constant String               := get_Command_Output (command    => Path_to (Command),
                                                                              arguments  => arg_List.all,
                                                                              input      => Input,
                                                                              status     => the_Status'Access,
                                                                              err_to_out => True);
         begin
            free (arg_List);
            return the_Output;
         end;

      else
         declare
            Command_1 : constant String := the_Command (the_Command'First .. pipe_Index-1);
            Command_2 : constant String := the_Command (pipe_Index+1      .. the_Command'Last);
         begin
            return Output_of (Command_2, input => Output_of (Command_1));
         end;
      end if;

   end Output_of;




   function Expand (the_File_GLOB : in String) return String
   is
      use ada.Strings.Fixed;
      use GNAT.Expect, System.OS_Lib;

      the_Path     : constant String               := "/usr/local/bin/";
      the_FileName : constant String               := "lace_environ_temporary_shell.sh";
      the_File     :          ada.Text_IO.File_Type;

   begin
      create   (the_File,  out_File,  the_Path & the_Filename);
      put_Line (the_File,  "echo " & the_File_GLOB);
      close    (the_File);

      change_Mode (the_Path & the_Filename, to => "a+rwx");

      declare
         the_Status : aliased  Integer;
         the_Arg    : constant system.strings.String_access := new String' (the_Path & the_Filename);
         the_Output : constant String                       := get_Command_Output (command    => Path_to ("bash"),
                                                                                   arguments  => (1 => the_Arg),
                                                                                   input      => "",
                                                                                   status     => the_Status'Access,
                                                                                   err_to_out => True);
      begin
         return the_Output;
      end;
   end Expand;



   procedure set_Password (for_User : in String)
   is
      use GNAT.Expect;


      the_Status     : aliased  Integer;
      the_Username   : constant system.strings.String_access := new String' (for_User);
      command_Output : constant String                       := get_Command_Output (command    => "/bin/passwd",
                                                                                    arguments  => (1 => the_Username),
                                                                                    input      => "",
                                                                                    status     => the_Status'Access,
                                                                                    err_to_out => True);
   begin
      if command_Output (command_Output'First .. command_Output'First + 16) /= "Changing password"
      then
         raise Error with command_Output;
      end if;
   end set_Password;



   procedure add_User (Name : in String;   Super : in Boolean := False)
   is
      use GNAT.Expect;


      function command_Output return String
      is
         the_Status     : aliased  Integer;
         the_Username   : constant system.strings.String_access := new String' (Name);

         Arg_1          : constant system.strings.String_access := new String' ("-m");
         Arg_2          :          system.strings.String_access;
         Arg_3          :          system.strings.String_access;
         Arg_4          :          system.strings.String_access;
         Arg_5          :          system.strings.String_access;

      begin
         if Super
         then
            Arg_2 :=  new String' ("-G");
            Arg_3 :=  new String' ("sudo");

            Arg_4 :=  new String' ("-G");
            Arg_5 :=  new String' ("root");

            return get_Command_Output (command    => "/usr/sbin/useradd",
                                       arguments  => (1 => the_Username,
                                                      2 => Arg_1,
                                                      3 => Arg_2,
                                                      4 => Arg_3,
                                                      5 => Arg_4,
                                                      6 => Arg_5),
                                       input      => "",
                                       status     => the_Status'Access,
                                       err_to_out => True);
         else
            return get_Command_Output (command    => "/usr/sbin/useradd",
                                       arguments  => (1 => the_Username,
                                                      2 => Arg_1),
                                       input      => "",
                                       status     => the_Status'Access,
                                       err_to_out => True);
         end if;
      end command_Output;

   begin
      if command_Output /= ""
      then
         raise Error with command_Output;
      end if;
   end add_User;



   procedure rid_User (Name : in String)
   is
      use GNAT.Expect;

      the_Status     : aliased  Integer;
      the_Username   : constant system.strings.String_access := new String' (Name);
      Arg_1          : constant system.strings.String_access := new String' ("-r");
      command_Output : constant String                       := get_Command_Output (command    => "/usr/sbin/userdel",
                                                                                    arguments  => (1 => the_Username,
                                                                                                   2 => Arg_1),
                                                                                    input      => "",
                                                                                    status     => the_Status'Access,
                                                                                    err_to_out => True);
   begin
      if command_Output /= ""
      then
         raise Error with command_Output;
      end if;
   end rid_User;




   procedure change_Mode (Folder : in String;
                          To     : in String)
   is
      use GNAT.Expect;

      the_Status     : aliased  Integer;

      the_Folder     : constant system.strings.String_access := new String' (Folder);
      Arg_1          : constant system.strings.String_access := new String' ("-R");
      Arg_2          : constant system.strings.String_access := new String' (To);

      command_Output : constant String                       := get_Command_Output (command    => "/bin/chmod",
                                                                                    arguments  => (1 => Arg_1,
                                                                                                   2 => Arg_2,
                                                                                                   3 => the_Folder),
                                                                                    input      => "",
                                                                                    status     => the_Status'Access,
                                                                                    err_to_out => True);
   begin
      if command_Output /= ""
      then
         raise Error with command_Output;
      end if;
   end change_Mode;



   procedure change_Owner (Folder         : in String;
                           To             : in String)
   is
      use GNAT.Expect;

      the_Status     : aliased  Integer;

      the_Folder     : constant system.strings.String_access := new String' (Folder);
      Arg_1          : constant system.strings.String_access := new String' ("-R");
      Arg_2          : constant system.strings.String_access := new String' (To);

      command_Output : constant String                       := get_Command_Output (command    => "/bin/chown",
                                                                                    arguments  => (1 => Arg_1,
                                                                                                   2 => Arg_2,
                                                                                                   3 => the_Folder),
                                                                                    input      => "",
                                                                                    status     => the_Status'Access,
                                                                                    err_to_out => True);
   begin
      if command_Output /= ""
      then
         raise Error with command_Output;
      end if;
   end change_Owner;



   function Exists (Folder : in String) return Boolean
   is
   begin
      return ada.Directories.Exists (Folder);
   end Exists;



   procedure touch (fileName : in String)
   is
      use ada.Strings.fixed,  gnat.Expect;

      tar_Filename        : constant system.strings.String_access := new String'(fileName);
      the_Status          : aliased  Integer;
      command_Output      : constant String                       := get_Command_Output (command    => "/usr/bin/touch",
                                                                                         arguments  => (1 => tar_Filename),
                                                                                         input      => "",
                                                                                         status     => the_Status'Access,
                                                                                         err_to_out => True);
   begin
      if command_Output = ""
      then
         put_Line ("   touch: ok");
      else
         put_Line ("   touch: " & command_Output);
      end if;
   end touch;



   procedure switch_to_User (Named : in String)
   is
      use Posix,  Posix.User_Database,  posix.Process_Identification;

      User_in_DB : constant User_Database_Item := Get_User_Database_Item (to_Posix_String (Named));
      the_User   : constant User_ID            := User_ID_Of (User_in_DB);

   begin
      Set_User_ID (the_User);
   end switch_to_User;






   procedure save (the_Text      : in String;
                   to_Filename   : in String)
   is
      use ada.Text_IO;

      the_File : File_Type;
   begin
      create (the_File, Out_File, to_Filename);
      put    (the_File, the_Text);
      close  (the_File);
   end save;



   procedure decompress (the_Filename  : in String)
   is
      use ada.Strings.fixed,  gnat.Expect;
   begin
      if Tail (the_Filename, 7) = ".tar.gz"
      then
         declare
            tar_Options         : constant system.strings.String_access := new String'("-xf");
            tar_Filename        : constant system.strings.String_access := new String'(the_Filename);
            the_Status          : aliased  Integer;
            command_Output      : constant String                       := get_Command_Output (command    => "tar",
                                                                                               arguments  => (1 => tar_Options,
                                                                                                              2 => tar_Filename),
                                                                                               input      => "",
                                                                                               status     => the_Status'Access,
                                                                                               err_to_out => True);
         begin
            if command_Output = ""
            then
               put_Line ("   tar: ok");
            else
               put_Line ("   tar: " & command_Output);
            end if;
         end;

      elsif Tail (the_Filename, 8) = ".tar.bz2"
      then
         declare
            bunzip_Filename     : constant system.strings.String_access := new String'(the_Filename);
            the_Status          : aliased Integer;
            command_Output      : constant String                       := get_Command_Output (command    => "bunzip2",
                                                                                               arguments  => (1 => bunzip_Filename),
                                                                                               input      => "",
                                                                                               status     => the_Status'Access,
                                                                                               err_to_out => True);
         begin
            if command_Output = ""
            then
               put_Line ("   bunzip: ok");
            else
               put_Line ("   bunzip: " & command_Output);
            end if;
         end;

         declare
            tar_Options         : constant system.strings.String_access := new String'("-xf");
            tar_Filename        : constant system.strings.String_access := new String'(Head (the_Filename,  the_Filename'Length - 4));
            the_Status          : aliased  Integer;
            command_Output      : constant String                       := get_Command_Output (command    => "gtar",
                                                                                               arguments  => (1 => tar_Options,
                                                                                                              2 => tar_Filename),
                                                                                               input      => "",
                                                                                               status     => the_Status'Access,
                                                                                               err_to_out => True);
         begin
            if command_Output = ""
            then
               put_Line ("   gtar: ok");
            else
               put_Line ("   gtar: " & command_Output);
            end if;
         end;


      elsif Tail (the_Filename, 4) = ".tgz"
      then
         declare
            gunzip_Filename     : constant system.strings.String_access := new String'(the_Filename);
            the_Status          : aliased  Integer;
            command_Output      : constant String                       := get_Command_Output (command    => "gunzip",
                                                                                               arguments  => (1 => gunzip_Filename),
                                                                                               input      => "",
                                                                                               status     => the_Status'Access,
                                                                                               err_to_out => True);
         begin
            if command_Output = ""
            then
               put_Line ("   gunzip: ok");
            else
               put_Line ("   gunzip: " & command_Output);
            end if;
         end;

         declare
            tar_Options         : constant system.strings.String_access := new String'("-xf");
            tar_Filename        : constant system.strings.String_access := new String'(Head (the_Filename,  the_Filename'Length - 4) & ".tar");
            the_Status          : aliased  Integer;
            command_Output      : constant String                       := get_Command_Output (command    => "gtar",
                                                                                               arguments  => (1 => tar_Options,
                                                                                                              2 => tar_Filename),
                                                                                               input      => "",
                                                                                               status     => the_Status'Access,
                                                                                               err_to_out => True);
         begin
            if command_Output = ""
            then
               put_Line ("   gtar: ok");
            else
               put_Line ("   gtar: " & command_Output);
            end if;
         end;

      end if;

   end decompress;




   --- Paths
   --

   procedure link (From, To : in String)
   is
      use Posix;

      Unused : String := Output_of ("ln -s " & From & " " & To);
   begin
      null;
   end link;





   --- Folders
   --

   function  current_Folder return String
   is
   begin
      return ada.Directories.Current_Directory;
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
         ada.Directories.Set_Directory (To);
         Locked := True;
      end change;

      procedure clear
      is
      begin
         Locked := False;
      end clear;
   end folder_Lock;



   procedure goto_Folder (Named     : in String;
                          Lock      : in Boolean)
   is
   begin
      if Lock
      then
         folder_Lock.change (Named);
      else
         ada.Directories.Set_Directory (Named);
      end if;
   end goto_Folder;



   procedure unlock_Folder
   is
   begin
      folder_Lock.clear;
   end unlock_Folder;





   function  current_User return String
   is
      use Posix, posix.Process_Identification;
   begin
      return to_String (get_Login_Name);
   end current_User;




   function  home_Folder (user_Name : in String) return String
   is
      use Posix,  Posix.User_Database,  posix.Process_Identification;

      User_in_DB : constant User_Database_Item := Get_User_Database_Item (to_Posix_String (user_Name));
   begin
      return to_String (Initial_Directory_of (User_in_DB));
   end home_Folder;




   procedure remove_Folder (Named        : in String)
   is
   begin
      ada.Directories.Delete_Tree (Named);
   end remove_Folder;




   procedure verify_Folder (Named : in String)
   is
      use ada.Directories;
   begin
      create_Path (Named);
   end verify_Folder;




   function to_octal_Mode (Self : in POSIX.Permissions.Permission_Set) return String
   is
      use posix.process_Identification, posix.permissions;

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
          octal_Permissions (self (set_User_ID), self (set_Group_ID), False)
        & octal_Permissions (self (owner_Read),  self (owner_Write),  self (owner_Execute))
        & octal_Permissions (self (group_Read),  self (group_Write),  self (group_Execute))
        & octal_Permissions (self (others_Read), self (others_Write), self (others_Execute));
   end to_octal_Mode;


end lace.Environ;
