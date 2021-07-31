with
     shell.Commands,

     ada.Strings.fixed,
     ada.Strings.Maps,
     ada.Characters.latin_1,
     ada.Exceptions;

package body lace.Environ.OS_Commands
is
   use ada.Exceptions;


   function Path_to (Command : in String) return Paths.Folder
   is
      use Paths;
   begin
      return to_Folder (run_OS ("which " & Command));
   end Path_to;


   procedure run_OS (command_Line : in String;
                     Input        : in String := "")
   is
      use Shell;
   begin
      Commands.run (command_Line, +Input);
   exception
      when E : Commands.command_Error =>
         raise Error with Exception_Message (E);
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
      when E : command_Error =>
         raise Error with Exception_Message (E);
   end run_OS;


   function run_OS (command_Line : in String;
                    Input        : in String := "") return Data
   is
      use Shell,
          Shell.Commands;
      the_Command : Command := Forge.to_Command (command_Line);
   begin
      return Output_of (run (The_Command, +Input));
   exception
      when E : command_Error =>
         raise Error with Exception_Message (E);
   end run_OS;


end lace.Environ.OS_Commands;
