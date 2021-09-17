with
     lace.Environ.Paths;

package lace.Environ.OS_Commands
--
-- Allows running of operating system commands.
--
is

   function  Path_to (Command : in String) return Paths.Folder;

   procedure run_OS  (command_Line : in String;
                      Input        : in String := "");
   --
   -- Discards any output. Error is raised when the command fails.

   function  run_OS  (command_Line : in String;
                      Input        : in String := "") return Data;
   --
   -- Returns any output. Error is raised when the command fails.

   function  run_OS  (command_Line : in String;
                      Input        : in String  := "";
                      add_Errors   : in Boolean := True) return String;
   --
   -- Returns any output. Error output is appended if add_Errors is true.


   function Executable_on_Path (Executable : Paths.File) return Boolean;
   --
   -- Returns True if the Executable exists on the environment PATH variable.

end lace.Environ.OS_Commands;
