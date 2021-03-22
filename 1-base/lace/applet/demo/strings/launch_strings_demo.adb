with
     lace.Strings.bounded,
     ada.Text_IO;

procedure launch_strings_Demo
--
-- Displays a string message in a Pure unit.
--
is
   use ada.Text_IO;

   package Text is new lace.Strings.Bounded.Generic_Bounded_Length (Max => 64);
   use     Text;

   the_String : bounded_String := to_bounded_String ("Howdy ...");

begin
   append   (the_String, " doody !");
   put_Line (to_String (the_String));
end launch_strings_Demo;
