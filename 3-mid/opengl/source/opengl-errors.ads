package openGL.Errors
--
--  Provides utilities for displaying openGL errors.
--
is

   function  Current return String;
   --
   --  Returns a descriptive string of the last occurring openGL error.
   --  Returns "", when no error exists.
   --  Clears any existing error.

   procedure log (Prefix : in String := "");
   --
   --  Displays 'Current' error via 'ada.Text_IO.put_Line'.
   --  Clears any existing error.
   --  Raises 'openGL_Error' when an openGL error has been detected.

   procedure log (Prefix : in String := "";   Error_occurred : out Boolean);
   --
   --  Displays 'Current' error via 'ada.Text_IO.put_Line'.
   --  Clears any existing error.
   --  Sets 'Error_occurred' to true, if a GL error was detected.

end openGL.Errors;
