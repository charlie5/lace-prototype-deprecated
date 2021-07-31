with
     posix.Permissions,
     ada.Streams;

package lace.Environ
--
-- Models an operating system environment.
--
is
   use posix.Permissions;
   function to_octal_Mode (Permissions : in Permission_Set) return String;

   subtype Data is ada.Streams.Stream_Element_Array;

   Error : exception;

end lace.Environ;
