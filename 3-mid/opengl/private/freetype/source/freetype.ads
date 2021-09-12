package Freetype
--
-- A thick bindng to the 'Freetype' font library.
--
is
   pragma Pure;

   Error : exception;

   type Vector_3 is array (Positive range 1 .. 3) of Float;

end Freetype;
