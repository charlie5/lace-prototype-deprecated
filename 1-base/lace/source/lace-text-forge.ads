
package lace.Text.Forge
--
-- Provides constructors for Text.
--
is

   type Filename is new String;

   function to_String (Filename : in forge.Filename) return String;
   function to_Text   (Filename : in forge.Filename) return Item;

end lace.Text.Forge;
