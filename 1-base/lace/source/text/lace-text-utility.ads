package lace.Text.utility
--
-- Provides utility subprograms.
--
is

   function  replace (Self : in     Text.item;   Pattern : in String;
                                                 By      : in String) return Text.item;
   --
   -- If the replacement exceeds the capacity of 'Self', the result will be expanded.

   procedure replace (Self : in out Text.item;   Pattern : in String;
                                                 By      : in String);
   --
   -- 'Text.Error' will be raised if the replacement exceeds the capacity of 'Self'.

end lace.Text.utility;
