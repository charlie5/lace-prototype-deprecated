package lace.Any
--
--  Provides a base class for 'any' other class.
--  Allows for heteroegenous containers.
--  Similar, in intent, to the 'void*' of C (for Ada tagged types).
--
is
   pragma Pure;

   type         Item is         interface;
   type limited_Item is limited interface;
end lace.Any;
