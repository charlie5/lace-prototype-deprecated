
with neural.Set;



package Neural.Forge
--
-- Declares a factory for creation/destruction of core types.
--
is


   -- Pattern
   --

   function  Creation (From : in     Pattern) return Pattern;
   procedure Destroy  (Self : in out Pattern);




   -- Patterns
   --

   function  Creation (From_File_Named : in String) return neural.Set.Patterns_view;

   procedure Destroy (Self : in out Patterns);
   procedure Destroy (Self : in out neural.Set.Patterns_view);


   procedure Store   (Self : in     Patterns;    In_File_Named : in String);


end Neural.Forge;
