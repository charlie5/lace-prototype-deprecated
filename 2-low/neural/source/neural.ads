
with float_Math;


package Neural
--
-- Declares the 'neural' namespace and core types.
--
is
   pragma Pure;


   package Math renames float_Math;


   subtype Signal  is math.Real;
   type    Signals is array (Positive range <>) of neural.Signal;



   type Pattern is
      record
         Inputs  : access Signals;
         Outputs : access Signals;
      end record;

   type Patterns is array (Positive range <>) of Pattern;





private


   procedure dummy;


end Neural;
