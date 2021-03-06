

--  with Neural.Pattern;
with Neural.Net;

--  with float_Math;



package linear_Net
is
   package Math renames Neural.Math;


   type Item  is new        neural.Net.Item with private;
   type View  is access all Item;



   procedure define (Self :    out Item;
                     Name : in     String);



   function Pattern_For (Self : in Item;   the_Input     : in math.Real;
                                           Inputs_Only   : in Boolean        := False;
                                           the_Output    : in math.Real      := 0.0  ) return Neural.Pattern;


   function Output_for (Self : access Item;    the_Input     : in math.Real) return math.Real;



private

   type Item is new Neural.Net.Item with
      record
         null;
      end record;


end linear_Net;
