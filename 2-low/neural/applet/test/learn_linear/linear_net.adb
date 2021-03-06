
with Neural.Set;
with Neural.Net;

--  with opengl.IO;

--  with Math;

--  with lace.text.Cursor;
with lace.Environ;


with ada.Strings.Fixed;
with ada.Text_Io;



package body linear_Net is


   use Math, Ada.Text_IO;
   use type -- opengl.Real,
       neural.signal;


--     input_neuron_Count : constant := 1;


   subtype nn_Real is neural.signal;




   -- definition
   --

   procedure define (Self :    out Item;
                     Name : in     String)
   is
      use Neural.Net, Neural, lace.Environ;

--        the_Environ : lace.Environ.item;
      net_Name    : String           := Name & ".net";
   begin
      lace.Environ.verify_Folder (net_Name);
      define (neural.Net.Item (Self),  net_Name);        -- define base class

   exception
      when No_Net_Error =>
         Put_Line ("No valid '" & Name & "' net found ... creating new net.");

         Self.define (Name & ".net",
                      num_Input_Neurons  =>  1,
                      num_Hidden_Neurons => 25,
                      num_Output_Neurons =>  1);
   end define;




   function Pattern_For (Self : in Item;   the_Input     : in math.Real;
                                           Inputs_Only   : in Boolean        := False;
                                           the_Output    : in math.Real      := 0.0  ) return Neural.Pattern
   is
      use Neural;
--            opengl, opengl.IO;

      the_Inputs  : Signals (1 .. self.input_Count);
      the_Outputs : Signals (1 .. self.output_Count);
   begin
      the_Inputs (1) := nn_Real (the_Input);

      if not Inputs_only
      then
         the_Outputs (1) := Signal (the_Output);
      end if;

      return (Inputs  => new Signals'(the_Inputs),
              Outputs => new Signals'(the_Outputs));
   end Pattern_For;




   function Output_for (Self : access Item;    the_Input     : in math.Real) return math.Real
   is
      the_Pattern  : neural.Pattern := Pattern_for   (Self.all, the_Input, inputs_only => True);
      the_response : neural.Signals := self.Response (the_Pattern);
   begin
      return math.Real (the_Response (1));
   end Output_for;


end linear_Net;
