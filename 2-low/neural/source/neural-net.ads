private
with
     neural.Set,
     fann_c.fann;



package neural.Net
--
-- Provides a neural net.
--
is

   type Item is tagged private;
   type View is access all Item;



   -- forge
   --

   procedure define (Self :    out Item;   Name               : in     String;
                                           Num_Input_Neurons  : in Positive;
                                           Num_Hidden_Neurons : in Natural;
                                           Num_Output_Neurons : in Positive);

   procedure define (Self :    out Item;   Name               : in     String);


   No_Net_Error : exception;
   --
   -- Raised if the named net does not exist, or is undefined.


   procedure Store (Self : in Item);




   -- attributes
   --

   function Name         (Self : in Item) return String;
   function Epoch        (Self : in Item) return Positive;

   function input_Count  (Self : in Item) return Positive;
   function output_Count (Self : in Item) return Positive;


   -- learning patterns
   --
   procedure Patterns_are           (Self : in out Item;   Now : in Neural.Patterns);

   function  training_Patterns      (Self : in     Item) return Neural.Patterns;
   function  test_Patterns          (Self : in     Item) return Neural.Patterns;

   function  training_pattern_Count (Self : in     Item) return Natural;



   -- client actions ~ user supplied actions performed during training.
   --
   type an_Action is access procedure (Self : not null access Item);

   type client_Action is
      record
         Act  : an_Action;
         Rate : Positive;
      end record;

   type client_Actions is array (Positive range <>) of client_Action;

   no_Actions : client_Actions (1..0);



   -- training
   --
   type Client_Continue_Test is access function return Boolean;


   procedure Train (Self : access Item;   Max_Epochs       : in Positive := 10_000;
                                          Acceptable_Error : in Signal   := 0.1;
                                          Learning_Rate    : in Signal   := 0.1;

                                          Client_Actions   : in Neural.Net.Client_Actions := No_Actions;
                                          Client_Continue  : in Client_Continue_Test      := null);
     --
     -- Acceptable_Error: Error level at which the network is considered to be converged.
     -- Calculates the Training_RMS_Error.


   function  Is_Training            (Self : in     Item) return Boolean;

   procedure report_training_Status (Self : not null access Item);

   function  training_RMS_Error     (Self : in     Item) return Signal;

   function  test_RMS_Error         (Self : in     Item) return Signal;
   function  min_test_RMS_Error     (Self : in     Item) return Signal;

   procedure store_best_Epoch       (Self : access Item);
   procedure store_last_Epoch       (Self : access Item);




   -- response
   --

   function Response (Self : in Item;   To : in Neural.Pattern) return Signals;
     --
     -- Returns the net's output response for the pattern.
     --
     -- Calculates the Response_RMS_Error (error between desired outputs in the patterns and
     -- their corresponding response outputs).


   function Response_RMS_Error (Self : in Item) return Signal;






private

   type String_view is access String;


   type Item is tagged
      record
         Name               : String_view;

         Num_Input_Neurons  : Positive;
         Num_Hidden_Neurons : Natural;
         Num_Output_Neurons : Positive;

         Training_Patterns  : Neural.Set.Patterns_view;
         training_Set       : neural.Set.Set;

         Test_Patterns      : Neural.Set.Patterns_view;
         testing_Set        : neural.Set.Set;

         Epoch              : Natural;

         Training_RMS_Error : Signal := 0.0;
         Test_RMS_Error     : Signal := 0.0;
         Min_Test_RMS_Error : Signal := Signal'Last;

         Response_RMS_Error : Signal := 0.0;

         Is_Undefined       : Boolean := True;
         Is_Training        : Boolean := False;

         Fann               : fann_c.Fann.pointer;
      end record;


end Neural.Net;
