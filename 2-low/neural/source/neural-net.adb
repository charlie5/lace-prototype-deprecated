with interfaces.c.pointers;

with fann_c.Binding;   use fann_c.Binding;
with neural.Privvy;    use neural.Privvy;
with neural.Forge;     use neural.Forge;

with Interfaces.C;  use Interfaces.C;


with Ada.Text_Io;     use Ada.Text_IO;
with Ada.IO_Exceptions;

with Interfaces.C.Strings; use Interfaces.C.Strings;
with fann_c.Pointers; use fann_c.Pointers;


package body Neural.Net
--
--
--
is

   use Interfaces, Ada;




   function input_Count (Self : in Item) return Positive
   is
   begin
      return self.num_Input_Neurons;
   end input_Count;



   function output_Count (Self : in Item) return Positive
   is
   begin
      return self.num_Output_Neurons;
   end output_Count;



   function Folder_For (Self : in Item) return String
   is
--        use Environ;
      The_Folder : constant String := "./" & Self.Name.all & "/"; -- & ".net/";
   begin
--        Verify_Folder (The_Folder);
      return The_Folder;
   end Folder_For;



   function State_Filename_For (Self : in Item) return String
   is
   begin
      return Folder_For (Self) & "neural_state";
   end State_Filename_For;
   pragma Unreferenced (State_Filename_For);



   function Weights_Filename_For (Self : in Item) return String
   is
   begin
      return Folder_For (Self) & "neural_weights";
   end Weights_Filename_For;
   pragma Unreferenced (Weights_Filename_For);



   function Net_Filename_For (Self : in Item) return String
   is
   begin
      return Folder_For (Self) & "neural_net";
   end Net_Filename_For;





   function Training_Patterns_Filename_For (Self : in Item) return String
   is
   begin
      return Folder_For (Self) & "training.patterns";
   end Training_Patterns_Filename_For;





   function Test_Patterns_Filename_For (Self : in Item) return String
   is
   begin
      return Folder_For (Self) & "test.patterns";
   end Test_Patterns_Filename_For;







   procedure define (Self :    out Item;
                     Name : in     String;

                     Num_Input_Neurons  : in Positive;
                     Num_Hidden_Neurons : in Natural;
                     Num_Output_Neurons : in Positive)
   is
   begin
      Self.Name     := new String (Name'Range);  -- tbd: Memory leak.
      Self.Name.all := Name;

      Self.Num_Input_Neurons  := Num_Input_Neurons;
      Self.Num_Hidden_Neurons := Num_Hidden_Neurons;
      Self.Num_Output_Neurons := Num_Output_Neurons;

      Self.Epoch := 0;

      declare
         num_Layers : constant := 3;
         layer_Size : array (1 .. num_Layers) of aliased c.unSigned := (c.unsigned (self.Num_Input_Neurons),
                                                                        c.unsigned (Self.Num_Hidden_Neurons),
                                                                        c.unsigned (Self.Num_output_Neurons));
      begin
         self.Fann := fann_create_standard_array (num_layers, layer_Size (1)'unchecked_access);
      end;

      Self.Is_Undefined := False;
   end Define;





   procedure define (Self :    out Item;
                     Name : in     String)
   is
      use Set;

      Update_Count : long_long_Integer;

      the_File : File_Type;
   begin
      Self.Name := new String' (Name);

      declare
         subtype nn_Real is math.Real;
      begin
--           put_line ("Opening " &  net_Filename_for (Self));

         open (the_File, in_File, net_Filename_for (Self));


         self.Num_Input_Neurons  := Integer'Value (get_Line (the_File));
         self.Num_Hidden_Neurons := Integer'Value (get_Line (the_File));
         self.Num_Output_Neurons := Integer'Value (get_Line (the_File));

         self.Epoch   := Integer          'Value (get_Line (the_File));
--       Update_Count := long_long_Integer'Value (get_Line (the_File));

         self.Training_RMS_Error := nn_Real'Value (get_Line (the_File));
         self.Min_Test_RMS_Error := nn_Real'Value (get_Line (the_File));
         self.Response_RMS_Error := nn_Real'Value (get_Line (the_File));

         close (the_File);
      end;

      self.Fann := fann_create_from_file (new_String (net_Filename_for (Self) & ".fann"));


      -- Patterns.
      --

      --- Training patterns.
      --
      begin
         destroy (Self.Training_Patterns);
         self.Training_Patterns := forge.Creation (From_File_Named => Training_Patterns_Filename_For (Self));
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Put_Line ("No training patterns for " & Self.Name.all);    -- TBD.
      end;



      --- Test patterns.
      --
      begin
         Destroy (Self.Test_Patterns);
         Self.Test_Patterns := forge.Creation (From_File_Named => Test_Patterns_Filename_For (Self));
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Put_Line ("No test patterns for " & Self.Name.all);    -- TBD.
      end;


      Self.Is_Undefined := False;

   exception
      when The_Error : others =>
         raise No_Net_Error;
   end Define;



   procedure Store (Self : in Item)
   is
      use Set;

      The_File : File_Type;
      Status   : c.int;
      pragma Unreferenced (Status);
   begin
--        put_Line ("Storing the net in '" & Net_Filename_For (Self) & "'.");

      Create (The_File, Out_File, Net_Filename_For (Self));

      Put_Line (The_File, Positive'Image (Self.Num_Input_Neurons));
      Put_Line (The_File, Positive'Image (Self.Num_Hidden_Neurons));
      Put_Line (The_File, Positive'Image (Self.Num_Output_Neurons));


      Put_Line (The_File, Natural'Image (Self.Epoch));

      Put_Line (The_File,   neural.Signal'Image (self.Training_RMS_Error));
      Put_Line (The_File,   neural.Signal'Image (self.Min_Test_RMS_Error));
      Put_Line (The_File,   neural.Signal'Image (self.Response_RMS_Error));

      Close (The_File);


--        put_Line ("Storing FANN.");
      status := fann_save (self.Fann, new_String (net_Filename_for (Self) & ".fann"));

      Store (Self.Training_Patterns.all, Training_Patterns_Filename_For (Self));
      Store (Self.Test_Patterns.all,     Test_Patterns_Filename_For (Self));

   end Store;






   function Name (Self : in Item) return String
   is
   begin
      return Self.Name.all;
   end Name;





   procedure Patterns_are (Self : in out Item;    Now : in     Neural.Patterns)
   is
      use Set;

      All_Patterns          : constant Patterns (1 .. Now'length) := Now;
      Num_Training_Patterns : constant Natural                    := All_Patterns'Length * 2 / 3;
      Num_Test_Patterns     : constant Natural                    := All_Patterns'Length - Num_Training_Patterns;

      Status : C.int;
   begin
--        -- shuffle patterns
--        --
--        declare
--           subtype Placeholder is Positive range 1 .. all_Patterns'Last;
--
--           package Random is new Numerics.Discrete_Random (Placeholder);
--
--           Temp  : Neural.Pattern.Instance;
--           Index : Positive;
--           Gen   : Random.Generator;
--        begin -- Shuffle
--           Random.Reset (Gen);
--
--           Permute_All:
--           for I in 1 .. all_Patterns'Last loop
--              Index := Random.Random (Gen);
--
--              Temp                 := all_Patterns (I);
--              all_Patterns (I)     := all_Patterns (Index);
--              all_Patterns (Index) := Temp;
--           end loop
--           Permute_All;
--        end;



      -- Training patterns.
      --
      destroy (Self.Training_Patterns);

      Self.Training_Patterns := new Patterns'(All_Patterns (1 .. Num_Training_Patterns));
      self.training_Set      := to_Set (self.Training_Patterns);

      Status := fann_set_scaling_params (ann            => self.Fann,
                                         data           => self.training_Set.Fann,
                                         new_input_min  => -1.0,
                                         new_input_max  =>  1.0,
                                         new_output_min => -1.0,
                                         new_output_max =>  1.0);

      put_Line ("Scaling status: "  & c.int'Image (Status));

      fann_scale_train (ann  => self.Fann,
                        data => self.training_Set.Fann);

      declare
         use fann_c;

         type my_array is array (c.int range <>) of aliased fann_type;

         package pointers is new interfaces.c.pointers (Index              => c.int,
                                                        Element            => fann_type,
                                                        Element_Array      => my_array,
                                                        Default_Terminator => 0.0);
--         kkk : array (1 .. Num_Training_Patterns) of fann_type;
--         for kkk'Address use Self.training_Set.Fann.input.all'Address;

         kkk : constant my_array (1 .. c.int (Num_Training_Patterns)) := Pointers.Value (Pointers.pointer (Self.training_Set.Fann.input.all),
                                                                                c.ptrdiff_t (Num_Training_Patterns));
      begin

         for i in kkk'range
         loop
            null; -- put_Line (math.Image (math.real (kkk (i))));  -- Value (Self.training_Set.Fann.input)));
         end loop;
      end;

--        Free (Self.Training_Response);
--        Self.Training_Response := Signal_Sets.Creation (Self.Training_Patterns'Length,
--                                                        Self.Num_Output_Neurons);


      -- Test patterns.
      --
      destroy (Self.Test_Patterns);

      Self.Test_Patterns                          := new Patterns (1 .. Num_Test_Patterns);
      Self.Test_Patterns (1 .. Num_Test_Patterns) := All_Patterns (Num_Training_Patterns + 1 .. All_Patterns'Last);
      self.testing_Set                            := to_Set (self.Test_Patterns);

      fann_scale_train (ann  => self.Fann,
                        data => self.testing_Set.Fann);


--        Free (Self.Test_Response);
--        Self.Test_Response := Signal_Sets.Creation (Self.Test_Patterns'Length,
--                                                    Self.Num_Output_Neurons);

   end Patterns_are;





   procedure report_training_Status (Self : not null access Neural.Net.Item)
   is
      use Math;
   begin
      Put (Name (Self.all) & " epoch: " & Integer'Image (Epoch (Self.all)));
      Put ("    Training RMS error: "    & Image (math.Real (training_RMS_Error (Self.all))));
      Put ("    Test RMS error: "        & Image (math.Real (test_RMS_Error     (Self.all))));
      Put ("  (Min Test RMS error: "     & Image (math.Real (min_test_RMS_Error (Self.all))));
      Put (")");
      New_Line;
   end report_training_Status;





   procedure store_best_Epoch (Self    : access Neural.Net.Item)
   is
   begin
--        if        Epoch (Self.all) > 100
--          and then test_RMS_Error (Self.all) <= min_test_RMS_Error (Self.all)
--        then
         Store (Self.all);
--        end if;
   end store_best_Epoch;



   procedure store_last_Epoch (Self : access Neural.Net.Item)
   is
   begin
--        if Epoch (Self.all) > 100
--        then
         Store (Self.all);
--        end if;
   end store_last_Epoch;



   procedure Train (Self : access Item;   Max_Epochs       : in Positive := 10_000;
                                          Acceptable_Error : in Signal   := 0.1;
                                          Learning_Rate    : in Signal   := 0.1;

                                          Client_Actions   : in Neural.Net.Client_Actions := No_Actions;
                                          Client_Continue  : in Client_Continue_Test      := null)
   is
      use Neural.Set;

      Training_Patterns : Patterns_view := Self.Training_Patterns;
      Test_Patterns     : Patterns_view := Self.Test_Patterns;

   begin
      Self.Is_Training := True;

--        Nnet.Learning_Rate_For (Self.Net.all, Is_Now => Learning_Rate);

      fann_set_activation_function_hidden (self.Fann, fann_c.FANN_SIGMOID_SYMMETRIC);
      fann_set_activation_function_output (self.Fann, fann_c.FANN_LINEAR);
--        fann_set_activation_function_output (self.Fann, fann_c.FANN_SIGMOID);
      fann_set_training_algorithm         (self.Fann, fann_c.FANN_TRAIN_RPROP);

      All_Epochs:
      for Epoch in 1 .. Max_Epochs
      loop
         Self.Epoch              := Epoch;
         self.Training_RMS_Error := Signal (fann_train_epoch (self.Fann, self.training_Set.Fann));
--           self.Training_RMS_Error := Signal (fann_train_epoch_irpropm_parallel (self.Fann, self.training_Set.Fann, 13));

--           self.Test_RMS_Error := 1.0;

--           if Epoch mod 10 = 1
--           then
            self.Test_RMS_Error     := Signal (fann_test_data (self.Fann, self.testing_Set.Fann));
            self.Min_Test_RMS_Error := Signal'Min (Self.Test_RMS_Error,  Self.Min_Test_RMS_Error);
--           end if;


         for Each in Client_Actions'Range
         loop
            if Epoch mod Client_Actions (Each).Rate = 0
            then
               Client_Actions (Each).Act.all (Self);
            end if;
         end loop;

         exit All_Epochs
         when  Epoch > 50
           and (   Self.Test_RMS_Error <= Acceptable_Error
                or (             Client_Continue /= null
                    and then not Client_Continue.all));
      end loop All_Epochs;

      Self.Is_Training := False;
   end Train;




   function Is_Training (Self : in Item) return Boolean
   is
   begin
      return Self.Is_Training;
   end Is_Training;






   function Response (Self : in Item;
                         To   : in     Neural.Pattern) return Signals
   is
      use fann_c;

      The_Pattern  : Pattern renames To;
      the_Inputs   : fann_c.fann_type_array (1 .. the_Pattern.Inputs'length);

      The_Output   : fann_type_Pointer;
      The_Response : Signals (1 .. Self.Num_Output_Neurons);

      RMS_Error    : Signal := 0.0;

   begin
      if Self.Is_Undefined
      then
         raise No_Net_Error;
      end if;

      for Each in the_Inputs'Range
      loop
         the_Inputs (Each) := fann_type (the_Pattern.Inputs (Integer (Each)));
      end loop;

      fann_scale_input       (self.Fann,  the_Inputs (1)'unchecked_Access);
      the_Output := fann_run (self.Fann,  the_Inputs (1)'unchecked_Access);
      fann_descale_output    (self.Fann,  the_Output);

      declare
         use neural.Privvy.fann_type_Pointers;
         the_Output_array : constant fann_c.fann_type_array := Value (the_Output.all'Access, c.ptrdiff_t (self.Num_Output_Neurons));
      begin
         for Each in the_Response'Range
         loop
            the_Response (Each) := Signal (the_Output_array (c.size_t (Each) - 1));
         end loop;
      end;

      return the_Response;
   end Response;






   function training_RMS_Error (Self : in Item) return Signal
   is
   begin
      return Self.Training_RMS_Error;
   end training_RMS_Error;




   function test_RMS_Error (Self : in Item) return Signal
   is
   begin
      return Self.Test_RMS_Error;
   end test_RMS_Error;




   function min_test_RMS_Error (Self : in Item) return Signal
   is
   begin
      return Self.Min_Test_RMS_Error;
   end min_test_RMS_Error;




   function Response_RMS_Error (Self : in Item) return Signal
   is
   begin
      return Self.Response_RMS_Error;
   end Response_RMS_Error;





   function Epoch (Self : in Item) return Positive
   is
   begin
      return Self.Epoch;
   end Epoch;




   function training_pattern_Count (Self : in Item) return Natural
   is
   begin
      return Self.Training_Patterns'Length;
   end training_pattern_Count;





   function training_Patterns (Self : in Item) return Neural.Patterns
   is
   begin
      return Self.Training_Patterns.all;
   end training_Patterns;




   function test_Patterns (Self : in Item) return Neural.Patterns
   is
   begin
      return Self.Test_Patterns.all;
   end test_Patterns;



end Neural.Net;
