with neural.Privvy;
with fann_c.Binding;
with Interfaces.C;
with Fann_C.Pointers;

package body Neural.Set
is
   use neural.Privvy,
       fann_c.Binding,
       Interfaces;


   function to_Set (the_Patterns : in Patterns_view) return Set
   is
      the_Set : Set;


      procedure fill_Fann (pattern_Id    : in Interfaces.C.unsigned;
                           inputs_Count  : in Interfaces.C.unsigned;
                           outputs_Count : in Interfaces.C.unsigned;
                           Inputs        : in fann_c.Pointers.fann_type_Pointer;
                           Outputs       : in fann_c.Pointers.fann_type_Pointer);
      pragma Convention (C, fill_Fann);


      procedure fill_Fann (pattern_Id    : in Interfaces.C.unsigned;
                           inputs_Count  : in Interfaces.C.unsigned;
                           outputs_Count : in Interfaces.C.unsigned;
                           Inputs        : in fann_c.Pointers.fann_type_Pointer;
                           Outputs       : in fann_c.Pointers.fann_type_Pointer)
      is
         use fann_c, fann_type_Pointers;

         the_Pattern : Pattern renames the_Patterns (Integer (pattern_Id) + 1);

         the_Inputs  : fann_c.fann_Type_array (1 .. c.size_t (inputs_Count));
         the_Outputs : fann_c.fann_Type_array (1 .. c.size_t (outputs_Count));
      begin
         -- set inputs
         --
         for Each in the_Inputs'range loop
            the_Inputs (Each) := fann_Type (the_Pattern.Inputs (Integer (Each)));

--              put_Line (math.Image (math.Real (the_Inputs (Each))));
         end loop;

         copy_Array (the_Inputs (1)'Unchecked_Access,
                     Inputs.all'access,
                     c.ptrdiff_t (inputs_Count));

         -- set outputs
         --
         for Each in the_Outputs'range loop
            the_Outputs (Each) := fann_Type (the_Pattern.Outputs (Integer (Each)));
         end loop;

         copy_Array (the_Outputs (1)'Unchecked_Access,
                     Outputs.all'access,
                     c.ptrdiff_t (outputs_Count));
      end fill_Fann;


   begin
      the_Set.Patterns := the_Patterns;
      the_Set.Fann     := create_train_from_callback (num_data          => the_Patterns'Length,
                                                      num_input         => the_Patterns (1).Inputs 'Length,
                                                      num_output        => the_Patterns (1).Outputs'Length,
                                                      the_user_function => fill_Fann'unrestricted_access);
      return the_Set;
   end to_Set;




end Neural.Set;

