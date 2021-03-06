
with fann_c.fann_Train_data;



package Neural.Set
--
-- models a set of neural patterns used for trainng or testing a net.
--
is


   type Patterns_view is access all Patterns;



   type Set is
      record
         Patterns : Patterns_view;
         Fann     : fann_c.fann_Train_data.pointer;
      end record;

   function to_Set (the_Patterns : in Patterns_view) return Set;





end Neural.Set;
