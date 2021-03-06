
with Fann_C;
with Interfaces.C.pointers;   use Interfaces;




private
package Neural.Privvy is

   package fann_type_Pointers is new interfaces.c.Pointers (index              => c.size_t,
                                                            element            => fann_c.fann_type,
                                                            element_array      => fann_c.fann_type_array,
                                                            default_terminator => fann_c.fann_type'Last);


   procedure dummy;



end Neural.Privvy;
