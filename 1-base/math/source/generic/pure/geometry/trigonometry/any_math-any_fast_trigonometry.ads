with
     cached_Trigonometry;

generic
package any_math.any_fast_Trigonometry
is

   package Default is new cached_Trigonometry (Float_type => any_Math.Real,
                                               slot_Count => 10_000);
end any_math.any_fast_Trigonometry;
