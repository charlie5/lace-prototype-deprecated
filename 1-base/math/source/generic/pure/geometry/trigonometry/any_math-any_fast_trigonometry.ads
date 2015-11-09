with
     cached_Trigonometry;

generic
package any_math.any_fast_Trigonometry
is

   package Default is new cached_Trigonometry (Float_type                 => any_Math.Real,
                                               float_elementary_Functions => any_Math.Functions,
                                               slot_Count                 => 10_000);
end any_math.any_fast_Trigonometry;
