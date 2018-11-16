
package impact.d3.min_max
--
--
--
is
   use Math;


   function btClamped (a : in Real;  lower_Bound, upper_Bound : in Real) return Real;

   procedure btSetMin (a : in out Real;   b : in Real);
   procedure btSetMax (a : in out Real;   b : in Real);

   procedure btClamp  (a : in out Real;   lower_Bound, upper_Bound : in Real);

end impact.d3.min_max;
