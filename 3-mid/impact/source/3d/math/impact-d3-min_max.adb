

package body impact.d3.min_max
is

   function btClamped (a : in Real;  lower_Bound, upper_Bound : in Real) return Real
   is
   begin
      if    a < lower_Bound then
         return lower_Bound;

      elsif upper_Bound < a then
         return upper_Bound;

      else
         return a;
      end if;
   end btClamped;




   procedure btSetMin (a : in out Real;   b : in Real)
   is
   begin
      if b < a then
         a := b;
      end if;
   end btSetMin;



   procedure btSetMax (a : in out Real;   b : in Real)
   is
   begin
      if a < b then
         a := b;
      end if;
   end btSetMax;




   procedure btClamp  (a : in out Real;   lower_Bound, upper_Bound : in Real)
   is
   begin
      if a < lower_Bound then
         a := lower_Bound;

      elsif upper_Bound < a then
         a := upper_Bound;
      end if;
   end btClamp;




end impact.d3.min_max;
