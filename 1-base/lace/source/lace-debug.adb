
package body lace.Debug
is

   procedure enable_float_Exceptions
   is
      function feenableexcept (Val : Integer) return Integer;
      pragma Import (C, feenableexcept);

      Status : Integer;

   begin
      Status := feenableexcept (16#01#);
      Status := feenableexcept (16#02#);
      Status := feenableexcept (16#04#);
      Status := feenableexcept (16#08#);
      --        Status := feenableexcept (16#10#);     -- underflow
      --        Status := feenableexcept (16#20#);     -- inexact
   end enable_float_Exceptions;

end lace.Debug;
