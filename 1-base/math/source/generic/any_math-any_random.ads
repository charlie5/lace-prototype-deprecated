generic
package any_Math.any_Random
is

   function random_Real    (Lower : in Real    := Real'First;
                            Upper : in Real    := Real'Last)    return Real;

   function random_Integer (Lower : in Integer := Integer'First;
                            Upper : in Integer := Integer'Last) return Integer;

   function random_Boolean                                      return Boolean;

end any_Math.any_Random;
