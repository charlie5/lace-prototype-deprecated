package body c_math_c.Conversion
is

   function "+" (Self : in Integer) return C.int
   is
   begin
      return C.int (Self);
   end "+";


   function "+" (Self : in C.int) return Integer
   is
   begin
      return Integer (Self);
   end "+";


   function "+" (Self : in math.Real) return c_math_c.Real
   is
   begin
      return c_math_c.Real (Self);
   end "+";


   function "+" (Self : in c_math_c.Real) return math.Real
   is
   begin
      return math.Real (Self);
   end "+";


   function "+" (Self : in math.Vector_2) return c_math_c.Vector_2.item
   is
      Result : c_math_c.Vector_2.item;
   begin
      begin
         Result.x := c_math_c.Real (Self (1));
      exception
         when constraint_Error =>
            if Self (1) > 0.0
            then   Result.x := c_math_c.Real'Last;
            else   Result.x := c_math_c.Real'First;
            end if;
      end;

      begin
         Result.y := c_math_c.Real (Self (2));
      exception
         when constraint_Error =>
            if Self (2) > 0.0
            then   Result.x := c_math_c.Real'Last;
            else   Result.x := c_math_c.Real'First;
            end if;
      end;

      return Result;
   end "+";


   function "+" (Self : in c_math_c.Vector_2.item) return math.Vector_2
   is
   begin
      return (math.Real (Self.x),
              math.Real (Self.y));
   end "+";


   function "+" (Self : in math.Vector_3) return c_math_c.Vector_3.item
   is
      Result : c_math_c.Vector_3.item;
   begin
      begin
         Result.x := c_math_c.Real (Self (1));
      exception
         when constraint_Error =>
            if Self (1) > 0.0
            then   Result.x := c_math_c.Real'Last;
            else   Result.x := c_math_c.Real'First;
            end if;
      end;

      begin
         Result.y := c_math_c.Real (Self (2));
      exception
         when constraint_Error =>
            if Self (2) > 0.0
            then   Result.x := c_math_c.Real'Last;
            else   Result.x := c_math_c.Real'First;
            end if;
      end;

      begin
         Result.z := c_math_c.Real (Self (3));
      exception
         when constraint_Error =>
            if Self (3) > 0.0
            then   Result.x := c_math_c.Real'Last;
            else   Result.x := c_math_c.Real'First;
            end if;
      end;

      return Result;
   end "+";


   function "+" (Self : in c_math_c.Vector_3.item) return math.Vector_3
   is
   begin
      return (math.Real (Self.x),  math.Real (Self.y),  math.Real (Self.z));
   end "+";


   function "+" (Self : in math.Matrix_3x3) return c_math_c.Matrix_3x3.item
   is
   begin
      return (Real (Self (1,1)), Real (Self (1,2)), Real (Self (1,3)),
              Real (Self (2,1)), Real (Self (2,2)), Real (Self (2,3)),
              Real (Self (3,1)), Real (Self (3,2)), Real (Self (3,3)));
   end "+";


   function "+" (Self : in c_math_c.Matrix_3x3.item) return math.Matrix_3x3
   is
   begin
      return (1 => (math.Real (Self.m00), math.Real (Self.m01), math.Real (Self.m02)),
              2 => (math.Real (Self.m10), math.Real (Self.m11), math.Real (Self.m12)),
              3 => (math.Real (Self.m20), math.Real (Self.m21), math.Real (Self.m22)));
   end "+";


   function "+" (Self : in math   .Matrix_4x4     ) return c_math_c.Matrix_4x4.item
   is
   begin
      return (Real (Self (1,1)),  Real (Self (1,2)),  Real (Self (1,3)),  Real (Self (1,4)),
              Real (Self (2,1)),  Real (Self (2,2)),  Real (Self (2,3)),  Real (Self (2,4)),
              Real (Self (3,1)),  Real (Self (3,2)),  Real (Self (3,3)),  Real (Self (3,4)),
              Real (Self (4,1)),  Real (Self (4,2)),  Real (Self (4,3)),  Real (Self (4,4)));
   end "+";


   function "+" (Self : in c_math_c.Matrix_4x4.item) return math.Matrix_4x4
   is
   begin
      return (1 => (math.Real (Self.m00), math.Real (Self.m01), math.Real (Self.m02), math.Real (Self.m03)),
              2 => (math.Real (Self.m10), math.Real (Self.m11), math.Real (Self.m12), math.Real (Self.m13)),
              3 => (math.Real (Self.m20), math.Real (Self.m21), math.Real (Self.m22), math.Real (Self.m23)),
              4 => (math.Real (Self.m30), math.Real (Self.m31), math.Real (Self.m32), math.Real (Self.m33)));

   end "+";

end c_math_c.Conversion;
