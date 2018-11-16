with impact.d3.Quaternions;
with impact.d3.Matrix,

     interfaces.c.Pointers;



package body impact.d3.Transform
is


   type Scalars is array (Natural range <>) of aliased Real;

   package scalar_Pointers is new interfaces.c.Pointers (Natural, Real, Scalars, 0.0);






   function to_Transform (q : in Quaternion;   c : in Vector_3 := math.Origin_3d) return Transform_3d
   is
      use linear_Algebra_3d,  impact.d3.Matrix;
   begin
      return (to_Matrix (q),
              c);
   end to_Transform;




   function to_Transform (m : in Matrix_3x3;
                          c : in Vector_3 := math.Origin_3d) return Transform_3d
   is
   begin
      return (m, c);
   end to_Transform;




   procedure mult (Self : out Transform_3d;   t1,                    --  't1' Transform 1
                   t2 : in Transform_3d)
   is
      use math.Vectors;
   begin
      Self := (t1.Rotation * t2.Rotation,
               Transformed (t1, t2.Translation));
   end mult;




   function transformed (Self : in Transform_3d;   x : in Vector_3) return Vector_3
   is
      use impact.d3.Matrix, math.Vectors;
   begin
      return ((getRow (Self.Rotation, 1) * x  +  Self.Translation (1)),
              (getRow (Self.Rotation, 2) * x  +  Self.Translation (2)),
              (getRow (Self.Rotation, 3) * x  +  Self.Translation (3)));
   end transformed;






   function "*" (Left : in Transform_3d;   Right : in Quaternion) return Quaternion
   is
      use impact.d3.Matrix, impact.d3.Quaternions;
   begin
      return multiply (getRotation (Left),  Right);
   end "*";





   function getRotation (Self : in Transform_3d) return Quaternion
   is
      use impact.d3.Matrix;

      q : Quaternion;
   begin
      getRotation (self.Rotation, q);
      return q;
   end getRotation;





   function getBasis (Self : in     Transform_3d) return        Matrix_3x3
   is
   begin
      return self.Rotation;
   end getBasis;



   function getBasis (Self : access Transform_3d) return access Matrix_3x3
   is
   begin
      return self.Rotation'Access;
   end getBasis;




   function getOrigin (Self : in     Transform_3d) return        Vector_3
   is
   begin
      return self.Translation;
   end getOrigin;



   function getOrigin (Self : access Transform_3d) return access Vector_3
   is
   begin
      return self.Translation'Unchecked_Access;
   end getOrigin;





   procedure setFromOpenGLMatrix (Self : in out Transform_3d;   m : access Real)
   is
      use impact.d3.Matrix, scalar_Pointers;

      Start : constant scalar_Pointers.Pointer := m.all'Access;
   begin
      setFromOpenGLSubMatrix (Self.Rotation, m);

      self.Translation := (Pointer (Start + 12).all,
                        Pointer (Start + 13).all,
                        Pointer (Start + 14).all);
   end setFromOpenGLMatrix;






   procedure getOpenGLMatrix     (Self : in     Transform_3d;   m : access Real)
   is
      use impact.d3.Matrix, scalar_Pointers;

      Start : constant scalar_Pointers.Pointer := m.all'Access;
   begin
      getOpenGLSubMatrix (Self.Rotation, m);

      Pointer (Start + 12).all := Self.Translation (1);
      Pointer (Start + 13).all := Self.Translation (2);
      Pointer (Start + 14).all := Self.Translation (3);
      Pointer (Start + 15).all := 1.0;
   end getOpenGLMatrix;





   procedure setOrigin (Self : in out Transform_3d;   origin : in Vector_3)
   is
   begin
      self.Translation := origin;
   end setOrigin;




   procedure setBasis (Self : in out Transform_3d;   basis : in Matrix_3x3)
   is
   begin
      self.Rotation := basis;
   end setBasis;




   procedure setRotation (Self : in out Transform_3d;   q : in Quaternion)
   is
      use impact.d3.Matrix;
   begin
      setRotation (Self.Rotation, q);
   end setRotation;




   procedure setIdentity (Self : in out Transform_3d)
   is
      use impact.d3.Matrix;
   begin
      setIdentity (Self.Rotation);
      self.Translation := (0.0, 0.0, 0.0);
   end setIdentity;





--     function "*" (Left : in Transform;   Right : in Transform_3d) return Transform_3d
--     is
--        Result : Transform_3d;
--     begin
--        mult  (Result,  Left, Right);
--        return Result;
--     end;




   function inverse (Self : in Transform_3d) return Transform_3d
   is
      use math.Vectors;

      inv : constant Matrix_3x3 := transpose (Self.Rotation);
   begin
      return (inv,
              inv * (-self.Translation));
   end inverse;




   function getIdentity return Transform_3d
   is
   begin
      return (impact.d3.Matrix.getIdentity,
              (0.0, 0.0, 0.0));
   end getIdentity;



   function inverseTimes (Self : in Transform_3d;   t : in Transform_3d) return Transform_3d
   is
      use impact.d3.Matrix;
      v : constant Vector_3 := getOrigin (t) - Self.Translation;
   begin
      return (transposeTimes (Self.Rotation, t.Rotation),
              v * Self.Rotation);
   end inverseTimes;




   function invXform     (Self : in Transform_3d;   inVec : in Vector_3) return Vector_3
   is
      use impact.d3.Matrix, math.Vectors;
      v : constant Vector_3 := inVec - Self.Translation;
   begin
      return Transpose (Self.Rotation) * v;
   end invXform;


end impact.d3.Transform;
