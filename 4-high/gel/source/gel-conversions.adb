package body gel.Conversions
is
   use Math;


   function to_GL (Self : in geometry_3d.bounding_Box) return openGL.Bounds
   is
      the_Bounds : opengl.Bounds := (Ball => <>,
                                     Box  => (Lower => to_GL (Self.Lower),
                                              Upper => to_GL (Self.Upper)));
   begin
      openGL.set_Ball_from_Box (the_Bounds);
      return the_Bounds;
   end to_GL;



   function to_GL (Self : in Real) return opengl.Real
   is
   begin
      return opengl.Real (Self);

   exception
      when constraint_Error =>
         if Self > 0.0
         then   return opengl.Real'Last;
         else   return opengl.Real'First;
         end if;
   end to_GL;



   function to_GL (Self : in Vector_3) return opengl.Vector_3
   is
   begin
      return (to_GL (Self (1)),
              to_GL (Self (2)),
              to_GL (Self (3)));
   end to_GL;



   function to_GL (Self : in Matrix_3x3) return opengl.Matrix_3x3
   is
   begin
      return ((to_gl (Self (1, 1)),  to_gl (Self (1, 2)),  to_gl (Self (1, 3))),
              (to_gl (Self (2, 1)),  to_gl (Self (2, 2)),  to_gl (Self (2, 3))),
              (to_gl (Self (3, 1)),  to_gl (Self (3, 2)),  to_gl (Self (3, 3))));
   end to_GL;



   function to_GL (Self : in Matrix_4x4) return opengl.Matrix_4x4
   is
   begin
      return ((to_gl (Self (1, 1)),  to_gl (Self (1, 2)),  to_gl (Self (1, 3)),  to_gl (Self (1, 4))),
              (to_gl (Self (2, 1)),  to_gl (Self (2, 2)),  to_gl (Self (2, 3)),  to_gl (Self (2, 4))),
              (to_gl (Self (3, 1)),  to_gl (Self (3, 2)),  to_gl (Self (3, 3)),  to_gl (Self (3, 4))),
              (to_gl (Self (4, 1)),  to_gl (Self (4, 2)),  to_gl (Self (4, 3)),  to_gl (Self (4, 4))));
   end to_GL;



   function to_Math (Self : in opengl.Vector_3) return math.Vector_3
   is
   begin
      return (Self (1),
              Self (2),
              Self (3));
   end to_Math;


end gel.Conversions;
