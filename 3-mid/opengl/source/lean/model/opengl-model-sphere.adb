package body openGL.Model.sphere
is

   overriding
   function  Bounds (Self : in Item) return openGL.Bounds
   is
      Radius : constant math.Real := Self.Scale (1) / 2.0;
   begin
      return (ball => Radius,
              box  => (lower => (-Radius, -Radius, -Radius),
                       upper => ( Radius,  Radius,  Radius)));
   end Bounds;


end openGL.Model.sphere;
