package body openGL.Model.sphere
is

   overriding
   function Bounds (Self : in Item) return openGL.Bounds
   is
      Radius : constant Real := Self.Scale (1) / 2.0;
   begin
      return (Ball => Radius,
              Box  => (Lower => (-Radius, -Radius, -Radius),
                       Upper => ( Radius,  Radius,  Radius)));
   end Bounds;

end openGL.Model.sphere;
