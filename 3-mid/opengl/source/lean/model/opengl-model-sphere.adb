package body openGL.Model.sphere
is

   ---------
   --- Forge
   --

   procedure define (Self : out Item;   Radius : Real)
   is
   begin
      Self.Radius := Radius;
   end define;



   --------------
   --- Attributes
   --

   overriding
   function Bounds (Self : in Item) return openGL.Bounds
   is
   begin
      return (Ball => Self.Radius,
              Box  => (Lower => (-Self.Radius, -Self.Radius, -Self.Radius),
                       Upper => ( Self.Radius,  Self.Radius,  Self.Radius)));
   end Bounds;


end openGL.Model.sphere;
