with
     openGL.conversions;

package body openGL.Light.directional
is

   procedure inverse_view_Transform_is (Self : in out Item;   Now : in Matrix_3x3)
   is
      use linear_Algebra;
   begin
      Self.Direction        := Now * Normalised (Self.Site);
      Self.halfplane_Vector := Normalised (  Normalised (Self.Direction (1 .. 3))
                                           + (0.0, 0.0, 1.0));
   end inverse_view_Transform_is;



   procedure Color_is (Self : in out Item;   Ambient,
                                             Diffuse,
                                             Specular : in light_Color)
   is
      use openGL.conversions;
   begin
      Self. ambient_Color := to_Vector_4 (Ambient);
      Self. diffuse_Color := to_Vector_4 (Diffuse);
      Self.specular_Color := to_Vector_4 (Specular);
   end Color_is;



   function ambient_Color (Self : in Item) return Vector_4
   is
   begin
      return Self.ambient_Color;
   end ambient_Color;



   function diffuse_Color (Self : in Item) return Vector_4
   is
   begin
      return Self.diffuse_Color;
   end diffuse_Color;



   function specular_Color (Self : in Item) return Vector_4
   is
   begin
      return Self.specular_Color;
   end specular_Color;



   function Direction (Self : in Item) return Vector_3
   is
   begin
      return Self.Direction;
   end Direction;



   function halfplane_Vector (Self : in Item) return Vector_3
   is
   begin
      return Self.halfplane_Vector;
   end halfplane_Vector;


end openGL.Light.directional;
