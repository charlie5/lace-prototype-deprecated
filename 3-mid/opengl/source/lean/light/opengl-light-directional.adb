package body openGL.Light.directional
is

   procedure inverse_view_Transform_is (Self : in out Item;   Now : in Matrix_3x3)
   is
      use linear_Algebra;
   begin
      Self.Direction          := Normalised (Self.Site) * Now;
      Self.halfplane_Vector   := Normalised (Normalised (Self.Direction (1 .. 3)) + (0.0, 0.0, 1.0));
   end inverse_view_Transform_is;



   procedure Color_is (Self : in out Item;   Ambient  : in Vector_4;
                                             Diffuse  : in Vector_4;
                                             Specular : in Vector_4)
   is
   begin
      Self. ambient_Color := Ambient;
      Self. diffuse_Color := Diffuse;
      Self.specular_Color := Specular;
   end Color_is;

end openGL.Light.directional;


