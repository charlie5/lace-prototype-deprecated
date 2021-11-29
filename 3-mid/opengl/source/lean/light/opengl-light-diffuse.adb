with
     openGL.conversions;

package body openGL.Light.diffuse
is
   -----------
   --- Utility
   --

   function to_light_Color (From : in Vector_4) return lucid_Color
   is
   begin
      return (Primary => (Red   => Primary (From (1)),
                          Green => Primary (From (2)),
                          Blue  => Primary (From (3))),
              Opacity => Opaqueness        (From (4)));
   end to_light_Color;





   --------------
   --- Attributes
   --

   function Position (Self : in Item) return Vector_4
   is
   begin
      return Self.Position;
   end Position;


   function Intensities (Self : in Item) return Vector_3
   is
   begin
      return Self.Intensities;
   end Intensities;


   function Attenuation (Self : in Item) return Real
   is
   begin
      return Self.Attenuation;
   end Attenuation;


   function ambient_Coefficient (Self : in Item) return Real
   is
   begin
      return Self.ambient_Coefficient;
   end ambient_Coefficient;


   function cone_Angle (Self : in Item) return Degrees
   is
   begin
      return Self.cone_Angle;
   end cone_Angle;


   function cone_Direction (Self : in Item) return Vector_3
   is
   begin
      return Self.cone_Direction;
   end cone_Direction;



   procedure Position_is (Self : in out Item;   Now : in Vector_4)
   is
   begin
      Self.Position := Now;
   end Position_is;


   procedure Intensities_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.Intensities := Now;
   end Intensities_is;


   procedure Attenuation_is (Self : in out Item;   Now : in Real)
   is
   begin
      Self.Attenuation := Now;
   end Attenuation_is;


   procedure ambient_Coefficient_is (Self : in out Item;   Now : in Real)
   is
   begin
      Self.ambient_Coefficient := Now;
   end ambient_Coefficient_is;



   procedure cone_Angle_is (Self : in out Item;   Now : in Degrees)
   is
   begin
      Self.cone_Angle := Now;
   end cone_Angle_is;


   procedure cone_Direction_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.cone_Direction := Now;
   end cone_Direction_is;



   --- Old ...
   --






   --------------
   --- Attributes
   --

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
                                             Specular : in lucid_Color)
   is
      use openGL.conversions;
   begin
      Self. ambient_Color := to_Vector_4 (Ambient);
      Self. diffuse_Color := to_Vector_4 (Diffuse);
      Self.specular_Color := to_Vector_4 (Specular);
   end Color_is;



   procedure  ambient_Color_is (Self : in out Item;   Now : lucid_Color)
   is
      use openGL.conversions;
   begin
      Self. ambient_Color := to_Vector_4 (Now);
   end ambient_Color_is;



   procedure  diffuse_Color_is (Self : in out Item;   Now : lucid_Color)
   is
      use openGL.conversions;
   begin
      Self. diffuse_Color := to_Vector_4 (Now);
   end diffuse_Color_is;



   procedure specular_Color_is (Self : in out Item;   Now : lucid_Color)
   is
      use openGL.conversions;
   begin
      Self.specular_Color := to_Vector_4 (Now);
   end specular_Color_is;



   function ambient_Color (Self : in Item) return lucid_Color
   is
   begin
      return to_light_Color (Self.ambient_Color);
   end ambient_Color;



   function diffuse_Color (Self : in Item) return lucid_Color
   is
   begin
      return to_light_Color (Self.diffuse_Color);
   end diffuse_Color;



   function specular_Color (Self : in Item) return lucid_Color
   is
   begin
      return to_light_Color (Self.specular_Color);
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


end openGL.Light.diffuse;
