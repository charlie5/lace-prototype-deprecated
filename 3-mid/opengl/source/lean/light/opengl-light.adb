with
     openGL.conversions;


package body openGL.Light
is

   function Kind (Self : in Item) return light.Kind_t
   is
   begin
      return Self.Kind;
   end Kind;



   function is_On (Self : in Item) return Boolean
   is
   begin
      return Self.On;
   end is_On;


   procedure is_On (Self : in out Item;   Now : in Boolean := True)
   is
   begin
      Self.On := Now;
   end is_On;



   function Site (Self : in Item) return openGL.Site
   is
   begin
      return openGL.Site (Self.Site (1 .. 3));
   end Site;



   procedure Site_is (Self : in out Item;   Now : in openGL.Site)
   is
   begin
      case Self.Kind
      is
         when Diffused =>   Self.Site := Vector_4  (Now & 1.0);
         when Direct   =>   Self.Site := Vector_4 (-Now & 0.0);
      end case;
   end Site_is;



--     function Position (Self : in Item) return Vector_3
--     is
--     begin
--        return Self.Position;
--     end Position;


   function Color (Self : in Item) return openGL.Color
   is
   begin
      return Self.Color;
   end Color;


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



--     procedure Position_is (Self : in out Item;   Now : in Vector_3)
--     is
--     begin
--        Self.Position := Now;
--     end Position_is;


   procedure Color_is (Self : in out Item;   Now : in openGL.Color)
   is
   begin
      Self.Color := Now;
   end Color_is;


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




--     procedure Color_is (Self : in out Item;   Ambient,
--                                               Diffuse : in lucid_Color)
--     is
--        use openGL.conversions;
--     begin
--        Self. ambient_Color := to_Vector_4 (Ambient);
--        Self. diffuse_Color := to_Vector_4 (Diffuse);
--     end Color_is;


end openGL.Light;
