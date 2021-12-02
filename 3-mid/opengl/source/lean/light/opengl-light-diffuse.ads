package openGL.Light.diffuse
--
--  Models a diffuse light.
--
is
   type Item  is new Light.item with private;
   type Items is array (Positive range <>) of Item;


   --------------
   --- Attributes
   --
   function  Position            (Self : in     Item) return Vector_3;
   function  Intensities         (Self : in     Item) return Vector_3;
   function  Attenuation         (Self : in     Item) return Real;
   function  ambient_Coefficient (Self : in     Item) return Real;
   function  cone_Angle          (Self : in     Item) return Degrees;
   function  cone_Direction      (Self : in     Item) return Vector_3;

   procedure Position_is            (Self : in out Item;   Now : in Vector_3);
   procedure Intensities_is         (Self : in out Item;   Now : in Vector_3);
   procedure Attenuation_is         (Self : in out Item;   Now : in Real);
   procedure ambient_Coefficient_is (Self : in out Item;   Now : in Real);
   procedure cone_Angle_is          (Self : in out Item;   Now : in Degrees);
   procedure cone_Direction_is      (Self : in out Item;   Now : in Vector_3);



   --- Old ...
   --
   procedure inverse_view_Transform_is (Self : in out Item;   Now : in Matrix_3x3);

   procedure Color_is (Self : in out Item;   Ambient,
                                             Diffuse,
                                             Specular : in lucid_Color);

   procedure  ambient_Color_is (Self : in out Item;   Now : lucid_Color);
   procedure  diffuse_Color_is (Self : in out Item;   Now : lucid_Color);
   procedure specular_Color_is (Self : in out Item;   Now : lucid_Color);

   function  ambient_Color    (Self : in Item) return lucid_Color;
   function  diffuse_Color    (Self : in Item) return lucid_Color;
   function specular_Color    (Self : in Item) return lucid_Color;

   function  Direction        (Self : in Item) return Vector_3;    -- Normalized light direction in eye space.
   function  halfplane_Vector (Self : in Item) return Vector_3;    -- Normalized half-plane vector.



private

   type Item is new Light.item with
      record
         Position            : Vector_3 := (0.0, 0.0, 5.0);
         Intensities         : Vector_3 := (1.0, 1.0, 1.0);
         Attenuation         : Real     :=  0.1;
         ambient_Coefficient : Real     :=  0.1;
         cone_Angle          : Degrees  :=  2.0;
         cone_Direction      : Vector_3 := (0.0, 0.0, -1.0);

         Direction        : Vector_3;
         halfplane_Vector : Vector_3;

         ambient_Color    : Vector_4 := (0.0, 0.0, 0.0, 1.0);     -- The GL defaults for all lights bar 'Light0'.
         diffuse_Color    : Vector_4 := (0.0, 0.0, 0.0, 1.0);
         specular_Color   : Vector_4 := (0.0, 0.0, 0.0, 1.0);
      end record;

end openGL.Light.diffuse;
