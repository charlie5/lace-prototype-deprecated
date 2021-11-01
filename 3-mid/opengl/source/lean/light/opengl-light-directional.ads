package openGL.Light.directional
--
--  Models a directional light.
--
is

   type Item  is new Light.item with private;
   type Items is array (Positive range <>) of Item;


   procedure inverse_view_Transform_is (Self : in out Item;   Now : in Matrix_3x3);

   procedure Color_is (Self : in out Item;   Ambient,
                                             Diffuse,
                                             Specular : in light_Color);

   procedure  ambient_Color_is (Self : in out Item;   Now : light_Color);
   procedure  diffuse_Color_is (Self : in out Item;   Now : light_Color);
   procedure specular_Color_is (Self : in out Item;   Now : light_Color);

   function  ambient_Color    (Self : in Item) return light_Color;
   function  diffuse_Color    (Self : in Item) return light_Color;
   function specular_Color    (Self : in Item) return light_Color;

   function  Direction        (Self : in Item) return Vector_3;    -- Normalized light direction in eye space.
   function  halfplane_Vector (Self : in Item) return Vector_3;    -- Normalized half-plane vector.



private

   type Item is new Light.item with
      record
         Direction        : Vector_3;
         halfplane_Vector : Vector_3;

         ambient_Color    : Vector_4 := (0.0, 0.0, 0.0, 1.0);     -- The GL defaults for all lights bar 'Light0'.
         diffuse_Color    : Vector_4 := (0.0, 0.0, 0.0, 1.0);
         specular_Color   : Vector_4 := (0.0, 0.0, 0.0, 1.0);
      end record;

end openGL.Light.directional;
