package openGL.Light.directional
--
--  Models a directional light.
--
is

   type Item  is new Light.item with private;
   type Items is array (Positive range <>) of Item;


   procedure inverse_view_Transform_is (Self : in out Item;   Now : in Matrix_3x3);


   procedure Color_is (Self : in out Item;   Ambient  : in lucid_Color;
                                             Diffuse  : in lucid_Color;
                                             Specular : in lucid_Color);

   procedure light_Color_is (Self : in out Item;   Ambient  : in light_Color;
                                                   Diffuse  : in light_Color;
                                                   Specular : in light_Color);


   function  ambient_Color (Self : in    Item) return Vector_4;
   function  diffuse_Color (Self : in    Item) return Vector_4;
   function specular_Color (Self : in    Item) return Vector_4;



   function  Direction        (Self : in    Item) return Vector_3;    -- Normalized light direction in eye space.
   function  halfplane_Vector (Self : in    Item) return Vector_3;    -- Normalized half-plane vector.


private

   type Item is new Light.item with
      record
         Direction          : Vector_3;
         halfplane_Vector   : Vector_3;

         ambient_Color      : Vector_4;
         diffuse_Color      : Vector_4;
         specular_Color     : Vector_4;
      end record;

end openGL.Light.directional;


