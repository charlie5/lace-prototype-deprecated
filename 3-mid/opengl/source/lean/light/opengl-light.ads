with
     openGL.Palette;


package openGL.Light
--
--  Models a light.
--
is
   type Item  is tagged private;
   type Items is array (Positive range <>) of Item;


   --------------
   --- Attributes
   --
   type Id_t   is new Natural;
   type Kind_t is (Diffuse, Direct);

   null_Id : constant Id_t;

   function  Id      (Self : in     Item)     return light.Id_t;
   procedure Id_is   (Self : in out Item;   Now : in light.Id_t);

   function  Kind    (Self : in     Item)     return light.Kind_t;
   procedure Kind_is (Self : in out Item;   Now : in light.Kind_t);

   function  is_On   (Self : in     Item)     return Boolean;
   procedure is_On   (Self : in out Item;   Now : in Boolean := True);

   function  Site    (Self : in     Item)     return openGL.Site;
   procedure Site_is (Self : in out Item;   Now : in openGL.Site);

   function  Color               (Self : in     Item) return Color;
   function  Attenuation         (Self : in     Item) return Real;
   function  ambient_Coefficient (Self : in     Item) return Real;
   function  cone_Angle          (Self : in     Item) return Degrees;
   function  cone_Direction      (Self : in     Item) return Vector_3;

   procedure Color_is               (Self : in out Item;   Now : in openGL.Color);
   procedure Attenuation_is         (Self : in out Item;   Now : in Real);
   procedure ambient_Coefficient_is (Self : in out Item;   Now : in Real);
   procedure cone_Angle_is          (Self : in out Item;   Now : in Degrees);
   procedure cone_Direction_is      (Self : in out Item;   Now : in Vector_3);



private

   null_Id : constant Id_t := Id_t'First;

   type Item is tagged
      record
         Id   : light.Id_t   := null_Id;
         Kind : light.Kind_t := Direct;
         On   : Boolean      := True;
         Site : openGL.Site  := (0.0, 0.0, 1.0);     -- The GL default.

         Color               : openGL.Color := Palette.White;
         Attenuation         : Real         :=  0.1;
         ambient_Coefficient : Real         :=  0.1;
         cone_Angle          : Degrees      :=  2.0;
         cone_Direction      : Vector_3     := (0.0, 0.0, -1.0);
      end record;

end openGL.Light;
