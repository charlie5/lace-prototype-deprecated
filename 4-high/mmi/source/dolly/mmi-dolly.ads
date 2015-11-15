with
     mmi.Camera;


package mmi.Dolly
--
--  Models a camera dolly.
--
is

   type Item is abstract tagged private;
   type View is access all Item'Class;



   --  Forge
   --

   procedure define  (Self : in out Item)   is abstract;
   procedure destroy (Self : in out Item)   is abstract;

   procedure free    (Self : in out View);




   --  Attributes
   --

   procedure Camera_is            (Self : in out Item'Class;   Now       : in Camera.view);


   type Direction is (Left, Right, Up, Down, Forward, Backward);

   procedure is_moving            (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True);
   procedure is_spinning          (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True);
   procedure is_orbiting          (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True);

   function  Speed                (Self : in     Item)                 return math.Real;
   procedure Speed_is             (Self : in out Item;         Now       : in math.Real);
   procedure speed_Multiplier_is  (Self : in out Item;         Now       : in math.Real);

   procedure allow_linear_Motion  (Self : in out Item;         Allow     : in Boolean)   is null;
   procedure allow_orbital_Motion (Self : in out Item;         Allow     : in Boolean)   is null;



   --  Operations
   --

   procedure freshen (Self : in out Item)   is abstract;





private

   type Direction_Flags is array (Direction) of Boolean;


   type Item is abstract tagged
      record
         Camera           : mmi.Camera.view;

         Motion           : Direction_Flags := (others => False);
         Spin             : Direction_Flags := (others => False);
         Orbit            : Direction_Flags := (others => False);

         Speed            : math.Real       := 1.0;
         speed_Multiplier : math.Real       := 1.0;
      end record;


end mmi.Dolly;
