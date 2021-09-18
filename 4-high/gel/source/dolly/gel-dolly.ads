with
     gel.Camera,
     ada.Containers.Vectors;

package gel.Dolly
--
--  Models a camera dolly.
--
is
   type Item is abstract tagged private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure define  (Self : in out Item)   is abstract;
   procedure destroy (Self : in out Item)   is abstract;

   procedure free    (Self : in out View);


   --------------
   --- Attributes
   --

   type Direction is (Left, Right, Up, Down, Forward, Backward);

   procedure add_Camera           (Self : in out Item'Class;   the_Camera : in Camera.view);

   procedure is_moving            (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True);
   procedure is_spinning          (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True);
   procedure is_orbiting          (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True);

   function  Speed                (Self : in     Item)       return math.Real;
   procedure Speed_is             (Self : in out Item;   Now   : in math.Real);
   procedure speed_Multiplier_is  (Self : in out Item;   Now   : in math.Real);

   procedure allow_linear_Motion  (Self : in out Item;   Allow : in Boolean) is null;
   procedure allow_orbital_Motion (Self : in out Item;   Allow : in Boolean) is null;


   --------------
   --- Operations
   --

   procedure freshen (Self : in out Item) is abstract;



private

   use type gel.Camera.view;
   package camera_Vectors is new ada.Containers.Vectors (Positive, gel.Camera.view);
   subtype camera_Vector  is     camera_Vectors.Vector;

   type Direction_Flags is array (Direction) of Boolean;


   type Item is abstract tagged
      record
         Cameras    : camera_Vector;

         Motion     : Direction_Flags := (others => False);
         Spin       : Direction_Flags := (others => False);
         Orbit      : Direction_Flags := (others => False);

         Speed      : math.Real       := 1.0;
         Multiplier : math.Real       := 1.0;     -- Applied to speed.
      end record;


end gel.Dolly;
