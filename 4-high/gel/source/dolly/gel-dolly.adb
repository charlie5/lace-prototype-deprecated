with
     ada.unchecked_Deallocation;

package body gel.Dolly
is
   use Math;


   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      if Self = null
      then
         return;
      end if;

      Self.destroy;
      deallocate (Self);
   end free;


   --------------
   --- Attributes
   --

   procedure add_Camera (Self : in out Item'Class;   the_Camera : in Camera.view)
   is
   begin
      Self.Cameras.append (the_Camera);
   end add_Camera;



   procedure is_moving (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True)
   is
   begin
      Self.Motion (Direction) := Now;
   end is_moving;



   procedure is_spinning (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True)
   is
   begin
      Self.Spin (Direction) := Now;
   end is_spinning;



   procedure is_orbiting (Self : in out Item'Class;   Direction : dolly.Direction;   Now : in Boolean := True)
   is
   begin
      Self.Orbit (Direction) := Now;
   end is_orbiting;



   procedure Speed_is (Self : in out Item;  Now : in Real)
   is
   begin
      Self.Speed := Now;
   end Speed_is;



   function Speed (Self : in Item) return Real
   is
   begin
      return Self.Speed;
   end Speed;



   procedure speed_Multiplier_is (Self : in out Item;  Now : in Real)
   is
   begin
      Self.Multiplier := Now;
   end speed_Multiplier_is;


end gel.Dolly;

