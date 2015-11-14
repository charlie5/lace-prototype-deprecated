
with physics.Rigid;
with Math;




package physics.Motor.spring is

   -- a motor which acts as a spring to bring a target solid to a desired site or attitude.


   type Item is abstract new physics.Motor.item with
      record
         Rigid : physics.Rigid.pointer;                -- access to the Solid affected by this Motor.
      end record;




   procedure update (Self : in out Item) is abstract;






private

   procedure dummy;



end physics.Motor.spring;

