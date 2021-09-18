package body gel.Camera.forge
is

   function new_Camera return gel.Camera.item
   is
   begin
      return the_Camera : gel.Camera.item
      do
         define (the_Camera);
      end return;
   end new_Camera;



   function new_Camera return gel.Camera.view
   is
      Self : constant gel.Camera.view := new gel.Camera.item;
   begin
      Self.define;
      return Self;
   end new_Camera;

end gel.Camera.forge;
