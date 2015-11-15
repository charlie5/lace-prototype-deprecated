package body mmi.Camera.forge
is


   function new_Camera return mmi.Camera.item
   is
   begin
      return the_Camera : mmi.Camera.item
      do
         define (the_Camera);
      end return;
   end new_Camera;



   function new_Camera return mmi.Camera.view
   is
      Self : constant mmi.Camera.view := new mmi.Camera.item;
   begin
      Self.define;
      return Self;
   end new_Camera;


end mmi.Camera.forge;
