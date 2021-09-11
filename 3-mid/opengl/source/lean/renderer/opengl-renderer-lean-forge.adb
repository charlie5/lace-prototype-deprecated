package body openGL.Renderer.lean.forge
is


   function to_Renderer return Renderer.lean.item
   is
   begin
      return the_Renderer : Renderer.lean.item
      do
         the_Renderer.define;
      end return;
   end to_Renderer;



   function new_Renderer return Renderer.lean.view
   is
      Self : constant Renderer.lean.view := new Renderer.lean.item;
   begin
      Self.define;
      return Self;
   end new_Renderer;


end openGL.Renderer.lean.forge;
