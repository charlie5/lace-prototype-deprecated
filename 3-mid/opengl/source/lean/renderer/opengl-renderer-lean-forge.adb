package body openGL.Renderer.lean.forge
is


   function to_Renderer  return openGL.Renderer.lean.item
   is
   begin
      return the_Renderer : openGL.Renderer.lean.item
      do
         the_Renderer.define;
      end return;
   end to_Renderer;



   function new_Renderer  return openGL.Renderer.lean.view
   is
      Self : constant openGL.Renderer.lean.view := new openGL.Renderer.lean.item;
   begin
      Self.define;
      return Self;
   end new_Renderer;


end openGL.Renderer.lean.forge;
