package body openGL.Culler
is

   procedure Viewer_is (Self : in out Item'Class;   Now : in Renderer.lean.view)
   is
   begin
      Self.Viewer := Now.all'Access;
   end Viewer_is;


   function Viewer (Self : in Item'Class) return Renderer.lean.view
   is
   begin
      return Self.Viewer;
   end Viewer;

end openGL.Culler;
