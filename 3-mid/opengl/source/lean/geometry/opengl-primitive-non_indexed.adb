with
     openGL.Errors,
     openGL.Tasks,
     GL.Binding;

package body openGL.Primitive.non_indexed
is
   ---------
   --  Forge
   --

   overriding
   procedure define (Self : in out Item;   Kind : in facet_Kind)
   is
   begin
      Self.facet_Kind := Kind;
   end define;


   function new_Primitive (Kind         : in facet_Kind;
                           vertex_Count : in Natural) return Primitive.non_indexed.view
   is
      Self : constant View := new Item;
   begin
      define (Self.all, Kind);
      Self.vertex_Count := vertex_Count;

      return Self;
   end new_Primitive;


   overriding
   procedure destroy (Self : in out Item) is null;


   --------------
   --  Operations
   --

   overriding
   procedure render (Self : in out Item)
   is
      use GL,
          GL.Binding;
   begin
      Tasks.check;
      glDrawArrays (Thin (Self.facet_Kind),
                    0,
                    gl.GLint (Self.vertex_Count));
      Errors.log;
   end render;


end openGL.Primitive.non_indexed;
