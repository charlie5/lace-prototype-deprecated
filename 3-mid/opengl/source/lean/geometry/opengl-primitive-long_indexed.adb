with
     openGL.Errors,
     openGL.Buffer,
     openGL.Tasks,

     ada.Unchecked_Deallocation;


package body openGL.Primitive.long_indexed
is

   ---------
   --- Forge
   --

   procedure define (Self : in out Item;   Kind    : in facet_Kind;
                                           Indices : in openGL.long_Indices)
   is
      use openGL.Buffer.long_indices;
      buffer_Indices : aliased openGL.long_Indices := (Indices'Range => <>);
   begin
      for Each in buffer_Indices'Range
      loop
         buffer_Indices (Each) := Indices (Each) - 1;   -- Adjust indices to zero-based-indexing for GL.
      end loop;

      Self.facet_Kind := Kind;
      Self.Indices    := new openGL.Buffer.long_indices.object' (to_Buffer (buffer_Indices'Access,
                                                                            usage => openGL.buffer.static_Draw));
   end define;



   function new_Primitive (Kind    : in facet_Kind;
                           Indices : in openGL.long_Indices) return Primitive.long_indexed.view
   is
      Self : constant View := new Item;
   begin
      define (Self.all,  Kind, Indices);
      return Self;
   end new_Primitive;



   overriding
   procedure destroy (Self : in out Item)
   is
      procedure free is new ada.unchecked_Deallocation (Buffer.long_indices.Object'Class, Buffer.long_indices.view);
   begin
      Buffer.destroy (Self.Indices.all);
      free (Self.Indices);
   end destroy;



   --------------
   --  Attributes
   --

   -- None.



   --------------
   --  Operations
   --

   procedure Indices_are  (Self : in out Item;   Now : in long_Indices)
   is
      use openGL.Buffer.long_indices;
      buffer_Indices : aliased long_Indices := (Now'Range => <>);
   begin
      for Each in buffer_Indices'Range
      loop
         buffer_Indices (Each) := Now (Each) - 1;   -- Adjust indices to zero-based-indexing for GL.
      end loop;

      Self.Indices.set (to => buffer_Indices);
   end Indices_are;



   overriding
   procedure render (Self : in out Item)
   is
      use GL;
      check_is_OK : constant Boolean := openGL.Tasks.Check;   pragma Unreferenced (check_is_OK);
   begin
      render (openGL.Primitive.item (Self));   -- Do base class render.

      Self.Indices.enable;
      openGL.Errors.log;

      glDrawElements (Thin     (Self.facet_Kind),
                      gl.GLint (Self.Indices.Length),
                      GL_UNSIGNED_INT,
                      null);
   end render;


end openGL.Primitive.long_indexed;
