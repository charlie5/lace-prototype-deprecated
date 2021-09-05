with
     openGL.Errors,
     openGL.Buffer,
     openGL.Tasks,

     GL.Binding,
     GL.lean;


package body openGL.Primitive.indexed
is

   ---------
   --- Forge
   --

   procedure define (Self : in out Item;   Kind       : in facet_Kind;
                                           Indices    : in openGL.Indices;
                                           line_Width : in Real)
   is
      use openGL.Buffer.indices;
      buffer_Indices : aliased openGL.Indices := (Indices'Range => <>);

   begin
      for Each in buffer_Indices'Range
      loop
         buffer_Indices (Each) := Indices (Each) - 1;   -- Adjust indices to zero-based indexing for GL.
      end loop;

      Self.facet_Kind := Kind;
      Self.Indices    := new openGL.Buffer.indices.Object' (to_Buffer (buffer_Indices'Access,
                                                                       usage => openGL.buffer.static_Draw));
      Self.line_Width := line_Width;
   end define;



   procedure define (Self : in out Item;   Kind       : in facet_Kind;
                                           Indices    : in openGL.long_Indices;
                                           line_Width : in Real)
   is
      use openGL.Buffer.indices;
      buffer_Indices : aliased openGL.Indices := (Indices'Range => <>);

   begin
      for Each in buffer_Indices'Range
      loop
         buffer_Indices (Each) := Index_t (Indices (Each) - 1);   -- Adjust indices to zero-based indexing for GL.
      end loop;

      Self.facet_Kind := Kind;
      Self.Indices    := new openGL.Buffer.indices.Object' (to_Buffer (buffer_Indices'Access,
                                                                       usage => openGL.buffer.static_Draw));
      Self.line_Width := line_Width;
   end define;



   function new_Primitive (Kind       : in facet_Kind;
                           Indices    : in openGL.Indices;
                           line_Width : in Real          := unused_line_Width) return Primitive.indexed.view
   is
      Self : constant View := new Item;
   begin
      define (Self.all,  Kind, Indices, line_Width);
      return Self;
   end new_Primitive;



   function new_Primitive (Kind       : in facet_Kind;
                           Indices    : in openGL.long_Indices;
                           line_Width : in Real               := unused_line_Width) return Primitive.indexed.view
   is
      Self : constant View := new Item;
   begin
      define (Self.all,  Kind, Indices, line_Width);
      return Self;
   end new_Primitive;



   overriding
   procedure destroy (Self : in out Item)
   is
      the_Indices : Buffer.view := Buffer.view (Self.Indices);
   begin
      Buffer.free (the_Indices);
      Self.Indices := null;
   end destroy;



   --------------
   --  Attributes
   --

   -- None.



   --------------
   --  Operations
   --

   procedure Indices_are  (Self : in out Item;   Now : in Indices)
   is
      use openGL.Buffer.indices;
      buffer_Indices : aliased Indices := (Now'Range => <>);
   begin
      for Each in buffer_Indices'Range
      loop
         buffer_Indices (Each) := Now (Each) - 1;             -- Adjust indices to zero-based-indexing for GL.
      end loop;

      Self.Indices.set (to => buffer_Indices);
   end Indices_are;



   procedure Indices_are  (Self : in out Item;   Now : in long_Indices)
   is
      use openGL.Buffer.indices;
      buffer_Indices : aliased Indices := (Now'Range => <>);
   begin
      for Each in buffer_Indices'Range
      loop
         buffer_Indices (Each) := Index_t (Now (Each) - 1);   -- Adjust indices to zero-based-indexing for GL.
      end loop;

      Self.Indices.set (to => buffer_Indices);
   end Indices_are;





   overriding
   procedure render (Self : in out Item)
   is
      use GL,
          GL.Binding,
          GL.lean;
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      render (openGL.Primitive.item (Self));   -- Do base class render.

      Self.Indices.enable;
      openGL.Errors.log;

      glDrawElements (Thin     (Self.facet_Kind),
                      gl.GLint (Self.Indices.Length),
                      GL_UNSIGNED_SHORT,
                      null);
      openGL.Errors.log;
   end render;


end openGL.Primitive.indexed;
