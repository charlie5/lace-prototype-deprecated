with
     openGL.Errors,
     openGL.Tasks,

     GL.Pointers;


package body openGL.Buffer.general
is

   --------------------------
   --  'vertex buffer' Object
   --

   function to_Buffer (From  : access constant Element_Array;
                       Usage : in              Buffer.Usage) return  Object
   is
      use GL.Pointers;
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      return new_Buffer : Object
      do
         new_Buffer.Usage := Usage;

         verify_Name (new_Buffer);
         openGL.Errors.log;

         new_Buffer.Length := From'Length;

         enable       (new_Buffer);
         glBufferData (to_GL_Enum (Kind (new_Buffer)),
                       From.all'Size / 8,
                      +From (From'First)'Address,
                       to_GL_Enum (Usage));
         openGL.Errors.log;
      end return;
   end to_Buffer;



   function to_Buffer (From  : in Element_Array;
                       Usage : in Buffer.Usage) return  Object
   is
      use GL.Pointers;
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      return new_Buffer : Object
      do
         verify_Name (new_Buffer);

         new_Buffer.Usage  := Usage;
         new_Buffer.Length := From'Length;

         enable       (new_Buffer);
         glBufferData (to_GL_Enum (Kind (new_Buffer)),
                       From'Size / 8,
                      +From (From'First)'Address,
                       to_GL_Enum (Usage));
      end return;
   end to_Buffer;



   procedure set (Self : in out Object;   Position : in Positive     := 1;
                                          To       : in Element_Array)
   is
      use GL.Pointers;

      check_is_OK         : constant Boolean       := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      new_Vertices        : aliased  Element_Array := To;
      Vertex_Size_in_bits : constant Natural       := To (To'First)'Size;

   begin
      if Self.Length = To'Length
      then
         enable          (Self);
         glBufferSubData (to_GL_Enum (Kind (Self)),
                          offset =>  GLintptr ((Position - 1) * Vertex_Size_in_bits / 8),
                          size   =>  new_Vertices'Size / 8,
                          data   => +new_Vertices (new_Vertices'First)'Address);
      else
         Self.destroy;

         verify_Name (Self);
         Self.Length := To'Length;

         enable       (Self);
         glBufferData (to_GL_Enum (Kind (Self)),
                       To'Size / 8,
                      +To (To'First)'Address,
                       to_GL_Enum (Self.Usage));
      end if;

      openGL.Errors.log;
   end set;



   procedure set (Self : in out Object;   Position : in              Positive     := 1;
                                          To       : access constant Element_Array)
   is
   begin
      Self.set (Position, To.all);
   end set;


end openGL.Buffer.general;
