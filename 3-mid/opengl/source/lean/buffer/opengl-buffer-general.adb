with
     openGL.Errors,
     openGL.Tasks,

     GL.Pointers;

package body openGL.Buffer.general
is
   --------------------------
   --  'vertex buffer' Object
   --

   package body Forge
   is
      function to_Buffer (From  : access constant Element_Array;
                          Usage : in              Buffer.Usage) return  Object
      is
         use GL.Pointers;
      begin
         Tasks.check;

         return new_Buffer : Object
         do
            new_Buffer.Usage  := Usage;
            new_Buffer.Length := From'Length;
            new_Buffer.verify_Name;
            new_Buffer.enable;

            glBufferData (to_GL_Enum (new_Buffer.Kind),
                          From.all'Size / 8,
                         +From (From'First)'Address,
                          to_GL_Enum (Usage));
            Errors.log;
         end return;
      end to_Buffer;


      function to_Buffer (From  : in Element_Array;
                          Usage : in Buffer.Usage) return  Object
      is
         use GL.Pointers;
      begin
         Tasks.check;

         return new_Buffer : Object
         do
            new_Buffer.Usage  := Usage;
            new_Buffer.Length := From'Length;
            new_Buffer.verify_Name;
            new_Buffer.enable;

            glBufferData (to_GL_Enum (new_Buffer.Kind),
                          From'Size / 8,
                         +From (From'First)'Address,
                          to_GL_Enum (Usage));
         end return;
      end to_Buffer;

   end Forge;



   procedure set (Self : in out Object;   Position : in Positive := 1;
                                          To       : in Element_Array)
   is
      use GL.Pointers;

      new_Vertices        : aliased  Element_Array := To;
      Vertex_Size_in_bits : constant Natural       := To (To'First)'Size;

   begin
      Tasks.check;

      if Self.Length = To'Length
      then
         Self.enable;
         glBufferSubData (Target =>  to_GL_Enum (Self.Kind),
                          Offset =>  GLintptr ((Position - 1) * Vertex_Size_in_bits / 8),
                          Size   =>  new_Vertices'Size / 8,
                          Data   => +new_Vertices (new_Vertices'First)'Address);
      else
         Self.destroy;

         Self.verify_Name;
         Self.Length := To'Length;
         Self.enable;

         glBufferData (to_GL_Enum (Self.Kind),
                       To'Size / 8,
                      +To (To'First)'Address,
                       to_GL_Enum (Self.Usage));
      end if;

      Errors.log;
   end set;



   procedure set (Self : in out Object;   Position : in              Positive := 1;
                                          To       : access constant Element_Array)
   is
   begin
      Self.set (Position, To.all);
   end set;


end openGL.Buffer.general;
