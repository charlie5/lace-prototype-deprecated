with
     GL.lean,
     openGL.Tasks,
     openGL.Errors;


package body openGL.frame_Buffer
is


   function to_frame_Buffer (Width,
                             Height : in Positive) return Item
   is
      use openGL.Texture,
          GL, GL.lean;

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      Self        :          Item;

   begin
      Self.Texture := to_Texture (Width, Height);

      glGenFramebuffers (1, Self.Name'Access);

      -- Attach each texture to the first color buffer of an FBO and clear it.
      --
      glBindFramebuffer      (GL_FRAMEBUFFER, Self.Name);
      glFramebufferTexture2D (GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Self.Texture.Name, 0);

      glClear                (GL_COLOR_BUFFER_BIT);
      glBindFramebuffer      (GL_FRAMEBUFFER, 0);

      return Self;
   end to_frame_Buffer;



   function to_frame_Buffer return Item
   is
      use openGL.Texture,
          GL, GL.lean;

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      Self        :          Item;

   begin
      Self.Texture := openGL.Texture.null_Object;

      glGenFramebuffers (1, Self.Name'Access);

      return Self;
   end to_frame_Buffer;



   procedure destruct (Self : in out Item)
   is
      use GL.lean;
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      glDeleteFramebuffers (1,  Self.Name'Access);
      Self.Texture.destroy;
   end destruct;




   --------------
   --- Attributes
   --

   function  Name (Self : in     Item) return buffer_Name
   is
   begin
      return Self.Name;
   end Name;



   function Texture (Self : in Item) return openGL.Texture.Object
   is
   begin
      return Self.Texture;
   end Texture;


   procedure Texture_is (Self : in out Item;   Now : in openGL.Texture.Object)
   is
      use GL, GL.lean;
   begin
      openGL.Errors.log;

      Self.Texture := Now;

      -- Attach each texture to the first color buffer of an FBO and clear it.
      --
      glBindFramebuffer      (GL_FRAMEBUFFER, Self.Name);
      openGL.Errors.log;

      glFramebufferTexture2D (GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Self.Texture.Name, 0);
      openGL.Errors.log;

      glClear                (GL_COLOR_BUFFER_BIT);
      openGL.Errors.log;
   end Texture_is;



   function  is_Complete (Self : in     Item) return Boolean
   is
      use GL, GL.lean;
      use type GL.GLenum;

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      Result      : constant Boolean := glCheckFramebufferStatus (GL_FRAMEBUFFER) = GL_FRAMEBUFFER_COMPLETE;
   begin
      openGL.Errors.log;
      return Result;
   end is_Complete;




   --------------
   --- Operations
   --

   procedure enable (Self : in Item)
   is
      use GL, GL.lean;
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      glBindFramebuffer (GL_FRAMEBUFFER, Self.Name);

      if not Self.is_Complete
      then
         raise openGL.Error with "GL_FRAMEBUFFER is not 'complete'";
      end if;
   end enable;


   procedure disable (Self : in Item)
   is
      use GL, GL.lean;
      use type GL.GLenum;
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      glBindFramebuffer (GL_FRAMEBUFFER, 0);
   end disable;


end openGL.frame_Buffer;
