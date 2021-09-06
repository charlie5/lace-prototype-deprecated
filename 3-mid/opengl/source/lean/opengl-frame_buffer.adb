with
     GL.lean,
     GL.Binding,

     openGL.Tasks,
     openGL.Errors;

package body openGL.Frame_Buffer
is

   package body Forge
   is

      function to_Frame_Buffer (Width,
                                Height : in Positive) return Item
      is
         use openGL.Texture,
             GL,
             GL.Binding,
             GL.lean;

         Self : Item;

      begin
         Tasks.check;

         Self.Texture := openGL.Texture.Forge.to_Texture (Dimensions' (Width, Height));

         glGenFramebuffers (1, Self.Name'Access);

         -- Attach each texture to the first color buffer of an frame buffer object and clear it.
         --
         glBindFramebuffer      (GL_FRAMEBUFFER, Self.Name);
         glFramebufferTexture2D (GL_FRAMEBUFFER,
                                 GL_COLOR_ATTACHMENT0,
                                 GL_TEXTURE_2D,
                                 Self.Texture.Name,
                                 0);
         glClear                (GL_COLOR_BUFFER_BIT);
         glBindFramebuffer      (GL_FRAMEBUFFER, 0);

         return Self;
      end to_frame_Buffer;



      function to_Frame_Buffer return Item
      is
         use openGL.Texture,
             GL,
             GL.lean;

         Self : Item;
      begin
         Tasks.check;
         Self.Texture := openGL.Texture.null_Object;
         glGenFramebuffers (1, Self.Name'Access);

         return Self;
      end to_frame_Buffer;

   end Forge;



   procedure destruct (Self : in out Item)
   is
      use GL.lean;
   begin
      Tasks.check;
      glDeleteFramebuffers (1, Self.Name'Access);
      Self.Texture.destroy;
   end destruct;



   --------------
   --- Attributes
   --

   function Name (Self : in Item) return Buffer_Name
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
      use GL,
          GL.Binding,
          GL.lean;
   begin
      Tasks.check;
      openGL.Errors.log;

      Self.Texture := Now;

      -- Attach each texture to the first color buffer of an FBO and clear it.
      --
      glBindFramebuffer (GL_FRAMEBUFFER, Self.Name);
      openGL.Errors.log;

      glFramebufferTexture2D (GL_FRAMEBUFFER,
                              GL_COLOR_ATTACHMENT0,
                              GL_TEXTURE_2D,
                              Self.Texture.Name,
                              0);
      openGL.Errors.log;

      glClear (GL_COLOR_BUFFER_BIT);
      openGL.Errors.log;
   end Texture_is;



   function is_complete (Self : in Item) return Boolean
   is
      use GL,
          GL.lean;
      use type GL.GLenum;

      check_is_OK : constant Boolean := Tasks.check with Unreferenced;
      Result      : constant Boolean := glCheckFramebufferStatus (GL_FRAMEBUFFER) = GL_FRAMEBUFFER_COMPLETE;
   begin
      openGL.Errors.log;
      return Result;
   end is_complete;



   --------------
   --- Operations
   --

   procedure enable (Self : in Item)
   is
      use GL,
          GL.lean;
      check_is_OK : constant Boolean := Tasks.check with Unreferenced;
   begin
      glBindFramebuffer (GL_FRAMEBUFFER, Self.Name);

      if not Self.is_Complete
      then
         raise openGL.Error with "GL_FRAMEBUFFER" & Self.Name'Image & " is not 'complete'";
      end if;
   end enable;



   procedure disable (Self : in Item)
   is
      use GL,
          GL.lean;
      check_is_OK : constant Boolean := Tasks.check with Unreferenced;
   begin
      glBindFramebuffer (GL_FRAMEBUFFER, 0);
   end disable;


end openGL.Frame_Buffer;
