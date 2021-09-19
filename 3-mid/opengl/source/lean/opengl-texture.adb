with
     openGL.Errors,
     openGL.Tasks,
     openGL.IO,

     GL.Binding,
     GL.lean,
     GL.Pointers,

     ada.unchecked_Deallocation;

package body openGL.Texture
is
   use GL,
       GL.lean,
       GL.Pointers;


   ----------------
   --  Texture Name
   --

   function new_texture_Name return texture_Name
   is
      use GL.Binding;
      the_Name : aliased texture_Name;
   begin
      Tasks.check;
      glGenTextures (1, the_Name'Access);
      return the_Name;
   end new_texture_Name;



   procedure free (the_texture_Name : in texture_Name)
   is
      the_Name : aliased texture_Name := the_texture_Name;
   begin
      Tasks.check;
      glDeleteTextures (1, the_Name'Access);
   end free;


   ---------
   --  Forge
   --

   package body Forge
   is

      function to_Texture (Name : in texture_Name) return Object
      is
         Self : Texture.Object;
      begin
         Self.Name := Name;
         -- TODO: Fill in remaining fields by querying GL.

         return Self;
      end to_Texture;


      function to_Texture (Dimensions : in Texture.Dimensions) return Object
      is
         use GL.Binding;
         Self : aliased Texture.Object;

      begin
         Tasks.check;

         Self.Dimensions := Dimensions;
         Self.Name       := new_texture_Name;
         Self.enable;

         glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);

         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

         return Self;
      end to_Texture;


      function to_Texture (the_Image   : in Image;
                           use_Mipmaps : in Boolean     := True) return Object
      is
         Self : aliased Texture.Object;
      begin
         Self.Name := new_texture_Name;
         Self.set_Image (the_Image, use_Mipmaps);

         return Self;
      end to_Texture;


      function to_Texture (the_Image   : in lucid_Image;
                           use_Mipmaps : in Boolean           := True) return Object
      is
         Self : aliased Texture.Object;
      begin
         Self.Name := new_texture_Name;
         Self.set_Image (the_Image, use_Mipmaps);

         return Self;
      end to_Texture;

   end Forge;



   procedure destroy (Self : in out Object)
   is
   begin
      free (Self.Name);     -- Release the GL texture name.
   end destroy;



   procedure free (Self : in out Object)
   is
   begin
      free (Self.Pool.all, Self);     -- Release 'Self' from it's pool for later re-use.
   end free;



   function is_Defined (Self : in Object) return Boolean
   is
      use type texture_Name;
   begin
      return Self.Name /= 0;
   end is_Defined;



   procedure set_Name (Self : in out Object;   To : in texture_Name)
   is
   begin
      Self.Name := To;
   end set_Name;


   function Name (Self : in Object) return texture_Name
   is
   begin
      return Self.Name;
   end Name;



   procedure set_Image (Self : in out Object;   To          : in Image;
                                                use_Mipmaps : in Boolean := True)
   is
      use GL.Binding;
      the_Image  :          Image renames To;
      min_Width  : constant Positive := the_Image'Length (2);
      min_Height : constant Positive := the_Image'Length (1);
   begin
      Tasks.check;

      Self.is_Transparent    := False;
      Self.Dimensions.Width  := min_Width;
      Self.Dimensions.Height := min_Height;
      Self.enable;

      glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);

      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      Errors.log;

      glTexImage2D (GL_TEXTURE_2D,
                    0,
                    GL_RGB,
                    GLsizei (Self.Dimensions.Width),
                    GLsizei (Self.Dimensions.Height),
                    0,
                    GL_RGB,
                    GL_UNSIGNED_BYTE,
                    +the_Image (1, 1).Red'Address);
      Errors.log;

      if use_Mipmaps
      then
         glGenerateMipmap (GL_TEXTURE_2D);
         Errors.log;
      end if;
   end set_Image;



   procedure set_Image (Self : in out Object;   To          : in lucid_Image;
                                                use_Mipmaps : in Boolean    := True)
   is
      use GL.Binding;

      the_Image  :          lucid_Image renames To;
      min_Width  : constant Positive := the_Image'Length (2);
      min_Height : constant Positive := the_Image'Length (1);

   begin
      Tasks.check;

      Self.is_Transparent    := True;
      Self.Dimensions.Width  := min_Width;
      Self.Dimensions.Height := min_Height;
      Self.enable;

      glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);

      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      Errors.log;

      glTexImage2D (GL_TEXTURE_2D,
                    0,
                    GL_RGBA,
                    GLsizei (Self.Dimensions.Width),
                    GLsizei (Self.Dimensions.Height),
                    0,
                    GL_RGBA,
                    GL_UNSIGNED_BYTE,
                    +the_Image (1, 1).Primary.Red'Address);
      Errors.log;

      if use_Mipmaps
      then
         glGenerateMipmap (GL_TEXTURE_2D);
         Errors.log;
      end if;
   end set_Image;



   function is_Transparent (Self : in Object) return Boolean
   is
   begin
      return Self.is_Transparent;
   end is_Transparent;



   procedure enable (Self : in Object)
   is
      use GL.Binding;
      use type GL.GLuint;
      pragma Assert (Self.Name > 0);
   begin
      Tasks.check;
      glBindTexture (GL.GL_TEXTURE_2D, Self.Name);
   end enable;



   function Power_of_2_Ceiling (From : in Positive) return GL.GLsizei
   is
      use type GL.GLsizei;
   begin
      if    From <=         2 then   return         2;
      elsif From <=         4 then   return         4;
      elsif From <=         8 then   return         8;
      elsif From <=        16 then   return        16;
      elsif From <=        32 then   return        32;
      elsif From <=        64 then   return        64;
      elsif From <=       128 then   return       128;
      elsif From <=       256 then   return       256;
      elsif From <=       512 then   return       512;
      elsif From <=      1024 then   return      1024;
      elsif From <=  2 * 1024 then   return  2 * 1024;
      elsif From <=  4 * 1024 then   return  4 * 1024;
      elsif From <=  8 * 1024 then   return  8 * 1024;
      elsif From <= 16 * 1024 then   return 16 * 1024;
      elsif From <= 32 * 1024 then   return 32 * 1024;
      end if;

      raise Constraint_Error with "Texture size too large:" & From'Image;
   end Power_of_2_Ceiling;



   function Size (Self : in Object) return Texture.Dimensions
   is
   begin
      return Self.Dimensions;
   end Size;



   -----------------------
   -- Name Maps of Texture
   --

   function fetch (From : access name_Map_of_texture'Class;   texture_Name : in asset_Name) return Object
   is
      Name : constant unbounded_String := to_unbounded_String (to_String (texture_Name));
   begin
      if From.Contains (Name)
      then
         return From.Element (Name);
      else
         declare
            new_Texture : constant Object := IO.to_Texture (texture_Name);
         begin
            From.insert (Name, new_Texture);
            return new_Texture;
         end;
      end if;
   end fetch;



   --------
   --  Pool
   --

   procedure destroy (the_Pool : in out Pool)
   is
      procedure deallocate is new ada.unchecked_Deallocation (pool_texture_List,
                                                              pool_texture_List_view);
   begin
      for Each of the_Pool.Map
      loop
         for i in 1 .. Each.Last
         loop
            destroy    (Each.Textures (i));
            deallocate (Each);
         end loop;
      end loop;
   end destroy;



   function new_Texture (From : access Pool;   Size : in Dimensions) return Object
   is
      use GL.Binding;

      the_Pool    : access  Pool renames From;
      the_Texture : aliased Object;

      unused_List : pool_texture_List_view;

   begin
      Tasks.check;

      if the_Pool.Map.contains (Size)
      then
         unused_List := the_Pool.Map.Element (Size);
      else
         unused_List := new pool_texture_List;
         the_Pool.Map.insert (Size, unused_List);
      end if;

      -- Search for existing, but unused, object.
      --
      if unused_List.Last > 0
      then     -- An existing unused texture has been found.
         the_Texture      := unused_List.Textures (unused_List.Last);
         unused_List.Last := unused_List.Last - 1;

         the_Texture.enable;

         gltexImage2D  (GL_TEXTURE_2D,  0,  GL_RGBA,
                        GLsizei (Size.Width),
                        GLsizei (Size.Height),
                        0,
                        GL_RGBA, GL_UNSIGNED_BYTE,
                        null);                             -- NB: Actual image is not initialised.

      else     -- No existing, unused texture found, so create a new one.
         the_Texture.Pool := From.all'unchecked_Access;
         the_Texture.Name := new_texture_Name;
         the_Texture.enable;

         glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);
         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,
                          GL_CLAMP_TO_EDGE);
         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,
                          GL_CLAMP_TO_EDGE);

         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
                          GL_LINEAR);
         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
                          GL_LINEAR);

         gltexImage2D  (gl_TEXTURE_2D,  0,  gl_RGBA,
                        GLsizei (Size.Width),
                        GLsizei (Size.Height),
                        0,
                        GL_RGBA, GL_UNSIGNED_BYTE,
                        null);                             -- NB: Actual image is not initialised.
      end if;

      the_Texture.Dimensions := Size;

      return the_Texture;
   end new_Texture;



   procedure free (From : in out Pool;   the_Texture : in Object)
   is
      use type texture_Name;
   begin
      if the_Texture.Name = 0 then
         return;
      end if;

      raise Program_Error with "TODO: free texture from pool";
--        declare
--           unused_texture_List : constant pool_texture_List_view
--             := Self.unused_Textures_for_size (the_Texture.Size_width,
--                                               the_Texture.Size_height);
--        begin
--           unused_texture_List.Last                                := unused_texture_List.Last + 1;
--           unused_texture_List.Textures (unused_texture_List.Last) := the_Texture;
--        end;
   end free;



   procedure vacuum (the_Pool : in out Pool)
   is
   begin
      for Each of the_Pool.Map
      loop
         declare
            unused_List : constant pool_texture_List_view := Each;
         begin
            if unused_List /= null
            then
               for Each in 1 .. unused_List.Last
               loop
                  free (unused_List.Textures (Each).Name);
               end loop;

               unused_List.Last := 0;
            end if;
         end;
      end loop;

      -- TODO: Test this ~ old code follows ...

--        for each_Width in Self.unused_Textures_for_size'Range (1)
--        loop
--           for each_Height in self.unused_Textures_for_size'Range (2)
--           loop
--              declare
--                 unused_texture_List : constant pool_texture_List_view
--                   := Self.unused_Textures_for_size (each_Width, each_Height);
--              begin
--                 if unused_texture_List /= null
--                 then
--                    for Each in 1 .. unused_texture_List.Last
--                    loop
--                       free (unused_texture_List.Textures (Each).Name);
--                    end loop;
--
--                    unused_texture_List.Last := 0;
--                 end if;
--              end;
--           end loop;
--        end loop;
   end vacuum;



   function Hash (the_Dimensions : in Texture.Dimensions) return ada.Containers.Hash_Type
   is
   begin
      return ada.Containers.Hash_type (  the_Dimensions.Width  * 13
                                       + the_Dimensions.Height * 17);
   end Hash;


end openGL.Texture;
