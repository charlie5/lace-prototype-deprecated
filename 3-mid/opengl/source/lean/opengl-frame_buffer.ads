with
     openGL.Texture;


package openGL.frame_Buffer
is

   type Item is tagged private;

   null_Buffer : constant Item;


   ---------
   --- Forge
   --

   function to_frame_Buffer                        return Item;
   function to_frame_Buffer (Width,
                             Height : in Positive) return Item;

   procedure destruct (Self : in out Item);


   --------------
   --- Attributes
   --
   subtype buffer_Name is GL.GLuint;     -- An openGL frame buffer 'Name'.

   function  Name        (Self : in     Item)     return buffer_Name;

   function  Texture     (Self : in     Item)     return openGL.Texture.Object;
   procedure Texture_is  (Self : in out Item;   Now : in openGL.Texture.Object);

   function  is_Complete (Self : in     Item)     return Boolean;


   --------------
   --- Operations
   --

   procedure enable  (Self : in Item);
   procedure disable (Self : in Item);
   --
   -- Unbind the FBO so rendering will return to the backbuffer.



private

   type Item is tagged
      record
         Name    : aliased buffer_Name;
         Texture :         openGL.Texture.Object;
      end record;

   null_Buffer : constant Item := (Name    => 0,
                                   Texture => openGL.Texture.null_Object);

end openGL.frame_Buffer;
