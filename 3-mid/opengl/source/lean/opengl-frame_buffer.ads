with
     openGL.Texture;

package openGL.Frame_Buffer
is

   type Item is tagged private;

   null_Buffer : constant Item;


   ---------
   --- Forge
   --

   package Forge
   is
      function to_Frame_Buffer return Item;
      function to_Frame_Buffer (Width,
                                Height : in Positive) return Item;
   end Forge;

   procedure destruct (Self : in out Item);


   --------------
   --- Attributes
   --
   subtype Buffer_Name is GL.GLuint;     -- An openGL frame buffer 'Name'.

   function  Name        (Self : in     Item)     return Buffer_Name;

   function  Texture     (Self : in     Item)     return openGL.Texture.Object;
   procedure Texture_is  (Self : in out Item;   now : in openGL.Texture.Object);

   function  is_complete (Self : in     Item)     return Boolean;


   --------------
   --- Operations
   --

   procedure enable  (Self : in Item);
   procedure disable (Self : in Item);
   --
   -- Unbind the frame buffer so rendering will return to the backbuffer.



private

   type Item is tagged
      record
         Name    : aliased buffer_Name;
         Texture :         openGL.Texture.Object;
      end record;

   null_Buffer : constant Item := (Name    => 0,
                                   Texture => openGL.Texture.null_Object);

end openGL.Frame_Buffer;
