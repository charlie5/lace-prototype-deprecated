with
     ada.unchecked_Conversion,
     ada.Strings.unbounded.Hash,
     ada.Containers.hashed_Maps;

package openGL.Texture
--
--  Provides openGL textures.
--
is
   --  Object - an openGL texture 'object'
   --

   type Object  is tagged private;
   type Objects is array (Positive range <>) of Object;

   null_Object : constant Object;

   subtype texture_Name is GL.GLuint;     -- An openGL texture object 'Name'.
   subtype Dimensions   is Extent_2d;


   ---------
   --- Forge
   --

   package Forge
   is
      function to_Texture (Name        : in texture_Name)       return Object;
      function to_Texture (Dimensions  : in Texture.Dimensions) return Object;
      function to_Texture (the_Image   : in Image;
                           use_Mipmaps : in Boolean := True)    return Object;
      function to_Texture (the_Image   : in lucid_Image;
                           use_Mipmaps : in Boolean := True)    return Object;
   end Forge;

   procedure destroy (Self : in out Object);
   procedure free    (Self : in out Object);


   --------------
   --- Attributes
   --

   function  is_Defined     (Self : in     Object) return Boolean;
   function  is_Transparent (Self : in     Object) return Boolean;

   procedure set_Name       (Self : in out Object;   To : in texture_Name);
   function  Name           (Self : in     Object)    return texture_Name;

   procedure enable         (Self : in     Object);

   procedure set_Image      (Self : in out Object;   To          : in Image;
                                                     use_Mipmaps : in Boolean := True);
   procedure set_Image      (Self : in out Object;   To          : in lucid_Image;
                                                     use_Mipmaps : in Boolean := True);

   function  Size           (Self : in     Object) return Texture.Dimensions;


   -------
   -- Maps
   --

   type name_Map_of_texture is tagged private;

   function fetch (From         : access name_Map_of_texture'Class;
                   texture_Name : in     asset_Name) return Object;


   --------
   --  Pool
   --
   --  For rapid allocation/deallocation of texture objects.

   --  TODO: Move this into a child package ?

   type Pool      is private;
   type Pool_view is access all Pool;

   procedure destroy     (the_Pool : in out Pool);
   function  new_Texture (From     : access Pool;   Size : in Dimensions) return Object;
   --
   --  Returns a texture object of the requested size.

   procedure free   (From     : in out Pool;   the_Texture : in Object);
   --
   --  Frees a texture for future use.

   procedure vacuum (the_Pool : in out Pool);
   --
   --  Releases any allocated, but unused, texture objects.



   -----------
   -- GL Enums
   --

   --  TexFormatEnm
   --
   type Format is (ALPHA,
                   RGB,               RGBA,
                   LUMINANCE,         LUMINANCE_ALPHA,
                   R3_G3_B2,
                   ALPHA4,            ALPHA8,            ALPHA12,            ALPHA16,
                   LUMINANCE4,        LUMINANCE8,        LUMINANCE12,        LUMINANCE16,
                   LUMINANCE4_ALPHA4, LUMINANCE6_ALPHA2, LUMINANCE8_ALPHA8,  LUMINANCE12_ALPHA4, LUMINANCE12_ALPHA12, LUMINANCE16_ALPHA16,
                   INTENSITY,         INTENSITY4,        INTENSITY8,         INTENSITY12,        INTENSITY16,
                   RGB4,              RGB5,              RGB8,               RGB10,              RGB12,               RGB16,
                   RGBA2,             RGBA4,             RGB5_A1,            RGBA8,              RGB10_A2,            RGBA12,      RGBA16,
                   BGR,               BGRA);

   type pixel_Format is (COLOR_INDEX,
                         RED, GREEN, BLUE, ALPHA,
                         RGB, RGBA,
                         LUMINANCE, LUMINANCE_ALPHA);

   function to_GL (From : in       Format) return GL.GLenum;
   function to_GL (From : in pixel_Format) return GL.GLenum;



   ----------
   -- Utility
   --

   function Power_of_2_Ceiling (From : in Positive) return GL.GLsizei;



private

   type Object is tagged
      record
         Name           : aliased texture_Name       := 0;
         Dimensions     :         Texture.Dimensions := (0, 0);
         is_Transparent :         Boolean            := False;
         Pool           : access  Texture.Pool;
      end record;


   -------
   -- Maps
   --

   use Ada.Strings.unbounded;
   package name_Maps_of_texture_id is new ada.Containers.hashed_Maps (unbounded_String,
                                                                      Texture.Object,
                                                                      Hash,
                                                                      "=");
   type name_Map_of_texture is new name_Maps_of_texture_id.Map with null record;


   ---------------
   --- Rep Clauses
   --

   for Format use
     (ALPHA               => 16#1906#,
      RGB                 => 16#1907#,
      RGBA                => 16#1908#,
      LUMINANCE           => 16#1909#,
      LUMINANCE_ALPHA     => 16#190A#,
      R3_G3_B2            => 16#2A10#,
      ALPHA4              => 16#803B#,
      ALPHA8              => 16#803C#,
      ALPHA12             => 16#803D#,
      ALPHA16             => 16#803E#,
      LUMINANCE4          => 16#803F#,
      LUMINANCE8          => 16#8040#,
      LUMINANCE12         => 16#8041#,
      LUMINANCE16         => 16#8042#,
      LUMINANCE4_ALPHA4   => 16#8043#,
      LUMINANCE6_ALPHA2   => 16#8044#,
      LUMINANCE8_ALPHA8   => 16#8045#,
      LUMINANCE12_ALPHA4  => 16#8046#,
      LUMINANCE12_ALPHA12 => 16#8047#,
      LUMINANCE16_ALPHA16 => 16#8048#,
      INTENSITY           => 16#8049#,
      INTENSITY4          => 16#804A#,
      INTENSITY8          => 16#804B#,
      INTENSITY12         => 16#804C#,
      INTENSITY16         => 16#804D#,
      RGB4                => 16#804F#,
      RGB5                => 16#8050#,
      RGB8                => 16#8051#,
      RGB10               => 16#8052#,
      RGB12               => 16#8053#,
      RGB16               => 16#8054#,
      RGBA2               => 16#8055#,
      RGBA4               => 16#8056#,
      RGB5_A1             => 16#8057#,
      RGBA8               => 16#8058#,
      RGB10_A2            => 16#8059#,
      RGBA12              => 16#805A#,
      RGBA16              => 16#805B#,
      BGR                 => 16#80E0#,
      BGRA                => 16#80E1#);

   for Format'Size use GL.GLenum'Size;


   for pixel_Format use
     (COLOR_INDEX     => 16#1900#,
      RED             => 16#1903#,
      GREEN           => 16#1904#,
      BLUE            => 16#1905#,
      ALPHA           => 16#1906#,
      RGB             => 16#1907#,
      RGBA            => 16#1908#,
      LUMINANCE       => 16#1909#,
      LUMINANCE_ALPHA => 16#190A#);

   for pixel_Format'Size use GL.GLenum'Size;



   --------
   --  Pool
   --

   type pool_texture_List is
      record
         Textures : Objects (1 .. 5_000);
         Last     : Natural             := 0;
      end record;

   type pool_texture_List_view is access all pool_texture_List;


   function Hash (the_Dimensions : in Texture.Dimensions) return ada.Containers.Hash_type;

   package size_Maps_of_pool_texture_List is new ada.Containers.hashed_Maps (Key_Type        => Dimensions,
                                                                             Element_Type    => pool_texture_List_view,
                                                                             Hash            => Hash,
                                                                             Equivalent_Keys => "=");
   type Pool is
      record
         Map : size_Maps_of_pool_texture_List.Map;
      end record;


   -------------
   --  Constants
   --
   null_Object : constant Object := (others => <>);


   ---------------
   --  Conversions
   --

   function convert_1 is new Ada.Unchecked_Conversion (Format,       GL.GLenum);
   function convert_2 is new Ada.Unchecked_Conversion (pixel_Format, GL.GLenum);

   function to_GL (From : in       Format) return GL.GLenum renames convert_1;
   function to_GL (From : in pixel_Format) return GL.GLenum renames convert_2;

end openGL.Texture;
