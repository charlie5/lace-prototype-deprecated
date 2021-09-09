with
     openGL.Shader,
     openGL.Attribute,
     openGL.Buffer.general,
     openGL.Texture,
     openGL.Palette,
     openGL.Tasks,
     openGL.Errors,

     GL.Binding,
     GL.lean,
     GL.Pointers,

     Interfaces.C.Strings,
     System.storage_Elements;


package body openGL.Geometry.lit_colored_textured_skinned
is
   --  Globals
   --
   vertex_Shader   : aliased Shader.item;
   fragment_Shader : aliased Shader.item;
   the_Program     : aliased openGL.Program.lit_colored_textured_skinned.view;

   white_Texture   : openGL.Texture.Object;


   ----------
   --  Vertex
   --

   function is_Transparent (Self : in Vertex_array) return Boolean   -- TODO: Replace this with the generic (check that all similar functions use the generic).
   is
      use type color_Value;
   begin
      for Each in Self'Range
      loop
         if Self (Each).Color.Opacity /= Opaque
         then
            return True;
         end if;
      end loop;

      return False;
   end is_Transparent;


   ---------
   --  Forge
   --

   type Geometry_view is access all Geometry.lit_colored_textured_skinned.item'Class;


   function new_Geometry return access Geometry.lit_colored_textured_skinned.item'Class
   is
      Self : constant Geometry_view := new Geometry.lit_colored_textured_skinned.item;
   begin
      Self.Program_is (openGL.Program.view (geometry.lit_colored_textured_skinned.Program));
      return Self;
   end new_Geometry;


   --------------
   --  Attributes
   --

   function Program return openGL.Program.lit_colored_textured_skinned.view
   is
      use type openGL.Program.lit_colored_textured_skinned.view;
   begin
      Tasks.check;

      if the_Program = null
      then   -- Define the shaders and program.
         declare
            use Palette,
                Attribute.Forge,
                GL.lean,
                GL.Pointers,
                Interfaces,
                System.storage_Elements;

            Sample : Vertex;

            Attribute_1_Name : aliased C.char_array := "aSite";
            Attribute_2_Name : aliased C.char_array := "aNormal";
            Attribute_3_Name : aliased C.char_array := "aColor";
            Attribute_4_Name : aliased C.char_array := "aCoords";
            Attribute_5_Name : aliased C.char_array := "bone_Ids";
            Attribute_6_Name : aliased C.char_array := "bone_Weights";

            Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'unchecked_Access);
            Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'unchecked_Access);
            Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_3_Name'unchecked_Access);
            Attribute_4_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_4_Name'unchecked_Access);
            Attribute_5_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_5_Name'unchecked_Access);
            Attribute_6_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_6_Name'unchecked_Access);

            Attribute_1 : openGL.Attribute.view;
            Attribute_2 : openGL.Attribute.view;
            Attribute_3 : openGL.Attribute.view;
            Attribute_4 : openGL.Attribute.view;
            Attribute_5 : openGL.Attribute.view;
            Attribute_6 : openGL.Attribute.view;

            white_Image : constant openGL.Image := (1 .. 2 => (1 .. 2 => White));
         begin
            white_Texture := openGL.Texture.Forge.to_Texture (white_Image);

            vertex_Shader  .define (Shader.Vertex,   "assets/opengl/shader/lit_textured_skinned.vert");
            fragment_Shader.define (Shader.Fragment, "assets/opengl/shader/lit_textured_skinned.frag");

            the_Program := new openGL.Program.lit_colored_textured_skinned.item;
            the_Program.define (  vertex_Shader'Access,
                                fragment_Shader'Access);
            the_Program.enable;

            Attribute_1 := new_Attribute (Name        => "aSite",
                                          gl_Location => the_Program.attribute_Location ("aSite"),
                                          Size        => 3,
                                          data_Kind   => Attribute.GL_FLOAT,
                                          Stride      => lit_colored_textured_skinned.Vertex'Size / 8,
                                          Offset      => 0,
                                          Normalized  => False);

            Attribute_2 := new_Attribute (Name        => "aNormal",
                                          gl_Location => the_Program.attribute_Location ("aNormal"),
                                          Size        => 3,
                                          data_Kind   => Attribute.GL_FLOAT,
                                          Stride      => lit_colored_textured_skinned.Vertex'Size / 8,
                                          Offset      =>   Sample.Normal (1)'Address
                                                         - Sample.Site   (1)'Address,
                                          Normalized  => False);

            Attribute_3 := new_Attribute (Name        => "aColor",
                                          gl_Location => the_Program.attribute_Location ("aColor"),
                                          Size        => 4,
                                          data_Kind   => Attribute.GL_UNSIGNED_BYTE,
                                          Stride      => lit_colored_textured_skinned.Vertex'Size / 8,
                                          Offset      =>   Sample.Color.Primary.Red'Address
                                                         - Sample.Site (1)         'Address,
                                          Normalized  => True);

            Attribute_4 := new_Attribute (Name        => "aCoords",
                                          gl_Location => the_Program.attribute_Location ("aCoords"),
                                          Size        => 2,
                                          data_Kind   => Attribute.GL_FLOAT,
                                          Stride      => lit_colored_textured_skinned.Vertex'Size / 8,
                                          Offset      =>   Sample.Coords.S'Address
                                                         - Sample.Site (1)'Address,
                                          Normalized  => False);

            Attribute_5 := new_Attribute (Name        => "bone_Ids",
                                          gl_Location => the_Program.attribute_Location ("bone_Ids"),
                                          Size        => 4,
                                          data_Kind   => Attribute.GL_FLOAT,
                                          Stride      => lit_colored_textured_skinned.Vertex'Size / 8,
                                          Offset      =>   Sample.bone_Ids (1)'Address
                                                         - Sample.Site (1)'Address,
                                          Normalized  => False);

            Attribute_6 := new_Attribute (Name        => "bone_Weights",
                                          gl_Location => the_Program.attribute_Location ("bone_Weights"),
                                          Size        => 4,
                                          data_Kind   => Attribute.GL_FLOAT,
                                          Stride      => lit_colored_textured_skinned.Vertex'Size / 8,
                                          Offset      =>   Sample.bone_Weights (1)'Address
                                                         - Sample.Site (1)'Address,
                                          Normalized  => False);
            the_Program.add (Attribute_1);
            the_Program.add (Attribute_2);
            the_Program.add (Attribute_3);
            the_Program.add (Attribute_4);
            the_Program.add (Attribute_5);
            the_Program.add (Attribute_6);

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "aSite").gl_Location,
                                  name    => +Attribute_1_Name_ptr);
            Errors.log;

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "aNormal").gl_Location,
                                  name    => +Attribute_2_Name_ptr);
            Errors.log;

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "aColor").gl_Location,
                                  name    => +Attribute_3_Name_ptr);
            Errors.log;

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "aCoords").gl_Location,
                                  name    => +Attribute_4_Name_ptr);
            Errors.log;

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "bone_Ids").gl_Location,
                                  name    => +Attribute_5_Name_ptr);
            Errors.log;

            glBindAttribLocation (program => the_Program.gl_Program,
                                  index   => the_Program.Attribute (named => "bone_Weights").gl_Location,
                                  name    => +Attribute_6_Name_ptr);
            Errors.log;
         end;
      end if;

      return the_Program;
   end Program;



   overriding
   procedure Indices_are  (Self : in out Item;   Now       : in Indices;
                                                 for_Facia : in Positive)
   is
   begin
      raise Error with "openGL.Geometry.lit_coloured_textured_skinned - 'Indices_are' ~ TODO";
   end Indices_are;



   package openGL_Buffer_of_geometry_Vertices is new Buffer.general (base_Object   => Buffer.array_Object,
                                                                     Index         => long_Index_t,
                                                                     Element       => Vertex,
                                                                     Element_Array => Vertex_array);

   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array)
   is
      use openGL_Buffer_of_geometry_Vertices.Forge;
   begin
      Self.Vertices       := new openGL_Buffer_of_geometry_Vertices.object' (to_Buffer (Now,
                                                                                        usage => Buffer.static_Draw));
      Self.is_Transparent :=    Self.is_Transparent
                             or is_Transparent (Now);
      -- Set the bounds.
      --
      declare
         function get_Site (Index : in long_Index_t) return Vector_3
         is (Now (Index).Site);

         function bounding_Box is new get_Bounds (long_Index_t, get_Site);
      begin
         Self.Bounds_are (bounding_Box (Count => Now'Length));
      end;
   end Vertices_are;



   overriding
   procedure enable_Texture (Self : in Item)
   is
      use GL,
          GL.Binding,
          openGL.Texture;
   begin
      Tasks.check;

      glActiveTexture (gl.GL_TEXTURE0);
      Errors.log;

      if Self.Texture = openGL.Texture.null_Object
      then
         if not white_Texture.is_Defined
         then
            declare
               use Palette;
               white_Image : constant openGL.Image := (1 .. 2 => (1 .. 2 => White));
            begin
               white_Texture := openGL.Texture.Forge.to_Texture (white_Image);
            end;
         end if;

         white_Texture.enable;
      else
         Self.Texture.enable;
      end if;
   end enable_Texture;


end openGL.Geometry.lit_colored_textured_skinned;