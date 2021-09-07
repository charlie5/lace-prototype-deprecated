with
     openGL.Shader,
     openGL.Buffer.general,
     openGL.Program.colored_textured,
     openGL.Attribute,
     openGL.Texture,
     openGL.Palette,
     openGL.Tasks,
     openGL.Errors,

     GL.Binding,
     GL.lean,
     GL.Pointers,

     System,
     Interfaces.C.Strings,
     System.storage_Elements;

package body openGL.Geometry.colored_textured
is
   use GL.lean,
       GL.Pointers,
       Interfaces;

   -----------
   --  Globals
   --

   vertex_Shader        : aliased Shader.item;
   fragment_Shader      : aliased Shader.item;
   the_Program          :         openGL.Program.colored_textured.view;
   white_Texture        :         openGL.Texture.Object;

   Attribute_1_Name     : aliased          C.char_array        := "aSite";
   Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'Access);

   Attribute_2_Name     : aliased          C.char_array        := "aColor";
   Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'Access);

   Attribute_3_Name     : aliased          C.char_array        := "aCoords";
   Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_3_Name'Access);


   ---------
   --  Forge
   --

   type Geometry_view is access all Geometry.colored_textured.item'Class;

   function new_Geometry return access Geometry.colored_textured.item'Class
   is
      use      System,
               System.storage_Elements;
      use type openGL.Program.colored_textured.view;

      Self : constant Geometry_view := new Geometry.colored_textured.item;

   begin
      Tasks.check;

      if the_Program = null
      then   -- Define the shaders and program.
         declare
            use Palette,
                Attribute.Forge;

            Sample : Vertex;

            Attribute_1 : Attribute.view;
            Attribute_2 : Attribute.view;
            Attribute_3 : Attribute.view;

            white_Image : constant Image := (1 .. 2 => (1 .. 2 => White));
         begin
            white_Texture := openGL.Texture.Forge.to_Texture (white_Image);

            vertex_Shader  .define (Shader.Vertex,   "assets/opengl/shader/colored_textured.vert");
            fragment_Shader.define (Shader.Fragment, "assets/opengl/shader/colored_textured.frag");

            the_Program := new openGL.Program.colored_textured.item;
            the_Program.define (vertex_Shader  'Access,
                                fragment_Shader'Access);
            the_Program.enable;

            Attribute_1 := new_Attribute (Name => "aSite",
                                          gl_Location => the_Program.attribute_Location ("aSite"),
                                          Size        => 3,
                                          data_Kind   => Attribute.GL_FLOAT,
                                          Stride      => colored_textured.Vertex'Size / 8,
                                          Offset      => 0,
                                          Normalized  => False);

            Attribute_2 := new_Attribute (Name        => "aColor",
                                          gl_Location => the_Program.attribute_Location ("aColor"),
                                          Size        => 4,
                                          data_Kind   => Attribute.GL_UNSIGNED_BYTE,
                                          Stride      => colored_textured.Vertex'Size / 8,
                                          Offset      =>   Sample.Color.Primary.Red'Address
                                                         - Sample.Site (1)         'Address,
                                          Normalized  => True);

            Attribute_3 := new_Attribute (Name        => "aCoords",
                                          gl_Location => the_Program.attribute_Location ("aCoords"),
                                          Size        => 2,
                                          data_Kind   => attribute.GL_FLOAT,
                                          Stride      => Colored_textured.Vertex'Size / 8,
                                          Offset      =>   Sample.Coords.S'Address
                                                         - Sample.Site (1)'Address,
                                          Normalized  => False);
            the_Program.add (Attribute_1);
            the_Program.add (Attribute_2);
            the_Program.add (Attribute_3);

            glBindAttribLocation (program =>  the_Program.gl_Program,
                                  index   =>  the_Program.Attribute (named => "aSite").gl_Location,
                                  name    => +Attribute_1_Name_ptr);
            Errors.log;

            glBindAttribLocation (program =>  the_Program.gl_Program,
                                  index   =>  the_Program.Attribute (named => "aColor").gl_Location,
                                  name    => +Attribute_2_Name_ptr);
            Errors.log;

            glBindAttribLocation (program =>  the_Program.gl_Program,
                                  index   =>  the_Program.Attribute (named => "aCoords").gl_Location,
                                  name    => +Attribute_3_Name_ptr);
            Errors.log;
         end;
      end if;

      Self.Program_is (the_Program.all'Access);
      return Self;
   end new_Geometry;


   ----------
   --  Vertex
   --

   function is_Transparent (Self : in Vertex_array) return Boolean
   is
      function get_Color (Index : in long_Index_t) return lucid_Color
      is (Self (Index).Color);

      function my_Transparency is new get_Transparency (any_Index_t => long_Index_t,
                                                        get_Color   => get_Color);
   begin
      return my_Transparency (Count => Self'Length);
   end is_Transparent;


   --------------
   --  Attributes
   --

   package openGL_Buffer_of_geometry_Vertices is new Buffer.general (base_Object   => Buffer.array_Object,
                                                                     Index         => long_Index_t,
                                                                     Element       => Vertex,
                                                                     Element_Array => Vertex_array);

   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array)
   is
      use openGL_Buffer_of_geometry_Vertices.Forge;
   begin
      Self.Vertices := new openGL_Buffer_of_geometry_Vertices.Object' (to_Buffer (Now,
                                                                                  usage => Buffer.static_Draw));
      Self.is_Transparent := is_Transparent (Now);

      -- Set the bounds.
      --
      declare
         function get_Site (Index : in long_Index_t) return Vector_3
         is (Now (Index).Site);

         function bounding_Box is new get_Bounds (long_Index_t, get_Site);
      begin
         Self.Bounds_are (bounding_Box (count => Now'Length));
      end;
   end Vertices_are;


   overriding
   procedure Indices_are  (Self : in out Item;   Now       : in Indices;
                                                 for_Facia : in Positive)
   is
   begin
      raise Error with "TODO";
   end Indices_are;


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
      then   enable (white_Texture);
      else   enable (Self.Texture);
      end if;
   end enable_Texture;


end openGL.Geometry.colored_textured;
