with
     openGL.Buffer.general,
     openGL.Shader,
     openGL.Program.lit,
     openGL.Attribute,
     openGL.Texture,
     openGL.Palette,
     openGL.Tasks,
     openGL.Errors,

     GL.Binding,
     GL.lean,
     GL.Pointers,

     Interfaces.C.Strings,
     System.storage_Elements;


package body openGL.Geometry.lit_textured
is
   use GL.lean,
       GL.Pointers,
       Interfaces;

   -----------
   --  Globals
   --

   vertex_Shader   : aliased Shader.item;
   fragment_Shader : aliased Shader.item;

   the_Program     : openGL.Program.lit.view;
   white_Texture   : openGL.Texture.Object;

   Name_1 : constant String := "Site";
   Name_2 : constant String := "Normal";
   Name_3 : constant String := "Coords";
   Name_4 : constant String := "Shine";

   Attribute_1_Name : aliased C.char_array := C.to_C (Name_1);
   Attribute_2_Name : aliased C.char_array := C.to_C (Name_2);
   Attribute_3_Name : aliased C.char_array := C.to_C (Name_3);
   Attribute_4_Name : aliased C.char_array := C.to_C (Name_4);

   Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'Access);
   Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'Access);
   Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_3_Name'Access);
   Attribute_4_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_4_Name'Access);


   ---------
   --  Forge
   --

   function new_Geometry return View
   is
      use      System,
               System.storage_Elements;
      use type openGL.Program.lit.view;

      Self : constant View := new Geometry.lit_textured.item;

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
            Attribute_4 : Attribute.view;

            white_Image : constant Image := (1 .. 2 => (1 .. 2 => +White));

         begin
            white_Texture := openGL.Texture.Forge.to_Texture (white_Image);

            vertex_Shader  .define (Shader.Vertex,   "assets/opengl/shader/lit_textured.vert");
            fragment_Shader.define (Shader.Fragment, "assets/opengl/shader/lit_textured.frag");

            the_Program := new openGL.Program.lit.item;
            the_Program.define (  vertex_Shader'Access,
                                fragment_Shader'Access);
            the_Program.enable;

            Attribute_1 := new_Attribute (Name        => Name_1,
                                          gl_Location => the_Program.attribute_Location (Name_1),
                                          Size        => 3,
                                          data_Kind   => attribute.GL_FLOAT,
                                          Stride      => lit_textured.Vertex'Size / 8,
                                          Offset      => 0,
                                          Normalized  => False);

            Attribute_2 := new_Attribute (Name        => Name_2,
                                          gl_Location => the_Program.attribute_Location (Name_2),
                                          Size        => 3,
                                          data_Kind   => attribute.GL_FLOAT,
                                          Stride      => lit_textured.Vertex'Size / 8,
                                          Offset      =>   Sample.Normal (1)'Address
                                                         - Sample.Site   (1)'Address,
                                          Normalized  => False);

            Attribute_3 := new_Attribute (Name        => Name_3,
                                          gl_Location => the_Program.attribute_Location (Name_3),
                                          Size        => 2,
                                          data_Kind   => attribute.GL_FLOAT,
                                          Stride      => lit_textured.Vertex'Size / 8,
                                          Offset      =>   Sample.Coords.S'Address
                                                         - Sample.Site (1)'Address,
                                          Normalized  => False);

            Attribute_4 := new_Attribute (Name        => Name_4,
                                          gl_Location => the_Program.attribute_Location (Name_4),
                                          Size        => 1,
                                          data_Kind   => attribute.GL_FLOAT,
                                          Stride      => lit_textured.Vertex'Size / 8,
                                          Offset      =>   Sample.Shine   'Address
                                                         - Sample.Site (1)'Address,
                                          Normalized  => False);

            the_Program.add (Attribute_1);
            the_Program.add (Attribute_2);
            the_Program.add (Attribute_3);
            the_Program.add (Attribute_4);

            glBindAttribLocation (program =>  the_Program.gl_Program,
                                  index   =>  the_Program.Attribute (named => Name_1).gl_Location,
                                  name    => +Attribute_1_Name_ptr);
            Errors.log;

            glBindAttribLocation (program =>  the_Program.gl_Program,
                                  index   =>  the_Program.Attribute (named => Name_2).gl_Location,
                                  name    => +Attribute_2_Name_ptr);
            Errors.log;

            glBindAttribLocation (program =>  the_Program.gl_Program,
                                  index   =>  the_Program.Attribute (named => Name_3).gl_Location,
                                  name    => +Attribute_3_Name_ptr);
            Errors.log;

            glBindAttribLocation (program =>  the_Program.gl_Program,
                                  index   =>  the_Program.Attribute (named => Name_4).gl_Location,
                                  name    => +Attribute_4_Name_ptr);
            Errors.log;
         end;
      end if;

      Self.Program_is (the_Program.all'Access);
      return Self;
   end new_Geometry;


   ----------
   --  Vertex
   --

   function is_Transparent (Self : in Vertex_array) return Boolean     -- TODO: Do these properly.
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end is_Transparent;



   function is_Transparent (Self : in Vertex_large_array) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end is_Transparent;


   --------------
   --  Attributes
   --

   package openGL_Buffer_of_geometry_Vertices       is new Buffer.general (base_Object   => Buffer.array_Object,
                                                                           Index         => Index_t,
                                                                           Element       => Vertex,
                                                                           Element_Array => Vertex_array);

   package openGL_large_Buffer_of_geometry_Vertices is new Buffer.general (base_Object   => Buffer.array_Object,
                                                                           Index         => long_Index_t,
                                                                           Element       => Vertex,
                                                                           Element_Array => Vertex_large_array);


   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array)
   is
      use openGL_Buffer_of_geometry_Vertices.Forge;
   begin
      Self.Vertices       := new openGL_Buffer_of_geometry_Vertices.Object' (to_Buffer (Now,
                                                                                        usage => Buffer.static_Draw));
      Self.is_Transparent := is_Transparent (Now);

      -- Set the bounds.
      --
      declare
         function get_Site (Index : in Index_t) return Vector_3
         is (Now (Index).Site);

         function bounding_Box is new get_Bounds (Index_t, get_Site);
      begin
         Self.Bounds_are (bounding_Box (Count => Now'Length));
      end;
   end Vertices_are;



   procedure Vertices_are (Self : in out Item;   Now : in Vertex_large_array)
   is
      use openGL_large_Buffer_of_geometry_Vertices.Forge;
   begin
      Self.Vertices       := new openGL_large_Buffer_of_geometry_Vertices.Object' (to_Buffer (Now,
                                                                                   usage => Buffer.static_Draw));
      Self.is_Transparent := is_Transparent (Now);

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

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);

   begin
      Tasks.check;

      glActiveTexture (gl.GL_TEXTURE0);
      Errors.log;

      if Self.Texture = openGL.Texture.null_Object
      then   enable (white_Texture);
      else   enable (Self.Texture);
      end if;
   end enable_Texture;


end openGL.Geometry.lit_textured;
