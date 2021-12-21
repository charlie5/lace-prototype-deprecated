with
     openGL.Shader,
     openGL.Program,
     openGL.Buffer.general,
     openGL.Tasks,
     openGL.Attribute,
     openGL.Errors,

     GL.lean,
     GL.Pointers,

     Interfaces.C.Strings,
     System.storage_Elements;

package body openGL.Geometry.colored
is
   use GL.lean, GL.Pointers;
   use Interfaces;

   -----------
   --  Globals
   --

   vertex_Shader   : aliased Shader.item;
   fragment_Shader : aliased Shader.item;
   the_Program     :         openGL.Program.view;

   Name_1 : constant String := "Site";
   Name_2 : constant String := "Color";

   Attribute_1_Name : aliased C.char_array := C.to_C (Name_1);
   Attribute_2_Name : aliased C.char_array := C.to_C (Name_2);

   Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'Access);
   Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'Access);


   ---------
   --  Forge
   --

   function new_Geometry return Geometry.colored.view
   is
      use      System.storage_Elements;
      use type openGL.Program.view;

      Self : constant Geometry.colored.view := new Geometry.colored.item;
   begin
      Tasks.check;

      if the_Program = null
      then   -- Define the shaders and program.
         declare
            use openGL.Attribute.Forge;

            sample_Vertex : Vertex;
            Attribute_1   : Attribute.view;
            Attribute_2   : Attribute.view;
         begin
            vertex_Shader  .define (Shader.Vertex,   "assets/opengl/shader/colored.vert");
            fragment_Shader.define (Shader.Fragment, "assets/opengl/shader/colored.frag");

            the_Program := new openGL.Program.item;
            the_Program.define (vertex_Shader  'Access,
                                fragment_Shader'Access);

            Attribute_1 := new_Attribute (Name        => Name_1,
                                          gl_Location => the_Program.attribute_Location (Name_1),
                                          Size        => 3,
                                          data_Kind   => Attribute.GL_FLOAT,
                                          Stride      => colored.Vertex'Size / 8,
                                          Offset      => 0,
                                          Normalized  => False);

            Attribute_2 := new_Attribute (Name        => Name_2,
                                          gl_Location => the_Program.attribute_Location (Name_2),
                                          Size        => 4,
                                          data_Kind   => Attribute.GL_UNSIGNED_BYTE,
                                          Stride      => colored.Vertex'Size / 8,
                                          Offset      =>   sample_Vertex.Color.primary.Red'Address
                                                         - sample_Vertex.Site (1)         'Address,
                                          Normalized  => True);
            the_Program.add (Attribute_1);
            the_Program.add (Attribute_2);

            glBindAttribLocation (Program =>  the_Program.gl_Program,
                                  Index   =>  the_Program.Attribute (named => Name_1).gl_Location,
                                  Name    => +Attribute_1_Name_ptr);
            Errors.log;

            glBindAttribLocation (Program =>  the_Program.gl_Program,
                                  Index   =>  the_Program.Attribute (named => Name_2).gl_Location,
                                  Name    => +Attribute_2_Name_ptr);
            Errors.log;
         end;
      end if;

      Self.Program_is (the_Program);

      return Self;
   end new_Geometry;


   ------------
   --  Vertices
   --

   function is_Transparent (Self : in Vertex_array) return Boolean
   is
      function get_Color (Index : in Index_t) return rgba_Color
      is (Self (Index).Color);

      function my_Transparency is new get_Transparency (any_Index_t => Index_t,
                                                        get_Color   => get_Color);
   begin
      return my_Transparency (Count => Self'Length);
   end is_Transparent;


   --------------
   --  Attributes
   --

   package openGL_Buffer_of_geometry_Vertices is new Buffer.general (base_Object   => Buffer.array_Object,
                                                                     Index         => Index_t,
                                                                     Element       => Vertex,
                                                                     Element_Array => Vertex_array);

   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array)
   is
      use openGL.Buffer,
          openGL_Buffer_of_geometry_Vertices.Forge;
   begin
      free (Self.Vertices);
      Self.Vertices := new openGL_Buffer_of_geometry_Vertices.Object' (to_Buffer (Now,
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


end openGL.Geometry.colored;
