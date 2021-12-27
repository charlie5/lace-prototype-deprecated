with
     openGL.Program.lit,
     openGL.Buffer.general,
     openGL.Shader,
     openGL.Attribute,
     openGL.Tasks,
     openGL.Errors,

     GL.lean,
     GL.Pointers,

     Interfaces.C.Strings,
     System.storage_Elements;


package body openGL.Geometry.lit_colored
is
   use GL.lean,
       GL.Pointers,
       Interfaces,
       System;

   ------------------
   --  Shader Program
   --

   type Program is
      record
         vertex_Shader   : aliased Shader.item;
         fragment_Shader : aliased Shader.item;
         Program         :         openGL.Program.lit.view;
      end record;


   -----------
   --- Globals
   --

   the_Program : aliased Program;


   Name_1 : constant String := "Site";
   Name_2 : constant String := "Normal";
   Name_3 : constant String := "Color";
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
      use      System.storage_Elements;
      use type openGL.Program.lit.view;

      procedure define (the_Program : access Program)
      is
         use Attribute.Forge;

         Sample : Vertex;

         Attribute_1,
         Attribute_2,
         Attribute_3,
         Attribute_4 : Attribute.view;
      begin
         the_Program.Program := new openGL.Program.lit.item;

         the_Program.  vertex_Shader.define (Shader.Vertex,    "assets/opengl/shader/lit_colored.vert");
         the_Program.fragment_Shader.define (Shader.Fragment,  "assets/opengl/shader/lit_colored.frag");

         the_Program.Program.define (the_Program.  vertex_Shader'Access,
                                     the_Program.fragment_Shader'Access);

         Attribute_1 := new_Attribute (Name        => Name_1,
                                       gl_Location => the_Program.Program.attribute_Location (Name_1),
                                       Size        => 3,
                                       data_Kind   => attribute.GL_FLOAT,
                                       Stride      => lit_colored.Vertex'Size / 8,
                                       Offset      => 0,
                                       Normalized  => False);

         Attribute_2 := new_Attribute (Name        => Name_2,
                                       gl_Location => the_Program.Program.attribute_Location (Name_2),
                                       Size        => 3,
                                       data_Kind   => attribute.GL_FLOAT,
                                       Stride      => lit_colored.Vertex'Size / 8,
                                       Offset      =>   Sample.Normal (1)'Address
                                                      - Sample.Site   (1)'Address,
                                       Normalized  => False);

         Attribute_3 := new_Attribute (Name        => Name_3,
                                       gl_Location => the_Program.Program.attribute_Location (Name_3),
                                       Size        => 4,
                                       data_Kind   => attribute.GL_UNSIGNED_BYTE,
                                       Stride      => lit_colored.Vertex'Size / 8,
                                       Offset      =>   Sample.Color.Primary.Red'Address
                                                      - Sample.Site (1)         'Address,
                                       Normalized  => True);

         Attribute_4 := new_Attribute (Name        => Name_4,
                                       gl_Location => the_Program.Program.attribute_Location (Name_4),
                                       Size        => 1,
                                       data_Kind   => attribute.GL_FLOAT,
                                       Stride      => lit_colored.Vertex'Size / 8,
                                       Offset      =>   Sample.Shine    'Address
                                                      - Sample.Site (1) 'Address,
                                       Normalized  => True);

         the_Program.Program.add (Attribute_1);
         the_Program.Program.add (Attribute_2);
         the_Program.Program.add (Attribute_3);
         the_Program.Program.add (Attribute_4);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => Name_1).gl_Location,
                               name    => +Attribute_1_Name_ptr);
         Errors.log;

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => Name_2).gl_Location,
                               name    => +Attribute_2_Name_ptr);
         Errors.log;

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => Name_3).gl_Location,
                               name    => +Attribute_3_Name_ptr);
         Errors.log;

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => Name_4).gl_Location,
                               name    => +Attribute_4_Name_ptr);
         Errors.log;
      end define;

      Self : constant View := new Geometry.lit_colored.item;

   begin
      Tasks.check;

      if the_Program.Program = null     --  Define the shaders and program, if required.
      then
         define (the_Program'Access);
      end if;

      Self.Program_is (openGL.Program.view (the_Program.Program));
      return Self;
   end new_Geometry;


   ----------
   --  Vertex
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
      use openGL_Buffer_of_geometry_Vertices.Forge;
   begin
      Buffer.free (Self.Vertices);

      Self.is_Transparent := False;
      self.Vertices       := new openGL_Buffer_of_geometry_Vertices.Object' (to_Buffer (Now,
                                                                                        usage => Buffer.static_Draw));
      Self.is_Transparent := is_Transparent (Now);

      -- Set the bounds.
      --
      declare
         function get_Site (Index : in Index_t) return Vector_3
         is (Now (Index).Site);

         function bounding_Box is new get_Bounds (Index_t, get_Site);
      begin
         Self.Bounds_are (bounding_Box (count => Now'Length));
      end;
   end Vertices_are;


end openGL.Geometry.lit_colored;
