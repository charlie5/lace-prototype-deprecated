with
     openGL.Program.lit_colored,
     openGL.Shader,
     openGL.Buffer.general,
     openGL.Attribute,
     openGL.Tasks,

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
         vertex_Shader   : aliased openGL.Shader.item;
         fragment_Shader : aliased openGL.Shader.item;
         Program         :         openGL.Program.lit_colored.view;
      end record;


   -----------
   --- Globals
   --
   the_Program          : aliased          Program;

   Attribute_1_Name     : aliased          C.char_array        := "aSite";
   Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'Access);

   Attribute_2_Name     : aliased          C.char_array        := "aNormal";
   Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'Access);

   Attribute_3_Name     : aliased          C.char_array        := "aColor";
   Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_3_Name'Access);



   ---------
   --  Forge
   --

   type Geometry_view is access all Geometry.lit_colored.item'class;

   function new_Geometry return access Geometry.lit_colored.item'class
   is
      use      system.Storage_Elements;
      use type openGL.Program.lit_colored.view;

      check_is_OK : constant Boolean       :=     openGL.Tasks.Check;        pragma Unreferenced (check_is_OK);
      Self        : constant Geometry_view := new Geometry.lit_colored.item;

      procedure define (the_Program : access Program)
      is
         sample_Vertex : Vertex;

         Attribute_1   : openGL.Attribute.view;
         Attribute_2   : openGL.Attribute.view;
         Attribute_3   : openGL.Attribute.view;
      begin
         the_Program.Program := new openGL.Program.lit_colored.item;

         the_Program.  vertex_Shader.define (openGL.Shader.Vertex,    "assets/opengl/shader/lit_colored.vert");
         the_Program.fragment_Shader.define (openGL.Shader.Fragment,  "assets/opengl/shader/lit_colored.frag");
         the_Program.        Program.define (the_Program.vertex_Shader  'Access,
                                            the_Program.fragment_Shader'Access);

         Attribute_1 := attribute.Forge.new_Attribute
                          (name        => "aSite",
                           gl_location => the_Program.Program.attribute_Location ("aSite"),
                           size        => 3,
                           data_kind   => attribute.GL_FLOAT,
                           stride      => lit_colored.Vertex'Size / 8,
                           offset      => 0,
                           normalized  => False);

         Attribute_2 := attribute.Forge.new_Attribute
                          (name        => "aNormal",
                           gl_location => the_Program.Program.attribute_Location ("aNormal"),
                           size        => 3,
                           data_kind   => attribute.GL_FLOAT,
                           stride      => lit_colored.Vertex'Size / 8,
                           offset      =>   sample_Vertex.Normal (1)'Address
                                          - sample_Vertex.Site   (1)'Address,
                           normalized  => False);

         Attribute_3 := attribute.Forge.new_Attribute
                          (name        => "aColor",
                           gl_location => the_Program.Program.attribute_Location ("aColor"),
                           size        => 4,
                           data_kind   => attribute.GL_UNSIGNED_BYTE,
                           stride      => lit_colored.Vertex'Size / 8,
                           offset      =>   sample_Vertex.Color.Primary.Red'Address
                                          - sample_Vertex.Site (1)         'Address,
                           normalized  => True);

         the_Program.Program.add (Attribute_1);
         the_Program.Program.add (Attribute_2);
         the_Program.Program.add (Attribute_3);


         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aSite").gl_Location,
                               name    => +Attribute_1_Name_ptr);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aNormal").gl_Location,
                               name    => +Attribute_2_Name_ptr);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aColor").gl_Location,
                               name    => +Attribute_3_Name_ptr);
      end define;

   begin
      --  Define the shaders and program, if required.
      --
      if the_Program.Program = null
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
      use type color_Value;

      function get_Color (Index : in Index_t) return lucid_Color
      is (Self (Index).Color);

      function my_Transparency is new get_Transparency (any_Index_t => Index_t,
                                                        get_Color   => get_Color);
   begin
      return my_Transparency (count => Self'Length);
   end is_Transparent;


   --------------
   --  Attributes
   --

   package openGL_Buffer_of_geometry_Vertices is new openGL.Buffer.general (base_object   => openGL.Buffer.array_Object,
                                                                            index         => Index_t,
                                                                            element       => Vertex,
                                                                            element_array => Vertex_array);

   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array)
   is
      use      openGL_Buffer_of_geometry_Vertices;
      use type Index_t,
               openGL.Buffer.view;
   begin
      openGL.Buffer.free (Self.Vertices);

      Self.is_Transparent := False;
      self.Vertices       := new openGL_Buffer_of_geometry_Vertices.Object' (to_Buffer (Now,
                                                                                        usage => openGL.buffer.static_Draw));
      Self.is_Transparent := is_Transparent (Now);

      -- Set the bounds.
      --
      declare
         function get_Site (Index : in Index_t) return Vector_3
         is (Now (Index).Site);

         function bBox is new get_Bounds (Index_t, get_Site);
      begin
         Self.Bounds_are (bBox (count => Now'Length));
      end;
   end Vertices_are;


end openGL.Geometry.lit_colored;
