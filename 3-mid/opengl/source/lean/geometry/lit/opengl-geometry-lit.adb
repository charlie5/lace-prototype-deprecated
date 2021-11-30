with
     openGL.Program.lit,
     openGL.Palette,
     openGL.Shader,
     openGL.Buffer.general,
     openGL.Attribute,
     openGL.Texture,
     openGL.Tasks,
     openGL.Errors,

     GL.Binding,
     GL.lean,
     GL.Pointers,

     Interfaces.C.Strings,
     System.storage_Elements;


package body openGL.Geometry.lit
is
   use GL.lean,
       GL.Pointers,
       Interfaces,
       System;

   ------------------
   --  Shader Program
   --

   type program_Id is (rgba_Texture, alpha_Texture);

   type Program is
      record
         vertex_Shader   : aliased Shader.item;
         fragment_Shader : aliased Shader.item;
         Program         :         openGL.Program.lit.view;
      end record;

   type Programs is array (program_Id) of aliased Program;


   -----------
   --- Globals
   --

   the_Programs : Programs;

   Attribute_1_Name : aliased C.char_array := "vert";
   Attribute_2_Name : aliased C.char_array := "vertNormal";
   Attribute_3_Name : aliased C.char_array := "aColor";
   Attribute_4_Name : aliased C.char_array := "vertTexCoord";
   Attribute_5_Name : aliased C.char_array := "Shine";

   Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'Access);
   Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'Access);
   Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_3_Name'Access);
   Attribute_4_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_4_Name'Access);
   Attribute_5_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_5_Name'Access);

   white_Texture : openGL.Texture.Object;


   ---------
   --  Forge
   --

   type Geometry_view is access all Geometry.lit.item'Class;

   function new_Geometry (texture_is_Alpha : in Boolean) return access Geometry.lit.item'Class
   is
      use type openGL.Program.lit.view;


      procedure define (the_Program         : access Program;
                        use_fragment_Shader : in     String)
      is
         use openGL.Palette,
             Attribute.Forge,
             system.Storage_Elements;

         Sample : Vertex;

         Attribute_1 : openGL.Attribute.view;
         Attribute_2 : openGL.Attribute.view;
         Attribute_3 : openGL.Attribute.view;
         Attribute_4 : openGL.Attribute.view;
         Attribute_5 : openGL.Attribute.view;

         white_Image : constant openGL.Image := (1 .. 2 => (1 .. 2 => +White));

      begin
         white_Texture       := openGL.Texture.Forge.to_Texture (white_Image);
         the_Program.Program := new openGL.Program.lit.item;

         the_Program.  vertex_Shader.define (Shader.Vertex,   "assets/opengl/shader/lit.vert");
         the_Program.fragment_Shader.define (Shader.Fragment, use_fragment_Shader);

         the_Program.Program.define (the_Program.  vertex_Shader'Access,
                                     the_Program.fragment_Shader'Access);
         the_Program.Program.enable;

         Attribute_1 := new_Attribute (Name        => "vert",
                                       gl_Location => the_Program.Program.attribute_Location ("vert"),
                                       Size        => 3,
                                       data_Kind   => attribute.GL_FLOAT,
                                       Stride      => lit.Vertex'Size / 8,
                                       Offset      => 0,
                                       Normalized  => False);

         Attribute_2 := new_Attribute (Name        => "vertNormal",
                                       gl_Location => the_Program.Program.attribute_Location ("vertNormal"),
                                       Size        => 3,
                                       data_Kind   => attribute.GL_FLOAT,
                                       Stride      => lit.Vertex'Size / 8,
                                       Offset      =>   Sample.Normal (1)'Address
                                                      - Sample.Site   (1)'Address,
                                       Normalized  => False);

         Attribute_3 := new_Attribute (Name        => "aColor",
                                       gl_Location => the_Program.Program.attribute_Location ("aColor"),
                                       Size        => 4,
                                       data_Kind   => attribute.GL_UNSIGNED_BYTE,
                                       Stride      => lit.Vertex'Size / 8,
                                       Offset      =>   Sample.Color.Primary.Red'Address
                                                      - Sample.Site (1)         'Address,
                                       Normalized  => True);

         Attribute_4 := new_Attribute (Name        => "vertTexCoord",
                                       gl_Location => the_Program.Program.attribute_Location ("vertTexCoord"),
                                       Size        => 2,
                                       data_Kind   => attribute.GL_FLOAT,
                                       Stride      => lit.Vertex'Size / 8,
                                       Offset      =>   Sample.Coords.S'Address
                                                      - Sample.Site (1)'Address,
                                       Normalized  => False);

         Attribute_5 := new_Attribute (Name        => "Shine",
                                       gl_Location => the_Program.Program.attribute_Location ("Shine"),
                                       Size        => 1,
                                       data_Kind   => attribute.GL_FLOAT,
                                       Stride      => lit.Vertex'Size / 8,
                                       Offset      =>   Sample.Shine   'Address
                                                      - Sample.Site (1)'Address,
                                       Normalized  => False);

         the_Program.Program.add (Attribute_1);
         the_Program.Program.add (Attribute_2);
         the_Program.Program.add (Attribute_3);
         the_Program.Program.add (Attribute_4);
         the_Program.Program.add (Attribute_5);

         glBindAttribLocation (Program =>  the_Program.Program.gl_Program,
                               Index   =>  the_Program.Program.Attribute (named => "vert").gl_Location,
                               Name    => +Attribute_1_Name_ptr);
         Errors.log;

         glBindAttribLocation (Program =>  the_Program.Program.gl_Program,
                               Index   =>  the_Program.Program.Attribute (named => "vertNormal").gl_Location,
                               Name    => +Attribute_2_Name_ptr);
         Errors.log;

         glBindAttribLocation (Program =>  the_Program.Program.gl_Program,
                               Index   =>  the_Program.Program.Attribute (named => "aColor").gl_Location,
                               Name    => +Attribute_3_Name_ptr);
         Errors.log;

         glBindAttribLocation (Program =>  the_Program.Program.gl_Program,
                               Index   =>  the_Program.Program.Attribute (named => "vertTexCoord").gl_Location,
                               Name    => +Attribute_4_Name_ptr);
         Errors.log;

         glBindAttribLocation (Program =>  the_Program.Program.gl_Program,
                               Index   =>  the_Program.Program.Attribute (named => "Shine").gl_Location,
                               Name    => +Attribute_5_Name_ptr);
         Errors.log;
      end define;

      Self : constant Geometry_view := new Geometry.lit.item;

   begin
      Tasks.check;

      if texture_is_Alpha     --  Define the shaders and program, if required.
      then
         if the_Programs (alpha_Texture).Program = null
         then
            define (the_Programs (alpha_Texture)'Access,
                    use_fragment_Shader => "assets/opengl/shader/lit.frag");
                    --  use_fragment_Shader => "assets/opengl/shader/lit_colored_textured-text.frag");
         end if;
      else
         if the_Programs (rgba_Texture).Program = null
         then
            define (the_Programs (rgba_Texture)'Access,
                    use_fragment_Shader => "assets/opengl/shader/lit.frag");
                    --  use_fragment_Shader => "assets/opengl/shader/lit_colored_textured.frag");
         end if;
      end if;

      if texture_is_Alpha
      then   Self.Program_is (the_Programs (alpha_Texture).Program.all'Access);
      else   Self.Program_is (the_Programs ( rgba_Texture).Program.all'Access);
      end if;

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
      use      openGL_Buffer_of_geometry_Vertices;
      use type Buffer.view;
   begin
      if Self.Vertices = null
      then
         self.Vertices := new openGL_Buffer_of_geometry_Vertices.Object' (Forge.to_Buffer (Now,
                                                                                           usage => Buffer.static_Draw));
      else
         set (openGL_Buffer_of_geometry_Vertices.Object (Self.Vertices.all),
              to => Now);
      end if;

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


end openGL.Geometry.lit;
