with
     openGL.Shader,
     openGL.Buffer.general,
     openGL.Attribute,
     openGL.Texture,
     openGL.Palette,
     openGL.Tasks,

     GL.lean,
     GL.Pointers,

     System,
     Interfaces.C.Strings,
     System.storage_Elements;


package body openGL.Geometry.lit_textured_skinned
is
   use GL.lean,
       GL.Pointers,

       Interfaces,
       System;


   --  Globals
   --
   the_vertex_Shader   : aliased openGL.Shader.item;
   the_fragment_Shader : aliased openGL.Shader.item;
   the_Program         : aliased openGL.Program.lit_textured_skinned.item;

   white_Texture       :         openGL.Texture.Object;


   ----------
   --  Vertex
   --

   function is_Transparent (Self : in Vertex_array) return Boolean   -- TODO: Replace this with the generic (check all similar use the generic).
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

   type Geometry_view is access all Geometry.lit_textured_skinned.item'class;


   function new_Geometry return access Geometry.lit_textured_skinned.item'class
   is
      Self : constant Geometry_view := new Geometry.lit_textured_skinned.item;
   begin
      Self.Program_is (openGL.Program.view (geometry.lit_textured_skinned.Program));
      return Self;
   end new_Geometry;


   --------------
   --  Attributes
   --

   function Program return openGL.Program.lit_textured_skinned.view
   is
      use type openGL.Program.lit_textured_skinned.view;
--        check_is_OK : constant Boolean   := openGL.Tasks.Check;   pragma Unreferenced (check_is_OK);
   begin
      raise Program_Error with "TODO";

--        if the_Program = null then   -- Define the shaders and program.
--           declare
--              use openGL.Palette,
--                  system.Storage_Elements;
--
--              sample_Vertex        : Vertex;
--
--              Attribute_1_Name     : aliased          C.char_array := "aSite";
--              Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr
--                := C.strings.to_chars_ptr (Attribute_1_Name'Unchecked_Access);
--
--              Attribute_2_Name     : aliased          C.char_array := "aNormal";
--              Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr
--                := C.strings.to_chars_ptr (Attribute_2_Name'Unchecked_Access);
--
--              Attribute_3_Name     : aliased          C.char_array := "aColor";
--              Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr
--                := C.strings.to_chars_ptr (Attribute_3_Name'Unchecked_Access);
--
--              Attribute_4_Name     : aliased          C.char_array := "aCoords";
--              Attribute_4_Name_ptr : aliased constant C.strings.chars_ptr
--                := C.strings.to_chars_ptr (Attribute_4_Name'Unchecked_Access);
--
--              Attribute_5_Name     : aliased          C.char_array := "bone_Ids";
--              Attribute_5_Name_ptr : aliased constant C.strings.chars_ptr
--                := C.strings.to_chars_ptr (Attribute_5_Name'Unchecked_Access);
--
--              Attribute_6_Name     : aliased          C.char_array := "bone_Weights";
--              Attribute_6_Name_ptr : aliased constant C.strings.chars_ptr
--                := C.strings.to_chars_ptr (Attribute_6_Name'Unchecked_Access);
--
--              Attribute_1 : openGL.Attribute.view;
--              Attribute_2 : openGL.Attribute.view;
--              Attribute_3 : openGL.Attribute.view;
--              Attribute_4 : openGL.Attribute.view;
--              Attribute_5 : openGL.Attribute.view;
--              Attribute_6 : openGL.Attribute.view;
--
--              white_Image : constant openGL.Image := (1 .. 2 => (1 .. 2 => White));
--           begin
--              white_Texture := openGL.Texture.to_Texture (white_Image);
--
--              the_vertex_Shader  .define (openGL.Shader.Vertex,   "assets/mmi/shader/lit_textured_skinned.vert");
--              the_fragment_Shader.define (openGL.Shader.Fragment, "assets/mmi/shader/lit_textured_skinned.frag");
--
--  --              the_Program := new openGL.Program.lit_textured_skinned.item;
--              the_Program.define (the_vertex_Shader  'Access,
--                                  the_fragment_Shader'Access);
--              the_Program.enable;
--
--              Attribute_1 := attribute.Forge.new_Attribute
--                               (name        => "aSite",
--                                gl_location => the_Program.attribute_Location ("aSite"),
--                                size        => 3,
--                                data_kind   => attribute.GL_FLOAT,
--                                stride      => lit_textured_skinned.Vertex'Size / 8,
--                                offset      => 0,
--                                normalized  => False);
--
--              Attribute_2 := attribute.Forge.new_Attribute
--                               (name        => "aNormal",
--                                gl_location => the_Program.attribute_Location ("aNormal"),
--                                size        => 3,
--                                data_kind   => attribute.GL_FLOAT,
--                                stride      => lit_textured_skinned.Vertex'Size / 8,
--                                offset      =>   sample_Vertex.Normal (1)'Address
--                                               - sample_Vertex.Site   (1)'Address,
--                                normalized  => False);
--
--              Attribute_3 := attribute.Forge.new_Attribute
--                               (name        => "aColor",
--                                gl_location => the_Program.attribute_Location ("aColor"),
--                                size        => 4,
--                                data_kind   => attribute.GL_UNSIGNED_BYTE,
--                                stride      => lit_textured_skinned.Vertex'Size / 8,
--                                offset      =>   sample_Vertex.Color.Primary.Red'Address
--                                               - sample_Vertex.Site (1)         'Address,
--                                normalized  => True);
--
--              Attribute_4 := attribute.Forge.new_Attribute
--                               (name        => "aCoords",
--                                gl_location => the_Program.attribute_Location ("aCoords"),
--                                size        => 2,
--                                data_kind   => attribute.GL_FLOAT,
--                                stride      => lit_textured_skinned.Vertex'Size / 8,
--                                offset      =>   sample_Vertex.Coords.S'Address
--                                               - sample_Vertex.Site (1)'Address,
--                                normalized  => False);
--
--              Attribute_5 := attribute.Forge.new_Attribute
--                               (name        => "bone_Ids",
--                                gl_location => the_Program.attribute_Location ("bone_Ids"),
--                                size        => 4,
--                                data_kind   => attribute.GL_FLOAT,
--                                stride      => lit_textured_skinned.Vertex'Size / 8,
--                                offset      =>   sample_Vertex.bone_Ids (1)'Address
--                                               - sample_Vertex.Site (1)'Address,
--                                normalized  => False);
--
--              Attribute_6 := attribute.Forge.new_Attribute
--                               (name        => "bone_Weights",
--                                gl_location => the_Program.attribute_Location ("bone_Weights"),
--                                size        => 4,
--                                data_kind   => attribute.GL_FLOAT,
--                                stride      => lit_textured_skinned.Vertex'Size / 8,
--                                offset      =>   sample_Vertex.bone_Weights (1)'Address
--                                               - sample_Vertex.Site (1)'Address,
--                                normalized  => False);
--
--              the_Program.add (Attribute_1);
--              the_Program.add (Attribute_2);
--              the_Program.add (Attribute_3);
--              the_Program.add (Attribute_4);
--              the_Program.add (Attribute_5);
--              the_Program.add (Attribute_6);
--
--              glBindAttribLocation (program => the_Program.gl_Program,
--                                    index   => the_Program.Attribute (named => "aSite").gl_Location,
--                                    name    => +Attribute_1_Name_ptr);
--
--              glBindAttribLocation (program => the_Program.gl_Program,
--                                    index   => the_Program.Attribute (named => "aNormal").gl_Location,
--                                    name    => +Attribute_2_Name_ptr);
--
--              glBindAttribLocation (program => the_Program.gl_Program,
--                                    index   => the_Program.Attribute (named => "aColor").gl_Location,
--                                    name    => +Attribute_3_Name_ptr);
--
--              glBindAttribLocation (program => the_Program.gl_Program,
--                                    index   => the_Program.Attribute (named => "aCoords").gl_Location,
--                                    name    => +Attribute_4_Name_ptr);
--
--              glBindAttribLocation (program => the_Program.gl_Program,
--                                    index   => the_Program.Attribute (named => "bone_Ids").gl_Location,
--                                    name    => +Attribute_5_Name_ptr);
--
--              glBindAttribLocation (program => the_Program.gl_Program,
--                                    index   => the_Program.Attribute (named => "bone_Weights").gl_Location,
--                                    name    => +Attribute_6_Name_ptr);
--           end;
--        end if;

      return the_Program'Access;
   end Program;



   overriding
   procedure Indices_are  (Self : in out Item;   Now       : in Indices;
                                                 for_Facia : in Positive)
   is
   begin
      raise Program_Error with "TODO";
   end Indices_are;



   package openGL_Buffer_of_geometry_Vertices is new openGL.Buffer.general (base_object   => openGL.Buffer.array_Object,
                                                                            index         => long_Index_t,
                                                                            element       => Vertex,
                                                                            element_array => Vertex_array);

   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array)
   is
      use openGL_Buffer_of_geometry_Vertices;
   begin
      Self.Vertices       := new openGL_Buffer_of_geometry_Vertices.object' (to_Buffer (Now,
                                                                                        usage => openGL.buffer.static_Draw));
      Self.is_Transparent := Self.is_Transparent or is_Transparent (Now);

      -- Set the bounds.
      --
      declare
         function get_Site (Index : in long_Index_t) return Vector_3
         is (Now (Index).Site);

         function bBox is new get_Bounds (long_Index_t, get_Site);
      begin
         Self.Bounds_are (bBox (count => Now'Length));
      end;
   end Vertices_are;



   overriding
   procedure enable_Texture (Self : in Item)
   is
      use GL,
          openGL.Texture;

      check_is_OK : constant Boolean := openGL.Tasks.Check;   pragma Unreferenced (check_is_OK);

   begin
      glActiveTexture (gl.GL_TEXTURE0);

      if Self.Texture = openGL.Texture.null_Object
      then
         if not white_Texture.is_Defined
         then
            declare
               use openGL.Palette;
               white_Image : constant openGL.Image := (1 .. 2 => (1 .. 2 => White));
            begin
               white_Texture := openGL.Texture.to_Texture (white_Image);
            end;
         end if;

         enable (white_Texture);
      else
         enable (Self.Texture);
      end if;
   end enable_Texture;


end openGL.Geometry.lit_textured_skinned;
