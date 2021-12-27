with
     openGL.Geometry.lit_colored,
     openGL.Primitive.indexed;


package body openGL.Model.polygon.lit_colored
is

   function new_Polygon (Vertices : in Vector_2_array;
                         Color    : in lucid_Color) return View
   is
      Self : constant View := new Item;
   begin
      Self.Color := Color;

      Self.Vertices (Vertices'Range) := Vertices;
      Self.vertex_Count              := Vertices'Length;

      return Self;
   end new_Polygon;



   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use Geometry,
          Geometry.lit_colored;

      vertex_Count  : constant      Index_t :=      Index_t (Self.vertex_Count);
      indices_Count : constant long_Index_t := long_Index_t (Self.vertex_Count);

      the_Vertices  : aliased  Geometry.lit_colored.Vertex_array := (1 .. vertex_Count  => <>);
      the_Indices   : aliased  Indices                           := (1 .. indices_Count => <>);

      Color         : constant rgba_Color                := +Self.Color;
      the_Geometry  : constant Geometry.lit_colored.view := Geometry.lit_colored.new_Geometry;

   begin
      set_Vertices:
      begin
         for i in 1 .. vertex_Count
         loop
            the_Vertices (i).Site   := Vector_3 (Self.Vertices (Integer (i)) & 0.0);
            the_Vertices (i).Normal := (0.0, 0.0, 1.0);
            the_Vertices (i).Color  := Color;
            the_Vertices (i).Shine  := 0.5;
         end loop;
      end set_Vertices;

      --- Set Indices.
      --
      for i in the_Indices'Range
      loop
         the_Indices (i) := Index_t (i);
      end loop;

      the_Geometry.is_Transparent (False);
      the_Geometry.Vertices_are   (the_Vertices);

      declare
         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (Primitive.triangle_Fan,
                                               the_Indices);
      begin
         the_Geometry.add (Primitive.view (the_Primitive));
      end;

      return (1 => Geometry.view (the_Geometry));
   end to_GL_Geometries;


end openGL.Model.polygon.lit_colored;
