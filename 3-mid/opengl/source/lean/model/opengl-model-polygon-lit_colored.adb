with
     openGL.Geometry.lit_colored,
     openGL.Primitive.indexed;


package body openGL.Model.polygon.lit_colored
is
   use      openGL;
   use type math.Real;


   package body Forge
   is
      function new_Polygon (Vertices : in Vector_2_array;    Color : openGL.lucid_Color) return View
      is
         Self : constant View := new Item;
      begin
         Self.Color  := (Color);

         Self.Vertices (Vertices'Range) := Vertices;
         Self.vertex_Count              := Vertices'Length;

         Self.Bounds := bounding_Box_of (to_Vector_3_array (Self.Vertices (1 .. Self.vertex_Count)));

         Self.define  (scale => (1.0, 1.0, 1.0));
         return Self;
      end new_Polygon;

   end Forge;



--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds
--     is
--     begin
--        return Self.Bounds;
--     end Bounds;



   type Geometry_view is access all openGL.Geometry.lit_colored.item'class;


   --  nb: - an extra vertex is required at the end of each latitude ring
   --      - this last vertex has the same site as the rings initial vertex.
   --      - the  last    vertex has 's' texture coord of 1.0, whereas
   --        the  initial vertex has 's' texture coord of 0.0
   --
   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use openGL.Geometry,
          openGL.Geometry.lit_colored;

      vertex_Count  : constant openGL.     Index_t := openGL.     Index_t (Self.vertex_Count);
      indices_Count : constant openGL.long_Index_t := openGL.long_Index_t (Self.vertex_Count);

      the_Vertices  : aliased  openGL.geometry.lit_colored.Vertex_array := (1 .. vertex_Count  => <>);
      the_Indices   : aliased  Indices                                  := (1 .. indices_Count => <>);

      the_Geometry  : constant Geometry_view       := Geometry_view (openGL.Geometry.lit_colored.new_Geometry);

   begin
      set_Vertices :
      declare
         use type math.Real;
      begin
         for Each in 1 .. vertex_Count
         loop
            the_Vertices (Each).Site   := math.Vector_3 (Self.Vertices (Integer (Each)) & 0.0);
            the_Vertices (Each).Normal := (0.0, 0.0, 1.0);
            the_Vertices (Each).Color  := Self.Color;
         end loop;
      end set_Vertices;


      --- Set Indices.
      --
      for Each in the_Indices'Range
      loop
         the_Indices (Each) := openGL.Index_t (Each);
      end loop;

      the_Geometry.is_Transparent (False);

      Vertices_are (the_Geometry.all, the_Vertices);

      declare
         the_Primitive : constant openGL.Primitive.indexed.view
           := openGL.Primitive.indexed.new_Primitive (primitive.triangle_Fan,
                                                      the_Indices);
      begin
         the_Geometry.add (openGL.Primitive.view (the_Primitive));
      end;

      return (1 => openGL.Geometry.view (the_Geometry));
   end to_GL_Geometries;


end openGL.Model.polygon.lit_colored;
