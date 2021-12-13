with
     openGL.Primitive.indexed,
     openGL.Primitive,

     ada.unchecked_Deallocation;


package body openGL.Model.segment_line
is

   function to_segment_line_Model (Color : in openGL.Color) return Item
   is
      Self : constant Item := (Model.item with
                               +Color,
                               site_Vectors.empty_Vector,
                               others => <>);
   begin
      return Self;
   end to_segment_line_Model;



   function new_segment_line_Model (Color : in openGL.Color) return View
   is
   begin
      return new Item' (to_segment_line_Model (Color));
   end new_segment_line_Model;



   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use Geometry.colored,
          Primitive,
          Primitive.indexed,
          ada.Containers;

      vertex_Count  : constant      Index_t :=      Index_t (Self.Points.Length);
      indices_Count : constant long_Index_t := long_Index_t (vertex_Count);
      the_Indices   : aliased       Indices := (1 .. indices_Count => <>);

   begin
      if Self.Points.Length <= 2
      then
         return (1..0 => <>);
      end if;

      for i in the_Indices'Range
      loop
         the_Indices (i) := Index_t (i);
      end loop;

      Self.Geometry := Geometry.colored.new_Geometry;

      Self.Geometry.is_Transparent (False);
      Self.Geometry.Vertices_are (Self.Vertices (1 .. Index_t (Self.vertex_Count)));
      Self.Geometry.add (Primitive.view (new_Primitive (Line_Strip,
                                                        the_Indices)));
      return (1 => Self.Geometry.all'Access);
   end to_GL_Geometries;



   function Site (Self : in  Item;   for_End : in Integer) return Vector_3
   is
   begin
      return Self.Vertices (Index_t (for_End)).Site;
   end Site;



   function segment_Count (Self : in Item) return Natural
   is
   begin
      return Natural (Self.Points.Length) - 1;
   end segment_Count;



   procedure add_1st_Segment (Self : in out Item;   start_Site : in Vector_3;
                                                    end_Site   : in Vector_3)
   is
      use site_Vectors;
   begin
      pragma assert (Self.Points.Is_Empty);

      Self.Points.append (start_Site);
      Self.Points.append (end_Site);

      Self.vertex_Count := Self.vertex_Count + 1;

      Self.Vertices (Index_t (Self.vertex_Count)).Site  := start_Site;
      Self.Vertices (Index_t (Self.vertex_Count)).Color := (Self.Color, opaque_Value);

      Self.vertex_Count := Self.vertex_Count + 1;

      Self.Vertices (Index_t (Self.vertex_Count)).Site  := end_Site;
      Self.Vertices (Index_t (Self.vertex_Count)).Color := (Self.Color, opaque_Value);

      Self.needs_Rebuild := True;
   end add_1st_Segment;



   procedure add_Segment (Self : in out Item;   end_Site : in Vector_3)
   is
      use type ada.Containers.Count_type;

      procedure deallocate is new ada.unchecked_Deallocation (Geometry.colored.Vertex_array,
                                                              vertex_Array_view);
   begin
      pragma assert (not Self.Points.is_Empty);

      Self.Points.append (end_Site);

      if Self.Points.Length > Self.Vertices'Length
      then
         declare
            new_Vertices : constant vertex_Array_view
              := new Geometry.colored.Vertex_array (1 .. 2 * Self.Vertices'Length);
         begin
            new_Vertices (1 .. Self.Vertices'Length) := Self.Vertices.all;

            deallocate (Self.Vertices);
            Self.Vertices := new_Vertices;
         end;
      end if;

      Self.vertex_Count := Self.vertex_Count + 1;

      Self.Vertices (Index_t (Self.vertex_Count)).Site  := end_Site;
      Self.Vertices (Index_t (Self.vertex_Count)).Color := (Self.Color, opaque_Value);

      Self.needs_Rebuild := True;
   end add_Segment;



   procedure Site_is (Self : in out Item;   Now     : in Vector_3;
                                            for_End : in Integer)
   is
   begin
      Self.Vertices (Index_t (for_End)).Site := Now;
      Self.Points.replace_Element (for_End, Now);
      set_Bounds (Self);
      Self.needs_Rebuild := True;
   end Site_is;



   procedure Color_is (Self : in out Item;   Now     : in Color;
                                             for_End : in Integer)
   is
   begin
      Self.Vertices (Index_t (for_End)).Color := (+Now, opaque_Value);
      Self.needs_Rebuild := True;
   end Color_is;



   function Segments (Self : in Item) return Segments_t
   is
      the_Segments : Segments_t (1 .. Integer (Self.Points.Length) - 1);
   begin
      for Each in the_Segments'Range
      loop
         the_Segments (Each) := (First => Self.Points.Element (Each),
                                 Last  => Self.Points.Element (Each + 1));
      end loop;

      return the_Segments;
   end Segments;



   function Angle_in_xz_plane (the_Segment : in Segment) return Radians
   is
      use real_Functions;
      the_Vector : constant Vector_3 := the_Segment.Last - the_Segment.First;
   begin
      return arcTan (the_Vector (3) / the_Vector (1));
   end Angle_in_xz_plane;


end openGL.Model.segment_line;
