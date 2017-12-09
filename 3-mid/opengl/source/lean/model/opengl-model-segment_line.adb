with
     openGL.Primitive.indexed,
     openGL.Geometry.colored,
     openGL.Primitive,

     ada.Unchecked_Deallocation;
--  with Ada.Text_IO; use Ada.Text_IO;


package body openGL.Model.segment_line
is

   procedure define_internal (Self : in out Item);




   type vertex_Array_view is access all openGL.geometry.colored.Vertex_array;


   type State is
      record
         Vertices     :        Vertex_array_view := new openGL.geometry.colored.Vertex_array (1 .. 2);
         vertex_Count :        Natural           := 0;

         Geometry     : access openGL.Geometry.colored.item'Class;
      end record;




   function to_segment_line_Model (Scale : math.Vector_3;
                                   Color : openGL.Color) return Model.segment_line.item
   is
      pragma Unreferenced (Scale);
      Self : Model.segment_line.item := (openGL.Model.item with
                                         Color,
                                         site_Vectors.Empty_Vector,
--                                           null_Bounds,
                                         new State);
   begin
      define_internal (Self);
      return Self;
   end to_segment_line_Model;



   function new_segment_line_Model (Scale : math.Vector_3;
                                    Color : openGL.Color) return Model.segment_line.view
   is
   begin
      return new Item' (to_segment_line_Model (Scale, Color));
   end new_segment_line_Model;



   procedure define_internal (Self : in out Item)
   is
      use openGL.Geometry.colored,
          openGL.Primitive,
          openGL.Primitive.indexed;

      use type ada.containers.count_type;

      vertex_Count      : constant openGL.Index_t      := openGL.     Index_t (Self.Points.Length + 1);
      indices_Count     : constant openGL.long_Index_t := openGL.long_Index_t (vertex_Count);

      the_Indices       : aliased  openGL.Indices      := (1 .. indices_Count => <>);

   begin
      for Each in the_Indices'Range
      loop
         the_Indices (Each) := openGL.Index_t (Each);
      end loop;
   end define_internal;



--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds
--     is
--     begin
--        return Self.Bounds;
--     end Bounds;


   procedure set_Indices (Self : in out Item)
   is
      use type ada.Containers.Count_Type;

      vertex_Count  : constant openGL.Index_t      := openGL.Index_t    (Self.Points.Length);
      indices_Count : constant openGL.long_Index_t := openGL.long_Index_t (vertex_Count);

      the_Indices   : aliased  Indices             := (1 .. indices_Count => <>);

   begin
      if the_Indices'Length > 1
      then
         for Each in the_Indices'Range
         loop
            the_Indices (Each) := openGL.Index_t (Each);
         end loop;

         Self.State.Geometry.Indices_are (the_Indices, for_facia => 1);
      end if;
   end set_Indices;




--     overriding
--     procedure set_Bounds (Self : in out Item)
--     is
--        the_Vertices : Vector_3_array (1 .. Index_t (Self.Points.Length));
--     begin
--        for i in the_Vertices'Range
--        loop
--           the_Vertices (i) := Self.Points.Element (Integer (i));
--        end loop;
--
--        Self.Bounds := bounding_Box_of (the_Vertices);
--     end set_Bounds;





   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use openGL.Geometry.colored,
          openGL.Primitive,
          openGL.Primitive.indexed,
          ada.Containers;

      vertex_Count  : constant openGL.Index_t      := openGL.Index_t (Self.Points.Length);
      indices_Count : constant openGL.long_Index_t := openGL.long_Index_t (vertex_Count);

      the_Indices   : aliased  Indices             := (1 .. indices_Count => <>);

   begin
      if Self.Points.Length <= 2
      then
         return (1..0 => <>);
      end if;

      for Each in the_Indices'Range
      loop
         the_Indices (Each) := openGL.Index_t (Each);
      end loop;

      Self.State.Geometry := openGL.Geometry.colored.new_Geometry;

      Self.state.Geometry.is_Transparent (False);

      Vertices_are (Self.State.Geometry.all,
                    Self.State.Vertices (1 .. Index_t (Self.State.vertex_Count)));

      Self.State.Geometry.add (openGL.Primitive.view (new_Primitive (Line_Strip, the_Indices)));

      return (1 => Self.State.Geometry.all'Access);
   end to_GL_Geometries;



   function Site (Self : in  Item;   for_End : in Integer) return math.Vector_3
   is
      use openGL.Geometry.colored;
   begin
      return Self.State.Vertices (openGL.Index_t (for_End)).Site;
   end Site;



   function segment_Count (Self : in Item) return Natural
   is
   begin
      return Natural (Self.Points.Length) - 1;
   end segment_Count;



   procedure add_1st_Segment (Self : in out Item;   start_Site : in math.Vector_3;
                                                    end_Site   : in math.Vector_3)
   is
      use openGL.Geometry.colored,
          site_Vectors;
   begin
      pragma assert (Self.Points.Is_Empty);

      Self.Points.append (start_Site);
      Self.Points.append (end_Site);


      Self.State.vertex_Count := Self.State.vertex_Count + 1;

      Self.State.Vertices (openGL.Index_t (Self.State.vertex_Count)).Site  := start_Site;
      Self.State.Vertices (openGL.Index_t (Self.State.vertex_Count)).Color := (Self.Color, Opaque);


      Self.State.vertex_Count := Self.State.vertex_Count + 1;

      Self.State.Vertices (openGL.Index_t (Self.State.vertex_Count)).Site  := end_Site;
      Self.State.Vertices (openGL.Index_t (Self.State.vertex_Count)).Color := (Self.Color, Opaque);

      Self.needs_Rebuild := True;
   end add_1st_Segment;




   procedure add_Segment     (Self : in out Item;   end_Site   : in math.Vector_3)
   is
      use openGL.Geometry.colored;
      use type ada.containers.Count_Type;

      procedure free is new ada.Unchecked_Deallocation (openGL.geometry.colored.Vertex_array,  vertex_array_view);

   begin
      pragma assert (not Self.Points.is_Empty);

      Self.Points.append (end_Site);

      if Self.Points.Length > Self.state.Vertices'Length
      then
         declare
            new_Vertices : constant vertex_array_view
              := new openGL.geometry.colored.Vertex_array (1 .. 2 * Self.state.Vertices'Length);
         begin
            new_Vertices (1 .. Self.state.Vertices'Length) := Self.State.vertices.all;

            free (Self.state.Vertices);
            Self.state.Vertices := new_Vertices;
         end;
      end if;


      Self.State.vertex_Count := Self.State.vertex_Count + 1;

      Self.State.Vertices (openGL.Index_t (Self.State.vertex_Count)).Site  := end_Site;
      Self.State.Vertices (openGL.Index_t (Self.State.vertex_Count)).Color := (Self.Color, Opaque);

      Self.needs_Rebuild := True;
   end add_Segment;



   procedure Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                            for_End : in Integer)
   is
      use openGL.Geometry.colored;
   begin
      Self.State.Vertices (openGL.Index_t (for_End)).Site := Now;
      Self.Points.replace_Element (for_End, Now);
      set_Bounds (Self);
      Self.needs_Rebuild := True;
   end Site_is;



   procedure Color_is (Self : in out Item;   Now     : in openGL.Color;
                                             for_End : in Integer)
   is
      use openGL.Geometry.colored;
   begin
      Self.State.Vertices (openGL.Index_t (for_End)).Color := (Now, 255);
      Self.needs_Rebuild := True;
   end Color_is;



   function Segments (Self : in Item) return Model.segment_line.Segments_t
   is
      the_Segments : Segments_t (1 .. Integer (Self.Points.Length) - 1);
   begin
      for Each in the_Segments'Range
      loop
         the_Segments (Each) := (first => Self.Points.Element (Each),
                                 last  => Self.Points.Element (Each + 1));
      end loop;

      return the_Segments;
   end Segments;



   function Angle_in_xz_plane (Self : in Segment) return math.Radians
   is
      the_Vector : math.Vector_3 := Self.Last - Self.First;
   begin
      return arcTan (the_Vector (3) / the_Vector (1));
   end Angle_in_xz_plane;




   -----------
   --- Streams
   --

   procedure Item_write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                         Self   : in     segment_Line.Item)
   is
   begin
      openGL.Color 'Write (Stream, Self.Color);
      site_Vector  'Write (Stream, Self.Points);
   end Item_write;



   procedure Item_read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                        Self   : out    segment_Line.Item)
   is
      the_Points : site_Vector;
   begin
      openGL.Color 'Read (Stream, Self.Color);
      site_Vector  'Read (Stream, the_Points);

      Self.State := new State;
      define_Internal (Self);

      declare
         use site_vectors;
         Cursor : site_vectors.Cursor := the_Points.First;
      begin
         while has_Element (Cursor)
         loop
            if Cursor = the_Points.First
            then
               Self.add_1st_Segment (Element       (Cursor),
                                     Element (Next (Cursor)));

            elsif Cursor = Next (the_Points.First)
            then
               null;

            else
               Self.add_Segment (Element (Cursor));
            end if;

            next (Cursor);
         end loop;
      end;
   end Item_read;


end openGL.Model.segment_line;
