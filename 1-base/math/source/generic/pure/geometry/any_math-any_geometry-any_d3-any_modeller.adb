with
     ada.Unchecked_Conversion;


package body any_math.any_geometry.any_d3.any_Modeller
is

   use ada.containers;
   use type Real;


   function Hash (the_Site : in my_Vertex) return ada.Containers.Hash_Type
   is
      function to_Hash is new ada.Unchecked_Conversion (Real, ada.Containers.Hash_Type);
   begin
      return to_Hash (the_Site (1) + the_Site (2) + the_Site (3));
   end Hash;



   function demand_Index (Self       : access item;
                          for_Vertex : in     my_Vertex) return Natural
   is
      use vertex_index_Maps;
      use vertex_Vectors;

      rounded_Vertex : constant my_Vertex               := for_Vertex;
      map_Cursor     : constant vertex_index_map_Cursor := find (Self.vertex_index_Map, rounded_Vertex);

   begin
      if has_Element (map_Cursor)
      then
         return Element (map_Cursor);
      end if;

      append (Self.Vertices,          Vertex (rounded_Vertex));
      insert (Self.vertex_index_Map,  rounded_Vertex,
                                      Natural (Length (Self.Vertices)));

      return Natural (Length (Self.Vertices));
   end demand_Index;



   function "<" (L, R : in index_Triangle) return Boolean
   is
   begin
      if L (1) < R (1) then return True;  end if;
      if L (1) > R (1) then return False; end if;

      if L (2) < R (2) then return True;  end if;
      if L (2) > R (2) then return False; end if;

      if L (3) < R (3) then return True;  end if;

      return False;
   end "<";



   procedure add_Triangle (Self : in out item;   Vertex_1, Vertex_2, Vertex_3 : in Site)
   is
      use index_triangle_Sets;

      vertex_1_Index         : constant Natural        := demand_Index (Self'Access, my_Vertex (Vertex_1));
      vertex_2_Index         : constant Natural        := demand_Index (Self'Access, my_Vertex (Vertex_2));
      vertex_3_Index         : constant Natural        := demand_Index (Self'Access, my_Vertex (Vertex_3));

      new_Triangle           : constant index_Triangle := (vertex_1_Index, vertex_2_Index, vertex_3_Index);
      new_Triangle_rotated_1 : constant index_Triangle := (vertex_3_Index, vertex_1_Index, vertex_2_Index);
      new_Triangle_rotated_2 : constant index_Triangle := (vertex_2_Index, vertex_3_Index, vertex_1_Index);

   begin
      if        new_Triangle (1) = new_Triangle (2)
        or else new_Triangle (1) = new_Triangle (3)
        or else new_Triangle (2) = new_Triangle (3)
      then
         null;        -- Discard collapsed triangle.

      else
         if        contains (Self.triangles, new_triangle)
           or else contains (Self.triangles, new_triangle_rotated_1)
           or else contains (Self.triangles, new_triangle_rotated_2)
         then
            null;     -- Triangle is already present.
         else
            include (Self.Triangles, new_Triangle);
         end if;
      end if;
   end add_Triangle;



   procedure clear (Self : in out Item)
   is
      use index_triangle_Sets, vertex_Vectors, vertex_index_Maps;
   begin
      Self.Triangles       .clear;
      Self.Vertices        .clear;
      Self.vertex_index_Map.clear;
   end clear;



   function triangle_Count (Self : in item) return Natural
   is
      use index_triangle_Sets;
   begin
      return Natural (Length (Self.Triangles));
   end triangle_Count;



   function Model (Self : in item) return a_Model
   is
      the_Model : a_Model := (site_Count => Integer (Self.Vertices.Length),
                              tri_Count  => Integer (Self.Triangles.Length),
                              sites      => <>,
                              triangles  => <>);
   begin
      declare
         use vertex_Vectors;
      begin
         for i in 1 .. Index (the_model.site_Count)
         loop
            the_Model.Sites (i) := Self.Vertices.Element (i);
         end loop;
      end;

      declare
         use index_triangle_Sets;
         Cursor  : index_triangle_Sets.Cursor := First (Self.Triangles);
      begin
         for i in 1 .. the_Model.tri_Count
         loop
            the_Model.Triangles (i) := (Element (Cursor));
            next (Cursor);
         end loop;
      end;

      return the_Model;
   end Model;



   function bounding_sphere_Radius (Self : access item) return Real
   is
   begin

      if Self.bounding_sphere_Radius = Real'First
      then
         declare
            use Functions, vertex_Vectors;
            Cursor     : vertex_Vectors.Cursor := Self.Vertices.First;
            the_Vertex : Vertex;
         begin
            while has_Element (Cursor)
            loop
               the_Vertex                  := Element (Cursor);
               Self.bounding_sphere_Radius := Real'Max (Self.bounding_sphere_Radius,
                                                          Sqrt (the_Vertex (1) * the_Vertex (1)
                                                                + the_Vertex (2) * the_Vertex (2)
                                                                + the_Vertex (3) * the_Vertex (3)));
               next (Cursor);
            end loop;
         end;
      end if;

      return Self.bounding_sphere_Radius;
   end bounding_sphere_Radius;


end any_math.any_geometry.any_d3.any_Modeller;
