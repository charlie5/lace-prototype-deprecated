with
     ada.Strings.Hash;

package body any_Math.any_Geometry.any_d3.any_Modeller
is
   use ada.Containers;


   function Hash (Site : in my_Vertex) return ada.Containers.Hash_type
   is
      use ada.Strings;
   begin
      return Hash (  Site (1)'Image
                   & Site (2)'Image
                   & Site (3)'Image);
   end Hash;



   function demand_Index (Self       : in out Item;
                          for_Vertex : in     my_Vertex) return Natural
   --
   -- If the vertex exists in the map, return the associated index.
   -- Otherwise add the new vertex and return it's index.
   --
   is
      use Vertex_Maps_of_Index;
      Cursor : constant Vertex_Maps_of_Index.Cursor := Self.Index_Map.find (for_Vertex);
   begin
      if has_Element (Cursor)
      then
         return Element (Cursor);
      end if;

      Self.Vertices.append (Vertex (for_Vertex));
      declare
         new_Index : constant Natural := Natural (Self.Vertices.Length);
      begin
         Self.Index_Map.insert (for_Vertex, new_Index);
         return new_Index;
      end;
   end demand_Index;



   function "<" (Left, Right : in Index_Triangle) return Boolean
   is
   begin
      if Left (1) < Right (1) then return True;  end if;
      if Left (1) > Right (1) then return False; end if;

      if Left (2) < Right (2) then return True;  end if;
      if Left (2) > Right (2) then return False; end if;

      if Left (3) < Right (3) then return True;  end if;

      return False;
   end "<";



   procedure add_Triangle (Self : in out Item;   Vertex_1, Vertex_2, Vertex_3 : in Site)
   is
      vertex_1_Index         : constant Natural        := demand_Index (Self, my_Vertex (Vertex_1));
      vertex_2_Index         : constant Natural        := demand_Index (Self, my_Vertex (Vertex_2));
      vertex_3_Index         : constant Natural        := demand_Index (Self, my_Vertex (Vertex_3));

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
         if        Self.Triangles.contains (new_triangle)
           or else Self.Triangles.contains (new_triangle_rotated_1)
           or else Self.Triangles.contains (new_triangle_rotated_2)
         then
            null;     -- Triangle is already present.
         else
            Self.Triangles.include (new_Triangle);
         end if;
      end if;
   end add_Triangle;



   procedure clear (Self : in out Item)
   is
   begin
      Self.Triangles.clear;
      Self.Vertices .clear;
      Self.Index_Map.clear;
   end clear;



   function Triangle_Count (Self : in Item) return Natural
   is
   begin
      return Natural (Self.Triangles.Length);
   end triangle_Count;



   function Model (Self : in Item) return a_Model
   is
      Result : a_Model := (Site_Count => Integer (Self.Vertices.Length),
                           Tri_Count  => Integer (Self.Triangles.Length),
                           Sites      => <>,
                           Triangles  => <>);
   begin
      for i in 1 .. Index (Result.site_Count)
      loop
         Result.Sites (i) := Self.Vertices.Element (i);
      end loop;

      declare
         use Index_Triangle_Sets;
         Cursor : Index_Triangle_Sets.Cursor := Self.Triangles.First;
      begin
         for i in 1 .. Result.Tri_Count
         loop
            Result.Triangles (i) := Element (Cursor);
            next (Cursor);
         end loop;
      end;

      return Result;
   end Model;



   function bounding_Sphere_Radius (Self : in out Item) return Real
   is
      use Functions;
   begin
      if Self.bounding_Sphere_Radius = Real'First
      then
         for Each of Self.Vertices
         loop
            Self.bounding_sphere_Radius := Real'Max (Self.bounding_sphere_Radius,
                                                     SqRt (  Each (1) * Each (1)
                                                           + Each (2) * Each (2)
                                                           + Each (3) * Each (3)));
         end loop;
      end if;

      return Self.bounding_sphere_Radius;
   end bounding_sphere_Radius;


end any_Math.any_Geometry.any_d3.any_Modeller;
