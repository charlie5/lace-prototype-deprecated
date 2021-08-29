package body any_Math.any_Geometry
is

   function Image (Self : in Triangle)  return String
   is
   begin
      return "(" & Vertex_Id'Image (Self (1)) & ","
                 & Vertex_Id'Image (Self (2)) & ","
                 & Vertex_Id'Image (Self (3)) & ")";
   end Image;



   function Image (Self : in Triangles) return String
   is
      Result : String (1 .. 1024);
      Last   : Standard.Natural  := 0;
   begin
      for Each in Self'Range
      loop
         declare
            Id_Image : constant String := Image (Self (Each));
         begin
            Result (Last + 1 .. Last + Id_Image'Length) := Id_Image;
            Last                                        := Last + Id_Image'Length;
         end;
      end loop;

      return Result (1 .. Last);

   exception
      when Constraint_Error =>
         declare
            Ellipsis : constant String := " ...";
         begin
            Result (Result'Last - ellipsis'Length + 1 .. Result'Last) := ellipsis;
            return Result (1 .. Last);
         end;
   end Image;



   function Image (Self : in Model) return String
   is
   begin
      return Self.Triangles.Image;
   end Image;



   function Image (Self : in Model_Triangles) return String
   is
   begin
      return   "Triangle_Count =>" & standard.Positive'Image (Self.Triangle_Count)
             & Image (Self.Triangles);
   end Image;


end any_Math.any_Geometry;
