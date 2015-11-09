package body any_math.any_Geometry
is

   function Image (Self : in Triangle)  return String
   is
   begin
      return "(" & vertex_Id'Image (Self (1)) & ","
                 & vertex_Id'Image (Self (2)) & ","
                 & vertex_Id'Image (Self (3)) & ")";
   end Image;



   function Image (Self : in Triangles) return String
   is
      Pad  : String (1 .. 1024);
      Last : Standard.Natural  := 0;
   begin
      for Each in Self'Range
      loop
         declare
            id_Image : constant String := Image (Self (Each));
         begin
            Pad (Last + 1 .. Last + id_Image'Length) := id_Image;
            Last                                     := Last + id_Image'Length;
         end;
      end loop;

      return Pad (1 .. Last);

   exception
      when Constraint_Error =>
         declare
            ellipsis : constant String := " ...";
         begin
            Pad (Pad'Last - ellipsis'Length + 1 .. Pad'Last) := ellipsis;
            return Pad (1 .. Last);
         end;
   end Image;



   function Image (Self : in Model) return String
   is
   begin
      return self.triangles.Image;
   end Image;



   function Image (Self : in model_Triangles) return String
   is
   begin
      return
          "triangle_count =>" & Standard.Positive'Image (Self.triangle_Count)
        & Image (self.Triangles);
   end Image;


end any_math.any_Geometry;
