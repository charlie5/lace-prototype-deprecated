with
     float_Math.Geometry.d3.Modeller.Forge;

package body openGL.IO.lat_long_Radius
is

   function to_Model (math_Model : access Geometry_3d.a_Model) return IO.Model
   is
      site_Count   : constant long_Index_t  := long_Index_t (math_Model.site_Count);
      coord_Count  : constant long_Index_t  := 0; --get_coord_Count;                        -- TODO: Add texturing.
      normal_Count : constant long_Index_t  := 0; --collada_Normals'Length   / 3;           -- TODO: Add lighting.

      the_Sites    : constant Sites_view    := new many_Sites   (1 .. site_Count);
      the_Normals  : constant Normals_view  := new many_Normals (1 .. normal_Count);
      the_Coords   :          Coords_view;

      the_Faces    :          IO.Faces_view := new IO.Faces (1 .. 50_000);
      face_Count   :          long_Index_t  := 0;

   begin
      if coord_Count > 0
      then
         the_Coords := new many_Coordinates_2D (1 .. coord_Count);
      end if;

      for i in 1 .. Integer (site_Count)
      loop
         the_Sites (long_Index_t (i)) := math_Model.Sites (i);
      end loop;


      --  Primitives
      --
      declare
         the_Vertices : Vertices (1 .. long_Index_t (math_Model.tri_Count * 3));
         Start        : long_Index_t;
         the_Face     : IO.Face;
      begin
         for i in math_Model.Triangles'Range
         loop
            Start := long_Index_t ((i - 1) * 3  +  1);

            the_Vertices (Start    ) := (site_Id => long_Index_t (math_Model.Triangles (i) (1)),  others => 0);
            the_Vertices (Start + 1) := (site_Id => long_Index_t (math_Model.Triangles (i) (2)),  others => 0);
            the_Vertices (Start + 2) := (site_Id => long_Index_t (math_Model.Triangles (i) (3)),  others => 0);

            the_Face := (Triangle,
                         the_Vertices (Start .. Start + 2));

            face_Count             := face_Count + 1;
            the_Faces (face_Count) := the_Face;
         end loop;
      end;

      declare
         used_Faces : constant IO.Faces_view := new IO.Faces' (the_Faces (1 .. face_Count));
      begin
         free (the_Faces);

         return (Sites   => the_Sites,
                 Coords  => the_Coords,
                 Normals => the_Normals,
                 Weights => null,
                 Faces   => used_Faces);
      end;
   end to_Model;



   function to_Model (model_File : in String) return IO.Model
   is
      use float_Math.Geometry.d3.Modeller.Forge;

      the_math_Model : aliased Geometry_3d.a_Model := mesh_Model_from (Model => polar_Model_from (model_File));
   begin
      return to_Model (the_math_Model'Access);
   end to_Model;


end openGL.IO.lat_long_Radius;
