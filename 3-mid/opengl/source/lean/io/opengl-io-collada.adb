with
     collada.Document,
     collada.Library.geometries,
     collada.Library.controllers,

     float_Math,
     ada.Text_IO;


package body openGL.IO.collada
is
   package Math        renames float_Math;
   package std_Collada renames standard.Collada;

   use Ada.Text_IO;



   function to_Model (model_Path : in String;
                      Scale      : in Vector_3 := (1.0, 1.0, 1.0)) return IO.Model
   is
      use std_Collada.Library,
          std_Collada.Library.geometries;

      use type Math.Index,
               long_Index_t,
               Real;

      which_Geometry    : constant := 1;     -- Select which gemometry.

      the_Document      : constant        std_Collada.Document.item := std_Collada.Document.to_Document (model_Path);

      the_Mesh          : constant        geometries.Mesh           := the_Document.Libraries.Geometries.Contents (which_Geometry).Mesh;
      the_Primitive     : constant        geometries.Primitive      := the_Mesh.Primitives (1);

      collada_Positions : constant access std_Collada.Float_array   := Positions_of (the_Mesh);
      collada_Normals   : constant access std_Collada.Float_array   := Normals_of   (the_Mesh, the_Primitive);
      collada_Coords    : constant access std_Collada.Float_array   := Coords_of    (the_Mesh, the_Primitive);


      function get_coord_Count return long_Index_t
      is
      begin
         if collada_Coords = null
         then
            return 0;
         else
            return collada_Coords'Length / 2;
         end if;
      end get_coord_Count;


      site_Count        : constant long_Index_t  := collada_Positions'Length / 3;
      coord_Count       : constant long_Index_t  := get_coord_Count;
      normal_Count      : constant long_Index_t  := collada_Normals'Length   / 3;

      the_Sites         : constant Sites_view    := new many_Sites          (1 .. site_Count);
      the_Coords        :          Coords_view;
      the_Normals       : constant Normals_view  := new many_Normals        (1 .. normal_Count);
      the_Weights       :          bone_Weights_array_view;

      the_Faces         :          IO.Faces_view := new IO.Faces (1 .. 50_000);
      face_Count        :          long_Index_t  := 0;

   begin
      if coord_Count > 0
      then
         the_Coords := new many_Coordinates_2D (1 .. coord_Count);
      end if;

      for Each in 1 .. Integer (site_Count)
      loop
         the_Sites (long_Index_t (Each)) := (  openGL.Real (collada_Positions (3 * (Each - 1) + 1))
                                             * Scale (1),
                                               openGL.Real (collada_Positions (3 * (Each - 1) + 2))
                                             * Scale (2),
                                               openGL.Real (collada_Positions (3 * (Each - 1) + 3))
                                             * Scale (3));
      end loop;


      for Each in 1 .. Integer (normal_Count)
      loop
         the_Normals (long_Index_t (Each)) := (collada_Normals (3 * (Each - 1) + 1),
                                               collada_Normals (3 * (Each - 1) + 2),
                                               collada_Normals (3 * (Each - 1) + 3));
      end loop;


      if collada_Coords /= null
      then
         for Each in 1 .. Integer (coord_Count)
         loop
            the_Coords (long_Index_t (Each)) := (openGL.Real (collada_Coords (2 * (Each - 1) + 1)),
                                                 openGL.Real (collada_Coords (2 * (Each - 1) + 2)));
         end loop;
      end if;


      --  Skinning
      --
      if         the_Document.Libraries.Controllers.Contents       /= null
        and then the_Document.Libraries.Controllers.Contents'Length > 0
      then
         declare
            use std_Collada.Library.controllers;

            the_Controller  : constant controllers.Controller   :=      the_Document.Libraries.Controllers.Contents (which_Geometry);
            the_Skin        : constant controllers.Skin         :=      the_Controller.Skin;

            collada_Weights : constant access
                                       std_Collada.Float_array  :=      Weights_of (the_Skin);

            vCount          :          std_Collada.Int_array    renames the_Skin.vertex_Weights.vCount.all;
            V               :          std_Collada.Int_array    renames the_Skin.vertex_Weights.v.all;

            inputs_Count    : constant Math.Index               :=      the_Skin.vertex_Weights.Inputs'Length;
            vCursor         :          Math.Index               :=      0;

         begin
            the_Weights := new bone_Weights_array (1 .. long_Index_t (the_Skin.vertex_Weights.Count));

            for each_Vertex in vCount'Range
            loop
               declare
                  the_Count     : constant long_Index_t      :=      long_Index_t (vCount       (each_Vertex));
                  these_Weights :          bone_Weights_view renames the_Weights  (long_Index_t (each_Vertex));
                  Base          :          Math.Index;
               begin
                  these_Weights := new bone_Weights (1 .. the_Count);

                  for Each in 1 .. the_Count
                  loop
                     vCursor := vCursor + 1;
                     Base    := (vCursor - 1) * inputs_Count + 1;

                     these_Weights (Each).Bone   := bone_Id (  1
                                                             + V (Base + joint_Offset_of (the_Skin.vertex_weights)));
                     these_Weights (Each).Weight := Real (collada_Weights (  1
                                                                           + Math.Index (V (  Base
                                                                                            + weight_Offset_of (the_Skin.vertex_weights)))));
                  end loop;
               end;
            end loop;
         end;
      end if;


      --  Primitives
      --
      case the_Primitive.Kind
      is
         when polyList =>
            parse_polyList :
            declare
               P            :          std_Collada.Int_array renames the_Primitive.P_List (1).all;
               inputs_Count : constant Natural               :=      the_Primitive.Inputs'Length;

               p_First      :          Math.Index            :=      1;
               p_Last       :          Math.Index;

               vertex_Count :          Natural;

            begin
               for Each in the_Primitive.vCount'Range
               loop
                  vertex_Count := the_Primitive.vCount (Each);
                  p_Last       :=   p_First
                                  + Math.Index (inputs_Count * vertex_Count)
                                  - 1;
                  declare
                     the_Vertices :          Vertices (1 .. long_Index_t (vertex_Count));

                     P_Indices    : constant std_Collada.Int_array (1 .. p_Last - p_First + 1) := P (p_First .. p_Last);
                     the_Face     :          IO.Face;
                     Base         :          Math.Index;
                  begin
                     for Each in the_Vertices'Range
                     loop
                        Base :=   Math.Index (Each - 1)
                                * Math.Index (inputs_Count)
                                + 1;

                        the_Vertices (Each).site_Id   :=   1
                                                         + long_Index_t (P_Indices (  Base
                                                                                    + vertex_Offset_of (the_Primitive)));
                        the_Vertices (Each).normal_Id :=   1
                                                         + long_Index_t (P_Indices (  Base
                                                                                    + normal_Offset_of (the_Primitive)));
                        if collada_Coords /= null
                        then
                           the_Vertices (Each).coord_Id :=   1
                                                           + long_Index_t (  P_Indices (Base
                                                                           + coord_Offset_of (the_Primitive)));
                        else
                           the_Vertices (Each).coord_Id := null_Id;
                        end if;

                        the_Vertices (Each).weights_Id := the_Vertices (Each).site_Id;
                     end loop;

                     case vertex_Count
                     is
                        when 3 =>   the_Face := (Triangle, the_Vertices);
                        when 4 =>   the_Face := (Quad,     the_Vertices);
                        when others =>
                           put_Line (  "parse_polyList ~ unhandled vertex count: "
                                     & Integer'Image (vertex_Count));
                     end case;

                     face_Count             := face_Count + 1;
                     the_Faces (face_Count) := the_Face;
                  end;

                  p_First := p_Last + 1;
               end loop;
            end parse_polyList;

         when Polygons =>
            parse_Polygons:
            declare
               inputs_Count : constant Natural := the_Primitive.Inputs'Length;
            begin
               for Each in the_Primitive.P_List'Range
               loop
                  declare
                     P_Indices    :          std_Collada.Int_array renames the_Primitive.P_List (Each).all;

                     vertex_Count : constant Natural := P_Indices'Length / inputs_Count;
                     the_Vertices :          Vertices (1 .. long_Index_t (vertex_Count));

                     the_Face     :          IO.Face;
                     Base         :          math.Index;
                  begin
                     for Each in the_Vertices'Range
                     loop
                        Base := Math.Index (  (Integer (Each) - 1)
                                            * inputs_Count
                                            + 1);

                        the_Vertices (Each).site_Id   :=   1
                                                         + long_Index_t (P_Indices (  Base
                                                                                    + vertex_Offset_of (the_Primitive)));
                        the_Vertices (Each).normal_Id :=   1
                                                         + long_Index_t (P_Indices (  Base
                                                                                    + normal_Offset_of (the_Primitive)));
                        if collada_Coords /= null
                        then
                           the_Vertices (Each).coord_Id :=   1
                                                           + long_Index_t (P_Indices (  Base
                                                                                      + coord_Offset_of (the_Primitive)));
                        else
                           the_Vertices (Each).coord_Id := null_Id;
                        end if;

                        the_Vertices (Each).weights_Id := the_Vertices (Each).site_Id;
                     end loop;

                     case vertex_Count
                     is
                        when 3      =>   the_Face := (Triangle, the_Vertices);
                        when 4      =>   the_Face := (Quad,     the_Vertices);
                        when others =>   put_Line (  "parse_Polygons ~ unhandled vertex count: "
                                                   & Integer'Image (vertex_Count));
                     end case;

                     face_Count             := face_Count + 1;
                     the_Faces (face_Count) := the_Face;
                  end;

               end loop;
            end parse_Polygons;

         when others =>
            put_Line (  "Warning: ignoring unimplemented primitive kind: "
                      & primitive_Kind'Image (the_Primitive.Kind));
      end case;


      declare
         used_Faces : constant IO.Faces_view := new IO.Faces' (the_Faces   (1 .. face_Count));
      begin
         free (the_Faces);

         return (Sites   => the_Sites,
                 Coords  => the_Coords,
                 Normals => the_Normals,
                 Weights => the_Weights,
                 Faces   => used_Faces);
      end;
   end to_Model;


end openGL.IO.collada;
