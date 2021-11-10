with
     collada.Document,
     collada.Library.geometries,
     collada.Library.controllers,

     ada.Text_IO;

package body openGL.IO.collada
is
   package std_Collada renames Standard.Collada;


   function to_Model (model_Path : in String) return IO.Model
   is
      use std_Collada.Library,
          std_Collada.Library.geometries,
          ada.Text_IO;

      which_Geometry    : constant := 1;     -- Select which gemometry, just for testing.

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


      site_Count   : constant long_Index_t  := collada_Positions'Length / 3;
      normal_Count : constant long_Index_t  := collada_Normals  'Length / 3;
      coord_Count  : constant long_Index_t  := get_coord_Count;

      the_Sites    : constant Sites_view    := new many_Sites   (1 ..  site_Count);
      the_Normals  : constant Normals_view  := new many_Normals (1 .. normal_Count);
      the_Coords   :          Coords_view;
      the_Weights  :          bone_Weights_array_view;

      the_Faces    :          IO.Faces_view := new IO.Faces (1 .. 50_000);
      face_Count   :          long_Index_t  := 0;

   begin
      if coord_Count > 0
      then
         the_Coords := new many_Coordinates_2D (1 .. coord_Count);
      end if;

      for i in 1 .. Integer (site_Count)
      loop
         the_Sites (long_Index_t (i)) := (collada_Positions (3 * (i - 1) + 1),
                                          collada_Positions (3 * (i - 1) + 2),
                                          collada_Positions (3 * (i - 1) + 3));
      end loop;

      for i in 1 .. Integer (normal_Count)
      loop
         the_Normals (long_Index_t (i)) := (collada_Normals (3 * (i - 1) + 1),
                                            collada_Normals (3 * (i - 1) + 2),
                                            collada_Normals (3 * (i - 1) + 3));
      end loop;

      if collada_Coords /= null
      then
         for i in 1 .. Integer (coord_Count)
         loop
            the_Coords (long_Index_t (i)) := (collada_Coords (2 * (i - 1) + 1),
                                              collada_Coords (2 * (i - 1) + 2));
         end loop;
      end if;

      --  Skinning
      --
      if         the_Document.Libraries.Controllers.Contents       /= null
        and then the_Document.Libraries.Controllers.Contents'Length > 0
      then
         declare
            use std_Collada.Library.controllers;

            the_Controller  : constant controllers.Controller := the_Document.Libraries.Controllers.Contents (which_Geometry);
            the_Skin        : constant controllers.Skin       := the_Controller.Skin;

            collada_Weights : constant access std_Collada.Float_array := Weights_of (the_Skin);

            V               : std_Collada.Int_array renames the_Skin.vertex_Weights.V      .all;
            v_Count         : std_Collada.Int_array renames the_Skin.vertex_Weights.v_Count.all;
            v_Cursor        :          math.Index        := 0;
            inputs_Count    : constant math.Index        := the_Skin.vertex_Weights.Inputs'Length;

         begin
            the_Weights := new bone_Weights_array (1 .. long_Index_t (the_Skin.vertex_Weights.Count));

            for each_Vertex in v_Count'Range
            loop
               declare
                  the_Count     : constant long_Index_t      :=      long_Index_t (v_Count      (each_Vertex));
                  these_Weights :          bone_Weights_view renames the_Weights  (long_Index_t (each_Vertex));
                  Base          :          Math.Index;
               begin
                  these_Weights := new bone_Weights (1 .. the_Count);

                  for i in 1 .. the_Count
                  loop
                     v_Cursor :=  v_Cursor + 1;
                     Base     := (v_Cursor - 1) * inputs_Count + 1;

                     these_Weights (i).Bone   := bone_Id (  1
                                                          + V (Base + joint_Offset_of (the_Skin.vertex_weights)));
                     these_Weights (i).Weight := Real (collada_Weights (  1
                                                                        + math.Index (V (  Base
                                                                                         + weight_Offset_of (the_Skin.vertex_Weights)))));
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
               P            : std_Collada.Int_array renames the_Primitive.P_List (1).all;
               inputs_Count : constant Natural      :=      the_Primitive.Inputs'Length;

               p_First      : math.Index := 1;
               p_Last       : math.Index;

               vertex_Count : Natural;

            begin
               for Each in the_Primitive.vCount'Range
               loop
                  vertex_Count := the_Primitive.vCount (Each);
                  p_Last       :=   p_First
                                  + math.Index (inputs_Count * vertex_Count)
                                  - 1;
                  declare
                     the_Vertices : Vertices (1 .. long_Index_t (vertex_Count));

                     P_Indices    : constant std_Collada.Int_array (1 .. p_Last - p_First + 1) := P (p_First .. p_Last);
                     the_Face     : IO.Face;
                     Base         : math.Index;
                  begin
                     for vertex_Id in the_Vertices'Range
                     loop
                        Base :=   math.Index (vertex_Id - 1)
                                * math.Index (inputs_Count)
                                + 1;

                        the_Vertices (vertex_Id).site_Id   :=   1
                                                              + long_Index_t (P_Indices (  Base
                                                                                         + vertex_Offset_of (the_Primitive)));
                        the_Vertices (vertex_Id).normal_Id :=   1
                                                              + long_Index_t (P_Indices (  Base
                                                                                         + normal_Offset_of (the_Primitive)));
                        if collada_Coords /= null
                        then
                           the_Vertices (vertex_Id).coord_Id :=   1
                                                                + long_Index_t (  P_Indices (Base
                                                                                + coord_Offset_of (the_Primitive)));
                        else
                           the_Vertices (vertex_Id).coord_Id := null_Id;
                        end if;

                        the_Vertices (vertex_Id).weights_Id := the_Vertices (vertex_Id).site_Id;
                     end loop;

                     case vertex_Count
                     is
                        when 3      => the_Face := (Triangle, the_Vertices);
                        when 4      => the_Face := (Quad,     the_Vertices);
                        when others => put_Line ("parse_polyList ~ unhandled vertex count:" & vertex_Count'Image);
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
                     P_Indices    : std_Collada.Int_array renames the_Primitive.P_List (Each).all;

                     vertex_Count : constant Natural := P_Indices'Length / inputs_Count;
                     the_Vertices : Vertices (1 .. long_Index_t (vertex_Count));

                     the_Face     : IO.Face;
                     Base         : math.Index;
                  begin
                     for vertex_Id in the_Vertices'Range
                     loop
                        Base := math.Index (  (Integer (vertex_Id) - 1)
                                            * inputs_Count
                                            + 1);

                        the_Vertices (vertex_Id).site_Id   :=   1
                                                              + long_Index_t (P_Indices (  Base
                                                                                         + vertex_Offset_of (the_Primitive)));
                        the_Vertices (vertex_Id).normal_Id :=   1
                                                              + long_Index_t (P_Indices (  Base
                                                                                         + normal_Offset_of (the_Primitive)));
                        if collada_Coords /= null
                        then
                           the_Vertices (vertex_Id).coord_Id :=   1
                                                                + long_Index_t (P_Indices (  Base
                                                                                           + coord_Offset_of (the_Primitive)));
                        else
                           the_Vertices (vertex_Id).coord_Id := null_Id;
                        end if;

                        the_Vertices (vertex_Id).weights_Id := the_Vertices (vertex_Id).site_Id;
                     end loop;

                     case vertex_Count
                     is
                        when 3      => the_Face := (Triangle, the_Vertices);
                        when 4      => the_Face := (Quad,     the_Vertices);
                        when others => put_Line ("parse_Polygons ~ unhandled vertex count:" & vertex_Count'Image);
                     end case;

                     face_Count             := face_Count + 1;
                     the_Faces (face_Count) := the_Face;
                  end;

               end loop;
            end parse_Polygons;


         when Triangles =>
            parse_Triangles:
            declare
               inputs_Count : constant Natural      :=      the_Primitive.Inputs'Length;
               P_Indices    : std_Collada.Int_array renames the_Primitive.P_List (1).all;
               Base         : math.Index            :=      1;

            begin
               for each_Tri in 1 .. the_Primitive.Count
               loop
                  declare
                     vertex_Count : constant := 3;
                     the_Vertices : Vertices (1 .. vertex_Count);

                     the_Face     : IO.Face;
                  begin
                     for vertex_Id in the_Vertices'Range
                     loop
                        the_Vertices (vertex_Id).site_Id   :=   1
                                                              + long_Index_t (P_Indices (  Base
                                                                                         + vertex_Offset_of (the_Primitive)));
                        the_Vertices (vertex_Id).normal_Id :=   1
                                                              + long_Index_t (P_Indices (  Base
                                                                                         + normal_Offset_of (the_Primitive)));
                        if collada_Coords /= null
                        then
                           the_Vertices (vertex_Id).coord_Id :=   1
                                                                + long_Index_t (P_Indices (  Base
                                                                                           + coord_Offset_of (the_Primitive)));
                        else
                           the_Vertices (vertex_Id).coord_Id := null_Id;
                        end if;

                        the_Vertices (vertex_Id).weights_Id := the_Vertices (vertex_Id).site_Id;

                        Base := Base + inputs_Count;
                     end loop;

                     the_Face               := (Triangle, the_Vertices);
                     face_Count             := face_Count + 1;
                     the_Faces (face_Count) := the_Face;
                  end;

               end loop;
            end parse_Triangles;


         when others =>
            put_Line ("Warning: ignoring unimplemented primitive kind: " & the_Primitive.Kind'Image);
      end case;


      declare
         used_Faces : constant IO.Faces_view := new IO.Faces' (the_Faces (1 .. face_Count));
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
