with
     openGL.Primitive.short_indexed,
     openGL.Primitive.      indexed,
     openGL.Primitive.long_indexed,

     openGL.Geometry.lit_textured,
     openGL.Geometry.lit_colored_textured_skinned,

     openGL.Texture,
     openGL.Palette,

     openGL.IO.wavefront,
     openGL.IO.collada,
     openGL.IO.lat_long_Radius,

     ada.Strings.fixed,
     ada.Containers.hashed_Maps,

     ada.unchecked_Deallocation;


package body openGL.Model.any
is

   type lit_textured_skinned_Geometry_view is access all openGL.Geometry.lit_colored_textured_skinned.item'Class;


   ---------
   --- Forge
   --

   function to_Model (Model            : in asset_Name;
                      Texture          : in asset_Name;
                      Texture_is_lucid : in Boolean) return openGL.Model.any.item
   is
   begin
      return Self : openGL.Model.any.item := (openGL.Model.item with
                                              Model,
                                              Texture,
                                              Texture_is_lucid,
                                              Geometry => null)
      do
         Self.Bounds.Ball := 1.0;
      end return;
   end to_Model;


   function new_Model (Model            : in asset_Name;
                       Texture          : in asset_Name;
                       Texture_is_lucid : in Boolean) return openGL.Model.any.view
   is
   begin
      return new openGL.Model.any.item' (to_Model (Model, Texture, Texture_is_lucid));
   end new_Model;


   --------------
   --- Attributes
   --

   function model_Name (Self : in Item) return asset_Name
   is
   begin
      return Self.Model;
   end model_Name;


   use openGL.IO;

   function Hash (Self : in io.Vertex) return ada.Containers.Hash_type
   is
   begin
      return ada.Containers.Hash_type (Self.site_Id + 3 * Self.coord_Id + 5 * Self.normal_Id + 7 * Self.weights_Id);
   end Hash;

   package io_vertex_Maps_of_gl_vertex_id is new ada.containers.Hashed_Maps (io.Vertex,
                                                                             long_Index_t,
                                                                             Hash,
                                                                             "=");
   subtype io_vertex_Map_of_gl_vertex_id is io_vertex_Maps_of_gl_vertex_id.Map;

   type any_Vertex is
      record
         Site   : Vector_3;
         Normal : Vector_3;
         Coords : Coordinate_2D;
         Shine  : Real;
         Bones  : bone_Weights (1 .. 4);
      end record;

   type any_Vertex_array      is array (long_Index_t range <>) of aliased any_Vertex;
   type any_Vertex_array_view is access all any_Vertex_array;

   procedure deallocate is new ada.unchecked_Deallocation (any_Vertex_array,
                                                           any_Vertex_array_view);


   function to_lit_textured_Vertices (From : in any_Vertex_array) return Geometry.lit_textured.Vertex_large_array
   is
      Result : Geometry.lit_textured.Vertex_large_array (From'Range);
   begin
      for i in From'Range
      loop
         Result (i) := (Site   => From (i).Site,
                        Normal => From (i).Normal,
                        Coords => From (i).Coords,
                        Shine  => From (i).Shine);
      end loop;

      return Result;
   end to_lit_textured_Vertices;



   function to_lit_textured_skinned_Vertices (From : in any_Vertex_array) return Geometry.lit_colored_textured_skinned.Vertex_array
   is
      use Palette;
      Result : Geometry.lit_colored_textured_skinned.Vertex_array (From'Range);
   begin
      for i in From'Range
      loop
         Result (i) := (Site         => From (i).Site,
                        Normal       => From (i).Normal,
                        Coords       => From (i).Coords,
                        Shine        => From (i).Shine,
                        Color        => (+White, opaque_Value),
                        bone_Ids     => (1 => Real (From (i).Bones (1).Bone),
                                         2 => Real (From (i).Bones (2).Bone),
                                         3 => Real (From (i).Bones (3).Bone),
                                         4 => Real (From (i).Bones (4).Bone)),
                        bone_Weights => (1 =>       From (i).Bones (1).Weight,
                                         2 =>       From (i).Bones (2).Weight,
                                         3 =>       From (i).Bones (3).Weight,
                                         4 =>       From (i).Bones (4).Weight));
      end loop;

      return Result;
   end to_lit_textured_skinned_Vertices;



   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);
   begin
      Self.build_GL_Geometries;
      return (1 => Self.Geometry);
   end to_GL_Geometries;



   procedure build_GL_Geometries (Self : in out Item)
   is
      use Geometry;

      model_Name : constant String := to_String (Self.Model);

      function load_Model return io.Model
      is
         use ada.Strings.fixed;
      begin
         if    Tail (model_Name, 4) = ".obj" then   return wavefront      .to_Model (model_Name);
         elsif Tail (model_Name, 4) = ".dae" then   return collada        .to_Model (model_Name);
         elsif Tail (model_Name, 4) = ".tab" then   return lat_long_Radius.to_Model (model_Name);
         else                                       raise  unsupported_model_Format with "Model => '" & model_Name & "'";
         end if;
      end load_Model;

      the_Model     : openGL.io.Model := load_Model;
      the_Map       : io_vertex_Map_of_gl_vertex_id;

      the_Vertices  : any_Vertex_array_view := new any_Vertex_array' (1 .. 100_000 => <>);
      vertex_Count  : openGL.long_Index_t   := 0;

      tri_Count     : Index_t := 0;
      Normals_known : Boolean := False;

      --  TODO: Use one set of gl face vertices and 2 sets of indices (1 for tris and 1 for quads).

   begin
      Self.Bounds := null_Bounds;

      --  1st pass: - Set our openGL face vertices.
      --            - Build 'io vertex' to 'openGL face vertex_Id' map.
      --
      for f in the_Model.Faces'Range
      loop
         declare
            use io_vertex_Maps_of_gl_vertex_id;

            the_model_Face : io.Face renames the_Model.Faces (f);

         begin
            if    the_model_Face.Kind = Triangle
               or the_model_Face.Kind = Quad
            then
               declare
                  the_io_Vertices : constant io.Vertices := Vertices_of (the_model_Face);
                  Cursor          :          io_vertex_Maps_of_gl_vertex_id.Cursor;
               begin
                  case the_model_Face.Kind
                  is
                     when Triangle =>   tri_Count := tri_Count + 1;
                     when Quad     =>   tri_Count := tri_Count + 2;
                     when Polygon  =>   null;
                  end case;

                  for v in the_io_Vertices'Range
                  loop
                     Cursor := the_Map.find (the_io_Vertices (v));

                     if not has_Element (Cursor)
                     then   -- We do not know about this vertex yet, so add it.
                        vertex_Count := vertex_Count + 1;

                        declare
                           the_io_Vertex :  io.Vertex renames the_io_Vertices (v);
                           the_gl_Vertex : any_Vertex renames the_Vertices (vertex_Count);
                        begin
                           the_gl_Vertex.Site := the_Model.Sites (the_io_Vertex.site_Id);

                           Self.Bounds.Box  := Self.Bounds.Box or the_gl_Vertex.Site;
                           Self.Bounds.Ball := Real'Max (Self.Bounds.Ball,
                                                         abs (the_gl_Vertex.Site));

                           if the_io_Vertex.coord_Id /= null_Id
                           then   the_gl_Vertex.Coords := the_Model.Coords (the_io_Vertex.coord_Id);
                           else   the_gl_Vertex.Coords := (0.0, 0.0);
                           end if;

                           if the_io_Vertex.normal_Id /= null_Id
                           then   the_gl_Vertex.Normal := the_Model.Normals (the_io_Vertex.normal_Id);
                                  the_gl_Vertex.Shine  := default_Shine;
                                  normals_Known        := True;
                           else   the_gl_Vertex.Normal := (0.0, 0.0, 0.0);
                           end if;

                           if    the_Model.Weights        /= null
                             and the_io_Vertex.weights_Id /= null_Id
                           then
                              declare
                                 the_Weights : bone_Weights renames the_Model.Weights (the_io_Vertex.weights_Id).all;
                              begin
                                 if the_Weights'Length > 0
                                 then
                                    the_gl_Vertex.Bones (1) := the_Weights (1);
                                    --
                                    --  nb: Only using the first 4 bones atm.

                                    if the_Weights'Length >= 2
                                    then   the_gl_Vertex.Bones (2) := the_Weights (2);
                                    else   the_gl_Vertex.Bones (2) := (0, 0.0);
                                    end if;

                                    if the_Weights'Length >= 3
                                    then   the_gl_Vertex.Bones (3) := the_Weights (3);
                                    else   the_gl_Vertex.Bones (3) := (0, 0.0);
                                    end if;

                                    if the_Weights'Length >= 4
                                    then   the_gl_Vertex.Bones (4) := the_Weights (4);
                                    else   the_gl_Vertex.Bones (4) := (0, 0.0);
                                    end if;

                                 else
                                    the_gl_Vertex.Bones := (1 => (0, 0.0),
                                                            2 => (0, 0.0),
                                                            3 => (0, 0.0),
                                                            4 => (0, 0.0));
                                 end if;
                              end;

                           else
                              the_gl_Vertex.Bones := (1 => (0, 0.0),
                                                      2 => (0, 0.0),
                                                      3 => (0, 0.0),
                                                      4 => (0, 0.0));
                           end if;

                           the_Map.insert (the_io_Vertex, vertex_Count);   --  'vertex_Count' provides the index of the current vertex.
                        end;
                     end if;

                  end loop;
               end;

            end if;
         end;
      end loop;

      --  We now have our gl face vertices built and mapped to each model vertex.


      --  2nd pass: - Set the triangle faceted indices.
      --            - Set the quad     faceted indices.
      --
      declare
         tri_indices_Count :          long_Index_t := 0;
         tri_indices_Last  : constant long_Index_t := long_Index_t (tri_Count) * 3;
         tri_Indices       : aliased  long_Indices (1 .. tri_indices_Last);

         procedure add_to_Tri (the_Vertex : in io.Vertex)
         is
         begin
            tri_indices_Count               := tri_indices_Count + 1;
            tri_Indices (tri_indices_Count) := the_Map.Element (the_Vertex);
         end add_to_Tri;

      begin
         for f in the_Model.Faces'Range
         loop
            declare
               the_model_Face  :          io.Face     renames the_Model.Faces (f);
               the_io_Vertices : constant io.Vertices :=      Vertices_of (the_model_Face);
            begin
               case the_model_Face.Kind
               is
                  when Triangle =>
                     for v in the_io_Vertices'Range
                     loop
                        add_to_Tri (the_io_Vertices (v));
                     end loop;

                  when Quad     =>
                     add_to_Tri (the_io_Vertices (1));
                     add_to_Tri (the_io_Vertices (2));
                     add_to_Tri (the_io_Vertices (3));

                     add_to_Tri (the_io_Vertices (3));
                     add_to_Tri (the_io_Vertices (4));
                     add_to_Tri (the_io_Vertices (1));

                  when Polygon  =>
                     null;
               end case;
            end;
         end loop;

         pragma assert (tri_indices_Count = tri_indices_Last);


         --  Determine which geometry class is required and create the geometry.
         --
         if the_Model.Weights = null
         then
            declare
               use Geometry.lit_textured;

               my_Vertices : aliased  lit_textured.Vertex_large_array
                 := to_lit_textured_Vertices (the_Vertices (1 .. vertex_Count));

               my_Geometry : constant Geometry.lit_textured.view
                 := lit_textured.new_Geometry;
            begin
               if not normals_Known
               then
                  set_Normals:
                  declare
                     type Normals_view is access all Normals;

                     function get_Sites return Sites
                     is
                        Result : Sites := (1 .. my_Vertices'Length => <>);
                     begin
                        for i in Result'Range
                        loop
                           Result (i) := my_Vertices (long_Index_t (i)).Site;
                        end loop;

                        return Result;
                     end get_Sites;

                     the_Sites   : constant openGL.Sites := get_Sites;
                     the_Normals :          Normals_view := Geometry.Normals_of (Primitive.Triangles,
                                                                                 tri_Indices,
                                                                                 the_Sites);
                     procedure deallocate is new ada.unchecked_Deallocation (Normals, Normals_view);

                  begin
                     for i in my_Vertices'Range
                     loop
                        my_Vertices (i).Normal := the_Normals (Index_t (i));
                        my_Vertices (i).Shine  := default_Shine;
                     end loop;

                     deallocate (the_Normals);
                  end set_Normals;
               end if;

               my_Geometry.Vertices_are (now => my_Vertices);
               Self.Geometry := Geometry.view (my_Geometry);
            end;

         else   -- Is skinned.
            declare
               use Geometry.lit_colored_textured_skinned;

               my_Vertices : aliased constant lit_colored_textured_skinned.Vertex_array
                 := to_lit_textured_skinned_Vertices (the_Vertices (1 .. vertex_Count));

               my_Geometry : constant lit_textured_skinned_Geometry_view
                 := lit_colored_textured_skinned.new_Geometry;
            begin
               my_Geometry.Vertices_are (now => my_Vertices);
               Self.Geometry := Geometry.view (my_Geometry);
            end;
         end if;

         deallocate (the_Vertices);
         destroy    (the_Model);

         --  Set the geometry texture.
         --
         if Self.Texture /= null_Asset
         then
            if Self.has_lucid_Texture
            then
               declare
                  use Texture;
                  the_Image   : constant lucid_Image
                    := io.to_lucid_Image (Self.Texture);

                  the_Texture : constant Texture.object
                    := Forge.to_Texture (the_Image);

               begin
                  Self.Geometry.Texture_is (the_Texture);
               end;
            else
               declare
                  use Texture;
                  the_Image   : constant Image          := io.to_Image (Self.Texture);
                  the_Texture : constant Texture.object := Forge.to_Texture (the_Image);
               begin
                  Self.Geometry.Texture_is (the_Texture);
               end;
            end if;
         end if;

         --  Add any facia to the geometry.
         --
         if tri_Indices'Length > 0
         then
            if vertex_Count <= long_Index_t (short_Index_t'Last)
            then
               declare
                  the_Primitive : constant Primitive.short_indexed.view
                    := Primitive.short_indexed.new_Primitive (Primitive.Triangles,
                                                              tri_Indices);
               begin
                  Self.Geometry.add (Primitive.view (the_Primitive));
               end;

            elsif vertex_Count <= long_Index_t (Index_t'Last)
            then
               declare
                  the_Primitive : constant Primitive.indexed.view
                    := Primitive.indexed.new_Primitive (primitive.Triangles,
                                                        tri_Indices);
               begin
                  Self.Geometry.add (Primitive.view (the_Primitive));
               end;

            else
               if openGL.Profile /= Desk
               then
                  raise Model_too_complex with "Only the 'Desk' openGL profile allows models with more than 2**16 - 1 vertices.";
               end if;

               declare
                  the_Primitive : constant Primitive.long_indexed.view
                    := Primitive.long_indexed.new_Primitive (primitive.Triangles,
                                                             tri_Indices);
               begin
                  Self.Geometry.add (Primitive.view (the_Primitive));
               end;
            end if;
         end if;


         if Geometry_3d.Extent (Self.Bounds.Box, 3) = 0.0
         then
            Self.Bounds.Box.Lower (3) := Self.Bounds.Box.Lower (3) - 0.2;     -- TODO: This is dubious at best.
         end if;

         Self.Geometry.is_Transparent (now => False);
         Self.Geometry.Label_is (to_String (Self.Model) & "-" & to_String (Self.Texture));
      end;

   end build_GL_Geometries;


end openGL.Model.any;
