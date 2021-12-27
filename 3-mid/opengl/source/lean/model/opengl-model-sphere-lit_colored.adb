with
     openGL.Geometry .lit_colored,
     openGL.Primitive.indexed;


package body openGL.Model.sphere.lit_colored
is
   ---------
   --- Forge
   --

   function new_Sphere (Radius     : in Real;
                        lat_Count  : in Positive := default_latitude_Count;
                        long_Count : in Positive := default_longitude_Count;
                        Color      : in lucid_Color) return View
   is
      Self : constant View := new Item;
   begin
      Self.Color      := Color;
      Self.lat_Count  := lat_Count;
      Self.long_Count := long_Count;

      Self.define (Radius);

      return Self;
   end new_Sphere;


   --------------
   --- Attributes
   --

   --  NB: - An extra vertex is required at the end of each latitude ring.
   --      - This last vertex has the same site as the rings initial vertex.
   --      - The  last    vertex has 's' texture coord of 1.0, whereas
   --        the  initial vertex has 's' texture coord of 0.0.
   --
   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use Geometry,
          Geometry.lit_colored;

      lat_Count      : Positive renames Self.lat_Count;
      long_Count     : Positive renames Self.long_Count;

      Num_lat_strips : constant Positive := lat_Count - 1;

      lat_Spacing    : constant Real := Degrees_180 / Real (lat_Count - 1);
      long_Spacing   : constant Real := Degrees_360 / Real (long_Count);

      indices_Count  : constant long_Index_t  := long_Index_t (Num_lat_strips * (long_Count + 1) * 2);
      vertex_Count   : constant      Index_t  :=   1 + 1                                                 -- North and south pole.
                                                 + Index_t ((long_Count + 1) * (lat_Count - 2));         -- Each latitude ring.

      the_Vertices   : aliased  Geometry.lit_colored.Vertex_array := (1 ..  vertex_Count => <>);
      the_Sites      : aliased  Sites                             := (1 ..  vertex_Count => <>);
      the_Indices    : aliased  Indices                           := (1 .. indices_Count => <>);

      Color          : constant rgba_Color                := to_rgba_Color (Self.Color);
      the_Geometry   : constant Geometry.lit_colored.view := Geometry.lit_colored.new_Geometry;

   begin
      set_Sites:
      declare
         use linear_Algebra,
             linear_Algebra_3D;

         north_Pole : constant Site := (0.0,  0.5,  0.0);
         south_Pole : constant Site := (0.0, -0.5,  0.0);

         the_Site   : Site    := north_Pole;
         vert_Id    : Index_t := 1;        -- Start at '1' (not '0') to account for the northpole.

         latitude_line_First : Site;

         a, b : Real := 0.0;               -- Angular 'cursors' used to track lat/long for texture coords.

      begin
         the_Sites (the_Vertices'First)           := north_Pole;

         the_Vertices (the_Vertices'First).Site   := north_Pole;
         the_Vertices (the_Vertices'First).Normal := Normalised (north_Pole);
         the_Vertices (the_Vertices'First).Color  := Color;

         the_Sites (the_Vertices'Last)            := south_Pole;

         the_Vertices (the_Vertices'Last).Site    := south_Pole;
         the_Vertices (the_Vertices'Last).Normal  := Normalised (south_Pole);
         the_Vertices (the_Vertices'Last).Color   := Color;


         for lat_Id in 2 .. lat_Count - 1
         loop
            a := 0.0;
            b := b + lat_Spacing;

            the_Site            := the_Site * z_Rotation_from (lat_Spacing);
            latitude_line_First := the_Site;                   -- Store initial latitude lines 1st point.

            vert_Id             := vert_Id + 1;
            the_Sites (vert_Id) := the_Site;                   -- Add 1st point on a line of latitude.

            the_Vertices (vert_Id).Site   := the_Site;
            the_Vertices (vert_Id).Normal := Normalised (the_Site);
            the_Vertices (vert_Id).Color  := Color;

            for long_Id in 1 .. long_Count
            loop
               a := a + long_Spacing;

               if long_Id /= long_Count
               then   the_Site := the_Site * y_Rotation_from (-long_Spacing);
               else   the_Site := latitude_line_First;         -- Restore the_Vertex back to initial latitude lines 1st point.
               end if;

               vert_Id             := vert_Id + 1;
               the_Sites (vert_Id) := the_Site;                -- Add each succesive point on a line of latitude.

               the_Vertices (vert_Id).Site   := the_Site;
               the_Vertices (vert_Id).Normal := Normalised (the_Site);
               the_Vertices (vert_Id).Color  := Color;
            end loop;

         end loop;
      end set_Sites;


      for i in the_Vertices'Range
      loop
         the_Vertices (i).Site := the_Vertices (i).Site * Self.Radius * 2.0;
      end loop;


      set_Indices:
      declare
         strip_Id : long_Index_t := 0;

         Upper    : Index_t;
         Lower    : Index_t;

      begin
         upper := 1;
         lower := 2;

         for lat_Strip in 1 .. num_lat_Strips
         loop
            for Each in 1 .. long_Count + 1
            loop
               strip_Id := strip_Id + 1;   the_Indices (strip_Id) := Upper;
               strip_Id := strip_Id + 1;   the_Indices (strip_Id) := Lower;

               if lat_Strip /= 1              then   Upper := Upper + 1;   end if;
               if lat_Strip /= num_lat_Strips then   Lower := Lower + 1;   end if;
            end loop;


            if lat_Strip = 1 then
               Upper := 2;
            end if;

            Lower := Upper + Index_t (long_Count) + 1;
         end loop;
      end set_Indices;


      the_Geometry.is_Transparent (False);
      the_Geometry.Vertices_are (the_Vertices);

      declare
         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (Primitive.triangle_Strip, the_Indices);
      begin
         the_Geometry.add (Primitive.view (the_Primitive));
      end;


      return (1 => Geometry.view (the_Geometry));
   end to_GL_Geometries;


end openGL.Model.sphere.lit_colored;
