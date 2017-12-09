with
     openGL.Geometry.lit_colored,
     openGL.Primitive.indexed,
     ada.Unchecked_Deallocation;


package body openGL.Model.sphere.lit_colored
is

   use openGL;
   use type math.Real;


   package body Forge
   is
      function new_Sphere (Radius : in math.Real;
                           Color  : in openGL.lucid_Color) return View
      is
         Self : constant View := new Item;
      begin
         Self.Color  := (Color);
         Self.define (scale => (Radius * 2.0,
                                Radius * 2.0,
                                Radius * 2.0));
         return Self;
      end new_Sphere;
   end Forge;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.Unchecked_Deallocation (Item'Class,
                                                              View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



   type Geometry_view is access all openGL.Geometry.lit_colored.item'Class;

   --  nb: - an extra vertex is required at the end of each latitude ring
   --      - this last vertex has the same site as the rings initial vertex.
   --      - the  last    vertex has 's' texture coord of 1.0, whereas
   --        the  initial vertex has 's' texture coord of 0.0
   --
   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use      openGL.Geometry,
               openGL.Geometry.lit_colored;
      use type Real;

      Degrees_360       : constant      := Pi * 2.0;
      Degrees_180       : constant      := Pi;

      latitude_Count    : constant      := 2 * 13;
      longitude_Count   : constant      := 2 * 26;

      Num_lat_strips    : constant      := latitude_Count - 1;

      latitude_Spacing  : constant Real := Degrees_180 / Real (latitude_Count - 1);
      longitude_Spacing : constant Real := Degrees_360 / Real (longitude_Count);

      vertex_Count      : constant openGL.Index_t      :=   1 + 1                                           -- North and south pole.
                                                          + (longitude_Count + 1) * (latitude_Count - 2);   -- Each latitude ring.

      indices_Count     : constant openGL.long_Index_t := Num_lat_strips * (longitude_Count + 1) * 2;

      the_Sites         : aliased  openGL.Sites        := (1 .. vertex_Count => <>);
      the_Geometry      : constant Geometry_view       := Geometry_view (openGL.Geometry.lit_colored.new_Geometry);

      the_Vertices      : aliased  openGL.geometry.lit_colored.Vertex_array := (1 .. vertex_Count  => <>);
      the_Indices       : aliased  Indices                                  := (1 .. indices_Count => <>);

   begin
      set_Sites :
      declare
         use      linear_Algebra,
                  linear_Algebra_3d;
         use type math.Real;

         north_Pole : constant Site := (0.0,  0.5,  0.0);
         south_Pole : constant Site := (0.0, -0.5,  0.0);

         the_Site   : Site    := north_Pole;
         vert_Id    : Index_t := 1;              -- Start at '1' (not '0')to account for northpole.

         latitude_line_First : Site;

         a, b       : Real := 0.0;               -- Angular 'cursors' used to track lat/long for texture coords.

      begin
         the_Sites (the_Vertices'First)           := north_Pole;

         the_Vertices (the_Vertices'First).Site   := north_Pole;
         the_Vertices (the_Vertices'First).Normal := Normalised (north_Pole);
         the_Vertices (the_Vertices'First).Color  := Self.Color;

         the_Sites (the_Vertices'Last)            := south_Pole;

         the_Vertices (the_Vertices'Last).Site    := south_Pole;
         the_Vertices (the_Vertices'Last).Normal  := Normalised (south_Pole);
         the_Vertices (the_Vertices'Last).Color   := Self.Color;


         for lat_Id in 2 .. latitude_Count - 1
         loop
            a := 0.0;
            b := b + latitude_Spacing;

            the_Site            := the_Site * z_rotation_from (latitude_Spacing);
            latitude_line_First := the_Site;                   -- Store initial latitude lines 1st point.

            vert_Id             := vert_Id + 1;
            the_Sites (vert_Id) := the_Site;                   -- Add 1st point on a line of latitude.

            the_Vertices (vert_Id).Site   := the_Site;
            the_Vertices (vert_Id).Normal := Normalised (the_Site);
            the_Vertices (vert_Id).Color  := Self.Color;

            for long_Id in 1 .. longitude_Count
            loop
               a := a + longitude_Spacing;

               if long_Id /= longitude_Count
               then   the_Site := the_Site * y_rotation_from (-longitude_Spacing);
               else   the_Site := latitude_line_First;         -- Restore the_Vertex back to initial latitude lines 1st point.
               end if;

               vert_Id             := vert_Id + 1;
               the_Sites (vert_Id) := the_Site;                -- Add each succesive point on a line of latitude.

               the_Vertices (vert_Id).Site   := the_Site;
               the_Vertices (vert_Id).Normal := Normalised (the_Site);
               the_Vertices (vert_Id).Color  := Self.Color;
            end loop;

         end loop;
      end set_Sites;


      set_Indices :
      declare
         strip_Id : long_Index_t := 0;

         Upper    : Index_t;
         Lower    : Index_t;

         procedure set
         is
         begin
            strip_Id := strip_Id + 1;   the_Indices (strip_Id) := Upper;
            strip_Id := strip_Id + 1;   the_Indices (strip_Id) := Lower;
         end set;

      begin
         upper := 1;
         lower := 2;

         for lat_Strip in 1 .. num_lat_Strips
         loop
            for Each in 1 .. longitude_Count + 1
            loop
               set;

               if lat_Strip /= 1              then   Upper := Upper + 1;   end if;
               if lat_Strip /= num_lat_Strips then   Lower := Lower + 1;   end if;
            end loop;


            if lat_Strip = 1 then
               Upper := 2;
            end if;

            Lower := Upper + longitude_Count + 1;
         end loop;
      end set_Indices;


      the_Geometry.is_Transparent (False);
      Vertices_are (the_Geometry.all, the_Vertices);

      declare
         the_Primitive : constant openGL.Primitive.indexed.view
           := openGL.Primitive.indexed.new_Primitive (primitive.triangle_Strip,  the_Indices);
      begin
         the_Geometry.add (openGL.Primitive.view (the_Primitive));
      end;


      return (1 => openGL.Geometry.view (the_Geometry));
   end to_GL_Geometries;


end openGL.Model.sphere.lit_colored;
