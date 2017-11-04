with
     openGL.Geometry.textured,
     openGL.Texture,
     openGL.IO,
     openGL.Primitive.indexed;


package body openGL.Model.sphere.textured
is

   use openGL;
   use type math.Real;



   package body Forge
   is
      function new_Sphere (Radius       : in math.Real;
                           Image        : in asset_Name := null_Asset;
                           is_Skysphere : in Boolean    := False) return View
      is
         Self : constant View := new Item;
      begin
         Self.Image        := Image;
         Self.is_Skysphere := is_Skysphere;

         Self.define (scale => (Radius * 2.0,
                                Radius * 2.0,
                                Radius * 2.0));
         return Self;
      end new_Sphere;
   end Forge;



   type Geometry_view is access all openGL.Geometry.textured.item'Class;

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

      use openGL.Geometry,
          openGL.Geometry.textured;

      use type Real;

      Degrees_180       : constant      := Pi;
      Degrees_360       : constant      := Pi * 2.0;

      latitude_Count    : constant      :=  2 * 13;
      longitude_Count   : constant      :=  2 * 26;

      Num_lat_strips    : constant      := latitude_Count - 1;

      latitude_Spacing  : constant Real := Degrees_180 / Real (latitude_Count - 1);
      longitude_Spacing : constant Real := Degrees_360 / Real (longitude_Count);

      vertex_Count      : constant openGL.Index_t      :=   1 + 1                                           -- North and south pole.
                                                          + (longitude_Count + 1) * (latitude_Count - 2);   -- Each latitude ring.

      indices_Count     : constant openGL.long_Index_t := Num_lat_strips * (longitude_Count + 1) * 2;

      the_Vertices      : aliased  openGL.geometry.textured.Vertex_array := (1 .. vertex_Count  => <>);
      the_Indices       : aliased  Indices                               := (1 .. indices_Count => <>);

      the_Geometry      : constant Geometry_view := Geometry_view (openGL.Geometry.textured.new_Geometry); -- (texture_is_Alpha => False));

   begin
      set_Sites :
      declare
         use      linear_Algebra,
                  linear_Algebra_3d;
         use type math.Real;

         north_Pole : constant Site    := (0.0,  0.5,  0.0);
         south_Pole : constant Site    := (0.0, -0.5,  0.0);

         the_Site   :          Site    := north_Pole;
         vert_Id    :          Index_t := 1;            -- Start at '1' (not '0')to account for north pole.
         a, b       :          Real    := 0.0;          -- Angular 'cursors' used to track lat/long for texture coords.

         latitude_line_First : Site;

      begin
         the_Vertices (the_Vertices'First).Site   := north_Pole;
         the_Vertices (the_Vertices'First).Coords := (s => 0.5, t => 1.0);

         the_Vertices (the_Vertices'Last).Site    := south_Pole;
         the_Vertices (the_Vertices'Last).Coords  := (s => 0.5, t => 0.0);

         for lat_Id in 2 .. latitude_Count - 1
         loop
            a := 0.0;
            b := b + latitude_Spacing;

            the_Site            := the_Site * z_rotation_from (latitude_Spacing);
            latitude_line_First := the_Site;                 -- Store initial latitude lines 1st point.

            vert_Id             := vert_Id + 1;

            the_Vertices (vert_Id).Site   := the_Site;
            the_Vertices (vert_Id).Coords := (s => a / Degrees_360,
                                              t => 1.0 - b / Degrees_180);

            for long_Id in 1 .. longitude_Count
            loop
               a := a + longitude_Spacing;

               if long_Id /= longitude_Count
               then   the_Site := the_Site * y_rotation_from (-longitude_Spacing);
               else   the_Site := latitude_line_First;       -- Restore the_Vertex back to initial latitude lines 1st point.
               end if;

               vert_Id             := vert_Id + 1;

               the_Vertices (vert_Id).Site   := the_Site;
               the_Vertices (vert_Id).Coords := (s => a / Degrees_360,
                                                 t => 1.0 - b / Degrees_180);
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

            if lat_Strip = 1
            then
               Upper := 2;
            end if;

            Lower := Upper + longitude_Count + 1;
         end loop;
      end set_Indices;


      declare
         Pad : Index_t;
      begin
         for i in the_Indices'Range
         loop
            if i mod 2 = 1
            then
               Pad               := the_Indices (i);
               the_Indices (i)   := the_Indices (i+1);
               the_Indices (i+1) := Pad;
            end if;
         end loop;
      end;


      if Self.Image /= null_Asset
      then
         set_Texture :
         declare
            use openGL.Texture;
            the_Image   : constant openGL.Image          := openGL.io.to_Image   (to_String (self.Image));
            the_Texture : constant openGL.Texture.object :=           to_Texture (the_Image);
         begin
            the_Geometry.Texture_is (the_Texture);
         end set_Texture;
      end if;

      declare
         the_Bounds : openGL.Bounds := (ball => <>,
                                        box  => (lower => (-self.Scale (1) / 2.0,
                                                           -self.Scale (2) / 2.0,
                                                           -self.Scale (3) / 2.0),
                                                 upper => ( self.Scale (1) / 2.0,
                                                            self.Scale (2) / 2.0,
                                                            self.Scale (3) / 2.0)));
      begin
         set_Ball_from_Box       (the_Bounds);
         the_Geometry.Bounds_are (the_Bounds);
      end;

      for i in the_Vertices'Range
      loop
         the_Vertices (i).Site := Scaled (the_Vertices (i).Site, by => Self.Scale);
      end loop;


      the_Geometry.is_Transparent (False);   -- tbd: Base this on vertex data.
      Vertices_are (the_Geometry.all,  the_Vertices);

      declare
         the_Primitive : constant openGL.Primitive.indexed.view
           := openGL.Primitive.indexed.new_Primitive (primitive.triangle_Strip,
                                                      the_Indices);
      begin
         the_Geometry.add (openGL.Primitive.view (the_Primitive));
      end;

      return (1 => openGL.Geometry.view (the_Geometry));
   end to_GL_Geometries;


end openGL.Model.sphere.textured;
