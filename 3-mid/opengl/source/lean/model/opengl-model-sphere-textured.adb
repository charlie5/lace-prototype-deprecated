with
     openGL.Geometry.textured,
     openGL.Texture,
     openGL.IO,
     openGL.Primitive.indexed;


package body openGL.Model.sphere.textured
is
   ---------
   --- Forge
   --

   function new_Sphere (Radius       : in Real;
                        lat_Count    : in Positive   := 26;
                        long_Count   : in Positive   := 52;
                        Image        : in asset_Name := null_Asset;
                        is_Skysphere : in Boolean    := False) return View
   is
      Self : constant View := new Item;
   begin
      Self.lat_Count    := lat_Count;
      Self.long_Count   := long_Count;
      Self.Image        := Image;
      Self.is_Skysphere := is_Skysphere;

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
          Geometry.textured;

      lat_Count      : Positive renames Self.lat_Count;
      long_Count     : Positive renames Self.long_Count;

      Num_lat_strips : constant Positive := lat_Count - 1;

      lat_Spacing    : constant Real := Degrees_180 / Real (lat_Count - 1);
      long_Spacing   : constant Real := Degrees_360 / Real (long_Count);

      vertex_Count   : constant Index_t      :=   1 + 1                                           -- North and south pole.
                                                + Index_t ((long_Count + 1) * (lat_Count - 2));   -- Each latitude ring.

      indices_Count  : constant long_Index_t := long_Index_t (Num_lat_strips * (long_Count + 1) * 2);

      the_Vertices   : aliased  Geometry.textured.Vertex_array := (1 .. vertex_Count  => <>);
      the_Indices    : aliased  Indices                        := (1 .. indices_Count => <>);

      the_Geometry   : constant Geometry.textured.view := Geometry.textured.new_Geometry;

   begin
      set_Sites:
      declare
         use linear_Algebra_3d;

         north_Pole : constant Site := (0.0,  0.5,  0.0);
         south_Pole : constant Site := (0.0, -0.5,  0.0);

         the_Site : Site    := north_Pole;
         vert_Id  : Index_t := 1;            -- Start at '1' (not '0')to account for north pole.
         a, b     : Real    := 0.0;          -- Angular 'cursors' used to track lat/long for texture coords.

         latitude_line_First : Site;

      begin
         the_Vertices (the_Vertices'First).Site   := north_Pole;
         the_Vertices (the_Vertices'First).Coords := (S => 0.5, T => 1.0);

         the_Vertices (the_Vertices'Last).Site    := south_Pole;
         the_Vertices (the_Vertices'Last).Coords  := (S => 0.5, T => 0.0);

         for lat_Id in 2 .. lat_Count - 1
         loop
            a := 0.0;
            b := b + lat_Spacing;

            the_Site            := the_Site * z_rotation_from (lat_Spacing);
            latitude_line_First := the_Site;                 -- Store initial latitude lines 1st point.

            vert_Id             := vert_Id + 1;

            the_Vertices (vert_Id).Site   := the_Site;
            the_Vertices (vert_Id).Coords := (S => a / Degrees_360,
                                              T => 1.0 - b / Degrees_180);

            for long_Id in 1 .. long_Count
            loop
               a := a + long_Spacing;

               if long_Id /= long_Count
               then   the_Site := the_Site * y_rotation_from (-long_Spacing);
               else   the_Site := latitude_line_First;       -- Restore the_Vertex back to initial latitude lines 1st point.
               end if;

               vert_Id := vert_Id + 1;

               the_Vertices (vert_Id).Site   := the_Site;
               the_Vertices (vert_Id).Coords := (S =>       a / Degrees_360,
                                                 T => 1.0 - b / Degrees_180);
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

         Upper : Index_t;
         Lower : Index_t;

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

            if lat_Strip = 1
            then
               Upper := 2;
            end if;

            Lower := Upper + Index_t (long_Count) + 1;
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
         set_Texture:
         declare
            use Texture;
            the_Image   : constant Image          := IO   .to_Image   (Self.Image);
            the_Texture : constant Texture.object := Forge.to_Texture ( the_Image);
         begin
            the_Geometry.Texture_is (the_Texture);
         end set_Texture;
      end if;

      the_Geometry.is_Transparent (False);   -- TODO: Base this on vertex data.
      the_Geometry.Vertices_are   (the_Vertices);

      declare
         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (Primitive.triangle_Strip,
                                               the_Indices);
      begin
         the_Geometry.add (Primitive.view (the_Primitive));
      end;

      return (1 => Geometry.view (the_Geometry));
   end to_GL_Geometries;


end openGL.Model.sphere.textured;
