with
     openGL.Geometry.lit_colored_textured,
     openGL.Texture,
     openGL.IO,
     openGL.Primitive.indexed;


package body openGL.Model.capsule.lit_colored_textured
is
   ---------
   --- Forge
   --

   function new_Capsule (Radius : in Real;
                         Height : in Real;
                         Color  : in lucid_Color;
                         Image  : in asset_Name := null_Asset) return View
   is
      Self : constant View := new Item;
   begin
      Self.Radius := Radius;
      Self.Height := Height;

      Self.Color := +Color;
      Self.Image := Image;

      return Self;
   end new_Capsule;


   --------------
   --- Attributes
   --

   type Geometry_view is access all Geometry.lit_colored_textured.item'Class;


   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use Geometry,
          Geometry.lit_colored_textured,
          real_Functions;

      Length        : constant Real    := Self.Height;
      Radius        : constant Real    := Self.Radius;

      quality_Level : constant Index_t := 4;
      sides_Count   : constant Index_t := Index_t (quality_Level * 4);     -- Number of sides to the cylinder (divisible by 4):

      type Edge is   -- A 'shaft' edge.
         record
            Fore : Site;
            Aft  : Site;
         end record;

      type      Edges is array (Index_t range 1 .. sides_Count)   of Edge;
      type arch_Edges is array (Index_t range 1 .. quality_Level) of Sites (1 .. sides_Count);

      tmp,
      nx, ny, nz,
      start_nx,
      start_ny   :          Real;
      a          : constant Real := Pi * 2.0 / Real (sides_Count);
      ca         : constant Real := Cos (a);
      sa         : constant Real := Sin (a);
      L          : constant Real := Length * 0.5;

      the_Edges  : Edges;

      the_shaft_Geometry  : constant Geometry_view
        := Geometry_view (Geometry.lit_colored_textured.new_Geometry (texture_is_Alpha => False));

      cap_1_Geometry : Geometry_view;
      cap_2_Geometry : Geometry_view;

   begin
      --  Define capsule shaft,
      --
      declare
         vertex_Count  : constant      Index_t :=      Index_t (sides_Count * 2 + 2);   -- 2 triangles per side plus 2 since we cannot share the first and last edge.
         indices_Count : constant long_Index_t := long_Index_t (sides_Count * 2 * 3);   -- 2 triangles per side with 3 vertices per triangle.

         the_Vertices  : aliased Geometry.lit_colored_textured.Vertex_array := (1 .. vertex_Count  => <>);
         the_Indices   : aliased Indices                                    := (1 .. indices_Count => <>);

      begin
         ny := 1.0;
         nz := 0.0;              -- Normal vector = (0.0, ny, nz)

         -- Set vertices.
         --
         declare
            use linear_Algebra;

            S       :          Real    := 0.0;
            S_delta : constant Real    := 1.0 / Real (sides_Count);

            i       :          Index_t := 1;
         begin
            for Each in 1 .. Index_t (Edges'Length)
            loop
               the_Edges (Each).Fore (1) :=  ny * Radius;
               the_Edges (Each).Fore (2) :=  nz * Radius;
               the_Edges (Each).Fore (3) :=  L;

               the_Edges (Each).Aft  (1) :=  ny * Radius;
               the_Edges (Each).Aft  (2) :=  nz * Radius;
               the_Edges (Each).Aft  (3) := -L;

               --  Rotate ny, nz.
               --
               tmp := ca * ny  -  sa * nz;
               nz  := sa * ny  +  ca * nz;
               ny  := tmp;

               the_Vertices (i).Site   := the_Edges (Each).Fore;
               the_Vertices (i).Normal := Normalised ((the_Vertices (i).Site (1),
                                                       the_Vertices (i).Site (2),
                                                       0.0));
               the_Vertices (i).Color  := Self.Color;
               the_Vertices (i).Shine  := 0.5;
               the_Vertices (i).Coords := (s => S,
                                           t => 1.0);
               i := i + 1;

               the_Vertices (i).Site   := the_Edges (Each).Aft;
               the_Vertices (i).Normal := the_Vertices (i - 1).Normal;
               the_Vertices (i).Color  := Self.Color;
               the_Vertices (i).Shine  := 0.5;
               the_Vertices (i).Coords := (s => S,
                                           t => 0.0);
               i := i + 1;

               S := S + S_delta;
            end loop;

            the_Vertices (i).Site   := the_Edges (1).Fore;
            the_Vertices (i).Normal := Normalised ((the_Vertices (i).Site (1),
                                                    the_Vertices (i).Site (2),
                                                    0.0));
            the_Vertices (i).Color  := Self.Color;
            the_Vertices (i).Shine  := 0.5;
            the_Vertices (i).Coords := (s => S,
                                        t => 1.0);
            i := i + 1;

            the_Vertices (i).Site   := the_Edges (1).Aft;
            the_Vertices (i).Normal := the_Vertices (i - 1).Normal;
            the_Vertices (i).Color  := Self.Color;
            the_Vertices (i).Shine  := 0.5;
            the_Vertices (i).Coords := (s => S,
                                        t => 0.0);
         end;

         -- Set indices.
         --
         declare
            i     : long_Index_t := 1;
            Start :      Index_t := 1;
         begin
            for Each in 1 .. long_Index_t (sides_Count)
            loop
               the_Indices (i) := Start;       i := i + 1;
               the_Indices (i) := Start + 1;   i := i + 1;
               the_Indices (i) := Start + 2;   i := i + 1;

               the_Indices (i) := Start + 1;   i := i + 1;
               the_Indices (i) := Start + 3;   i := i + 1;
               the_Indices (i) := Start + 2;   i := i + 1;

               Start := Start + 2;
            end loop;
         end;

         if Self.Image /= null_Asset
         then
            set_Texture:
            declare
               use Texture;
               the_Image   : constant Image          := IO.to_Image      (Self.Image);
               the_Texture : constant Texture.object := Forge.to_Texture (the_Image);
            begin
               the_shaft_Geometry.Texture_is (the_Texture);
            end set_Texture;
         end if;

         Vertices_are (the_shaft_Geometry.all, the_Vertices);

         declare
            the_Primitive : constant Primitive.indexed.view
              := Primitive.indexed.new_Primitive (primitive.Triangles,
                                                  the_Indices);
         begin
            the_shaft_Geometry.add (Primitive.view (the_Primitive));
         end;
      end;


      declare
         function new_Cap (is_Fore : Boolean) return Geometry_view
         is
            use linear_Algebra;

            cap_Geometry  : constant Geometry_view
              := Geometry.lit_colored_textured.new_Geometry (texture_is_Alpha => False);

            hoop_Count    : constant      Index_t := quality_Level;
            vertex_Count  : constant      Index_t :=      Index_t (Edges'Length * hoop_Count + 1);             -- A vertex for each edge of each hoop, + 1 for the pole.
            indices_Count : constant long_Index_t := long_Index_t (  (hoop_count - 1) * sides_Count * 2 * 3    -- For each hoop, 2 triangles per side with 3 vertices per triangle
                                                                   + sides_Count * 3);                         -- plus the extra indices for the pole triangles.

            the_Vertices  : aliased Geometry.lit_colored_textured.Vertex_array := (1 ..  vertex_Count => <>);
            the_Indices   : aliased Indices                                    := (1 .. indices_Count => <>);

            the_arch_Edges : arch_Edges;
            i              : Index_t   := 1;

            pole_Site      : constant Site := (if is_Fore then (0.0, 0.0,  L + Radius)
                                                          else (0.0, 0.0, -L - Radius));

            Degrees_90        : constant := Pi / 2.0;
            Degrees_360       : constant := Pi * 2.0;

            latitude_Count    : constant :=  hoop_Count + 1;
            longitude_Count   : constant :=  Edges'Length;

            latitude_Spacing  : constant Real := Degrees_90  / Real (latitude_Count - 1);
            longitude_Spacing : constant Real := Degrees_360 / Real (longitude_Count);

            a, b : Real := 0.0;          -- Angular 'cursors' used to track lat/long for texture coords.
         begin
            if not is_Fore
            then
               a := Degrees_360;
            end if;

            -- Set the vertices.
            --
            start_nx := 0.0;
            start_ny := 1.0;

            for each_Hoop in 1 .. quality_Level
            loop
               --  Get n=start_n.
               --
               nx := start_nx;
               ny := start_ny;
               nz := 0.0;

               for Each in 1 .. sides_Count
               loop
                  the_arch_Edges (each_Hoop) (Each) (1) :=  ny * Radius;
                  the_arch_Edges (each_Hoop) (Each) (2) :=  nz * Radius;
                  the_arch_Edges (each_Hoop) (Each) (3) :=  (if is_Fore then nx * Radius + L
                                                                        else nx * Radius - L);
                  --  Rotate ny, nz.
                  --
                  tmp := ca * ny  -  sa * nz;
                  nz  := sa * ny  +  ca * nz;
                  ny  := tmp;

                  the_Vertices (i).Site   := the_arch_Edges (each_Hoop) (Each);
                  the_Vertices (i).Normal := Normalised ((the_Vertices (i).Site (1),
                                                          the_Vertices (i).Site (2),
                                                          (if is_Fore then the_Vertices (i).Site (3) - L
                                                                      else the_Vertices (i).Site (3) + L)));
                  the_Vertices (i).Color  := Self.Color;
                  the_Vertices (i).Shine  := 0.5;
                  the_Vertices (i).Coords :=  (s => a / Degrees_360,
                                               t => b / Degrees_90);
                  i := i + 1;
                  a := (if is_Fore then a + longitude_Spacing
                                   else a - longitude_Spacing);
               end loop;

               declare
                  tmp : constant Real := start_nx;
               begin
                  if is_Fore
                  then
                     start_nx :=  ca * start_nx  +  sa * start_ny;
                     start_ny := -sa * tmp       +  ca * start_ny;
                  else
                     start_nx :=  ca * start_nx  -  sa * start_ny;
                     start_ny :=  sa * tmp       +  ca * start_ny;
                  end if;
               end;

               a := (if is_Fore then 0.0
                                else Degrees_360);
               b := b + latitude_Spacing;
            end loop;

            -- Add pole vertex.
            --
            the_Vertices (i).Site   := pole_Site;
            the_Vertices (i).Normal := Normalised (pole_Site);
            the_Vertices (i).Color  := Self.Color;
            the_Vertices (i).Shine  := 0.5;
            the_Vertices (i).Coords := (s => 0.5,
                                        t => 1.0);
            -- Set indices.
            --
            declare
               i          :     long_Index_t := 1;
               Start      :          Index_t := 1;
               hoop_Start :          Index_t := 1;
               pole_Index : constant Index_t := vertex_Count;

            begin
               for each_Hoop in 1 .. quality_Level
               loop
                  for Each in 1 .. sides_Count
                  loop
                     declare
                        function next_hoop_Vertex return Index_t
                        is
                        begin
                           if Each = sides_Count then return hoop_Start;
                           else                       return Start + 1;
                           end if;
                        end next_hoop_Vertex;
                     begin
                        if each_Hoop = quality_Level
                        then
                           if is_Fore
                           then
                              the_Indices (i) := Start;              i := i + 1;
                              the_Indices (i) := next_hoop_Vertex;   i := i + 1;
                              the_Indices (i) := pole_Index;         i := i + 1;
                           else
                              the_Indices (i) := Start;              i := i + 1;
                              the_Indices (i) := pole_Index;         i := i + 1;
                              the_Indices (i) := next_hoop_Vertex;   i := i + 1;
                           end if;
                        else
                           declare
                              v1 : constant Index_t := Start;
                              v2 : constant Index_t := next_hoop_Vertex;
                              v3 : constant Index_t := v1 + sides_Count;
                              v4 : constant Index_t := v2 + sides_Count;
                           begin
                              if is_Fore
                              then
                                 the_Indices (i) := v1;   i := i + 1;
                                 the_Indices (i) := v2;   i := i + 1;
                                 the_Indices (i) := v3;   i := i + 1;

                                 the_Indices (i) := v2;   i := i + 1;
                                 the_Indices (i) := v4;   i := i + 1;
                                 the_Indices (i) := v3;   i := i + 1;
                              else
                                 the_Indices (i) := v1;   i := i + 1;
                                 the_Indices (i) := v3;   i := i + 1;
                                 the_Indices (i) := v2;   i := i + 1;

                                 the_Indices (i) := v2;   i := i + 1;
                                 the_Indices (i) := v3;   i := i + 1;
                                 the_Indices (i) := v4;   i := i + 1;
                              end if;
                           end;
                        end if;

                        Start := Start + 1;
                     end;
                  end loop;

                  hoop_Start := hoop_Start + sides_Count;
               end loop;

               if Self.Image /= null_Asset
               then
                  set_the_Texture:
                  declare
                     use Texture;
                     the_Image   : constant Image          := IO.to_Image      (Self.Image);
                     the_Texture : constant Texture.object := Forge.to_Texture (the_Image);
                  begin
                     cap_Geometry.Texture_is (the_Texture);
                  end set_the_Texture;
               end if;

               Vertices_are (cap_Geometry.all, the_Vertices);

               declare
                  the_Primitive : constant Primitive.indexed.view
                    := Primitive.indexed.new_Primitive (Primitive.Triangles,
                                                        the_Indices);
               begin
                  cap_Geometry.add (Primitive.view (the_Primitive));
               end;
            end;

            return cap_Geometry;
         end new_Cap;

      begin
         cap_1_Geometry := new_Cap (is_Fore => True);
         cap_2_Geometry := new_Cap (is_Fore => False);
      end;

      return (1 => the_shaft_Geometry.all'Access,
              2 =>     cap_1_Geometry.all'Access,
              3 =>     cap_2_Geometry.all'Access);
   end to_GL_Geometries;


end openGL.Model.capsule.lit_colored_textured;
