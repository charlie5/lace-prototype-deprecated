with
     lace.Text.Cursor,
     lace.Text.Forge;


package body any_math.any_geometry.any_d3.any_Modeller.any_Forge
is

   function to_Box_Model (half_Extents : in Vector_3 := (0.5, 0.5, 0.5)) return a_Model
   is
      the_Modeller : any_Modeller.item;
   begin
      the_Modeller.add_Triangle ((0.0, 0.0, 0.0),
                                 (1.0, 0.0, 0.0),
                                 (1.0, 1.0, 0.0));

      the_Modeller.add_Triangle ((1.0, 1.0, 0.0),
                                 (0.0, 1.0, 0.0),
                                 (0.0, 0.0, 0.0));

      --  tbd: Add the rest.

      return the_Modeller.Model;
   end to_Box_Model;



   function to_capsule_Model (Length : in Real := 1.0;
                              Radius : in Real := 0.5) return a_Model
   is
      use Functions;

      quality_Level : constant Positive := 4;
      sides_Count   : constant Positive := Positive (quality_Level * 4);     -- Number of sides to the cylinder (divisible by 4):

      type Edge is   -- 'barrel' edge.
         record
            Fore : Site;
            Aft  : Site;
         end record;

      type      Edges is array (Positive range 1 .. sides_Count)   of Edge;
      type arch_Edges is array (Positive range 1 .. quality_Level) of Sites (1 .. sides_Count);

      tmp,
      nx, ny, nz,
      start_nx, start_ny,
      a, ca, sa          : Real;
      L                  : Real := Length;

      the_Edges          : Edges;
      the_Factory        : any_Modeller.item;

   begin
      L  := Length * 0.5;
      a  := Pi * 2.0 / Real (sides_Count);
      sa := sin (a);
      ca := cos (a);

      --  Define cylinder body.
      --
      ny := 1.0;
      nz := 0.0;              -- normal vector = (0,ny,nz)

      for Each in Edges'Range
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
      end loop;


      for Each in edges'Range
      loop
         if Each /= edges'Last
         then
            the_Factory.add_Triangle (the_Edges (Each)    .Fore,
                                      the_Edges (Each)    .Aft,
                                      the_Edges (Each + 1).Aft);
            the_Factory.add_Triangle (the_Edges (Each + 1).Aft,
                                      the_Edges (Each + 1).Fore,
                                      the_Edges (Each)    .Fore);
         else
            the_Factory.add_Triangle (the_Edges (Each)       .Fore,
                                      the_Edges (Each)       .Aft,
                                      the_Edges (edges'First).Aft);
            the_Factory.add_Triangle (the_Edges (edges'First).Aft,
                                      the_Edges (edges'First).Fore,
                                      the_Edges (Each)       .Fore);
         end if;
      end loop;


      --  Define fore cylinder cap.
      --
      declare
         the_arch_Edges : arch_Edges;
      begin
         start_nx := 0.0;
         start_ny := 1.0;

         for each_Hoop in 1 .. quality_Level
         loop
            --  Get start_n2 = rotated start_n.
            --
            declare
               start_nx2 : constant Real :=  ca * start_nx  +  sa * start_ny;
               start_ny2 : constant Real := -sa * start_nx  +  ca * start_ny;
            begin
               --  Get n=start_n and n2=start_n2.
               --
               nx := start_nx;
               ny := start_ny;
               nz := 0.0;

               declare
                  nx2 : constant Real := start_nx2;
                  ny2 :          Real := start_ny2;
                  nz2 :          Real := 0.0;
               begin
                  for Each in 1 .. sides_Count
                  loop
                     the_arch_Edges (each_Hoop)(Each) (1) := ny2 * Radius;
                     the_arch_Edges (each_Hoop)(Each) (2) := nz2 * Radius;
                     the_arch_Edges (each_Hoop)(Each) (3) := nx2 * Radius + L;

                     --  Rotate n,n2.
                     --
                     tmp := ca * ny  -  sa * nz;
                     nz  := sa * ny  +  ca * nz;
                     ny  := tmp;

                     tmp := ca * ny2  -  sa * nz2;
                     nz2 := sa * ny2  +  ca * nz2;
                     ny2 := tmp;
                  end loop;
               end;

               start_nx := start_nx2;
               start_ny := start_ny2;
            end;
         end loop;


         for Each in 1 .. sides_Count
         loop
            if Each /= sides_Count
            then
               the_Factory.add_Triangle (the_Edges (Each).Fore,
                                         the_Edges (Each + 1).Fore,
                                         the_arch_Edges (1)(Each));
            else
               the_Factory.add_Triangle (the_Edges (Each).Fore,
                                         the_Edges (1).Fore,
                                         the_arch_Edges (1)(Each));
            end if;

            if Each /= sides_Count
            then
               the_Factory.add_Triangle (the_Edges (Each + 1).Fore,
                                         the_arch_Edges (1)(Each + 1),
                                         the_arch_Edges (1)(Each));
            else
               the_Factory.add_Triangle (the_Edges (1).Fore,
                                         the_arch_Edges (1)(1),
                                         the_arch_Edges (1)(Each));
            end if;
         end loop;


         for each_Hoop in 1 .. quality_Level - 1
         loop
            for Each in 1 .. sides_Count
            loop
               declare
                  function next_hoop_Vertex return Positive
                  is
                  begin
                     if Each = sides_Count then return 1;
                     else                       return Each + 1;
                     end if;
                  end next_hoop_Vertex;
               begin
                  the_Factory.add_Triangle (the_arch_Edges (each_Hoop)    (Each),
                                            the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
                                            the_arch_Edges (each_Hoop + 1)(Each));

                  if each_Hoop /= quality_Level - 1
                  then
                     the_Factory.add_Triangle (the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
                                               the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex),
                                               the_arch_Edges (each_Hoop + 1)(Each));
                  end if;
               end;
            end loop;
         end loop;
      end;


      --  Define aft cylinder cap.
      --
      declare
         the_arch_Edges : arch_Edges;
      begin
         start_nx := 0.0;
         start_ny := 1.0;

         for each_Hoop in 1 .. quality_Level
         loop
            declare
               --  Get start_n2 = rotated start_n.
               --
               start_nx2 : constant Real := ca * start_nx  -  sa * start_ny;
               start_ny2 : constant Real := sa * start_nx  +  ca * start_ny;
            begin
               --  Get n=start_n and n2=start_n2.
               --
               nx := start_nx;
               ny := start_ny;
               nz := 0.0;

               declare
                  nx2 : constant Real := start_nx2;
                  ny2 :          Real := start_ny2;
                  nz2 :          Real := 0.0;
               begin
                  for Each in 1 .. sides_Count
                  loop
                     the_arch_Edges (each_Hoop)(Each) (1) := ny2 * Radius;
                     the_arch_Edges (each_Hoop)(Each) (2) := nz2 * Radius;
                     the_arch_Edges (each_Hoop)(Each) (3) := nx2 * Radius - L;

                     --  Rotate n,n2
                     --
                     tmp := ca * ny  -  sa * nz;
                     nz  := sa * ny  +  ca * nz;
                     ny  := tmp;
                     tmp := ca * ny2  -  sa * nz2;
                     nz2 := sa * ny2  +  ca * nz2;
                     ny2 := tmp;
                  end loop;
               end;

               start_nx := start_nx2;
               start_ny := start_ny2;
            end;
         end loop;


         for Each in 1 .. sides_Count
         loop
            if Each /= sides_Count
            then
               the_Factory.add_Triangle (the_Edges (Each).Aft,
                                         the_arch_Edges (1)(Each),
                                         the_Edges (Each + 1).Aft);
            else
               the_Factory.add_Triangle (the_Edges (Each).Aft,
                                         the_arch_Edges (1)(Each),
                                         the_Edges (1).Aft);
            end if;

            if Each /= sides_Count
            then
               the_Factory.add_Triangle (The_Edges (Each + 1).Aft,
                                         the_arch_Edges (1)(Each),
                                         the_arch_Edges (1)(Each + 1));
            else
               the_Factory.add_Triangle (the_Edges (1).Aft,
                                         the_arch_Edges (1)(Each),
                                         the_arch_Edges (1)(1));
            end if;
         end loop;


         for each_Hoop in 1 .. quality_Level - 1
         loop
            for Each in 1 .. sides_Count
            loop
               declare
                  function next_hoop_Vertex return Positive
                  is
                  begin
                     if Each = sides_Count then return 1;
                     else                       return Each + 1;
                     end if;
                  end next_hoop_Vertex;
               begin
                  the_Factory.add_Triangle (the_arch_Edges (each_Hoop)    (Each),
                                            the_arch_Edges (each_Hoop + 1)(Each),
                                            the_arch_Edges (each_Hoop)    (next_hoop_Vertex));

                  if each_Hoop /= quality_Level - 1
                  then
                     the_Factory.add_Triangle (the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
                                               the_arch_Edges (each_Hoop + 1)(Each),
                                               the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex));
                  end if;
               end;
            end loop;
         end loop;
      end;


      return the_Factory.Model;
   end to_capsule_Model;




   -- Polar to euclidian shape models.
   --

   package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use Float_Functions;


   function to_Radians (from : in Latitude) return Real
   is
   begin
      return real (from) * Pi / 180.0;
   end to_Radians;


   function to_Radians (from : in Longitude) return Real
   is
   begin
      return real (from) * Pi / 180.0;
   end to_Radians;



   function polar_Model_from (Model_Filename : in String) return polar_model
   is
   begin
      declare
         use lace.Text, lace.Text.Cursor;

         the_Text      : aliased lace.Text.item        := lace.Text.Forge.to_Text (Filename => lace.Text.Forge.Filename (Model_Filename));
         Cursor        : aliased lace.Text.Cursor.item := lace.Text.Cursor.First (the_Text'unchecked_Access);

         the_Latitude  :         latitude;
         the_Longitude :         longitude;
         the_Value     :         Integer;
         the_Distance  :         Real;

         the_Model     :         polar_Model;

      begin
         while has_Element (Cursor)
         loop
            the_Value := get_Integer (Cursor'Access);
            exit when the_Value = 360;

            the_Longitude := longitude (the_Value);
            the_Latitude  := latitude  (get_Integer (Cursor'access));

            the_Distance  := Real  (get_Real (Cursor'Access)); -- * 0.1; --1.0;

            skip_White (Cursor);

            the_Model (the_Longitude) (the_Latitude).Site (1) := 10.0 * cos (to_Radians (the_Latitude)) * sin (to_Radians (the_Longitude)) * the_Distance;
            the_Model (the_Longitude) (the_Latitude).Site (2) := 10.0 * sin (to_Radians (the_Latitude)) * the_Distance;
            the_Model (the_Longitude) (the_Latitude).Site (3) := 10.0 * cos (to_Radians (the_Latitude)) * cos (to_Radians (the_Longitude)) * the_Distance;
         end loop;

         return the_Model;
      end;
   end polar_Model_from;




   function mesh_Model_from (the_Model : in polar_Model) return a_Model
   is
      the_raw_model  : polar_Model := the_Model;

      the_mesh_Model : a_Model (site_Count => 2522,
                                tri_Count  => 73 * (16 * 4 + 6));

      the_longitude  : longitude := 0;
      the_latitude   : latitude ;

      the_Vertex     : Positive := 1;
      the_Triangle   : Positive := 1;

      the_North_Pole : Positive;
      the_South_Pole : Positive;

      function Sum (the_Longitude : in longitude;   Increment : in Integer) return longitude
      is
         Result : Integer := Integer (the_Longitude) + Increment;
      begin
         if Result >= 360
         then
            Result := Result - 360;
         end if;

         return longitude (Result);
      end Sum;


   begin
      the_mesh_Model.Sites (the_Vertex) := (the_raw_model (0) (-90).Site);
      the_North_Pole                    := the_Vertex;
      the_raw_Model (0) (-90).Id        := the_Vertex;
      the_Vertex                        := the_Vertex + 1;

      the_mesh_Model.Sites (the_Vertex) := (the_raw_model (0) (90).Site);
      the_south_Pole                    := the_Vertex;
      the_raw_Model (0) (90).Id         := the_Vertex;
      the_Vertex                        := the_Vertex + 1;

      loop
         the_latitude := -90;
         loop
            if the_Latitude = -90
            then
               the_raw_Model (the_Longitude) (the_Latitude).Id := the_North_Pole;

            elsif the_Latitude = 90
            then
               the_raw_Model (the_Longitude) (the_Latitude).Id := the_South_Pole;

            else
               the_mesh_Model.Sites (the_Vertex)               := the_raw_model (the_Longitude) (the_Latitude).Site;
               the_raw_Model (the_Longitude) (the_Latitude).Id := the_Vertex;
               the_Vertex                                      := the_Vertex + 1;
            end if;

            exit when the_Latitude = 90;

            the_Latitude := the_Latitude + 5;
         end loop;

         exit when the_Longitude = 355;

         the_Longitude := the_Longitude + 5;
      end loop;


      the_Longitude := 0;
      loop
         the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_North_Pole,
                                                      2 => the_raw_Model (Sum (the_Longitude, 5)) (-85).Id,
                                                      3 => the_raw_Model (     the_Longitude    ) (-85).Id);
         the_Triangle := the_Triangle + 1;

         the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_South_Pole,
                                                      2 => the_raw_Model      (the_Longitude)     (85).Id,
                                                      3 => the_raw_Model (Sum (the_Longitude, 5)) (85).Id);
         the_Triangle := the_Triangle + 1;

         the_latitude := -85;
         loop
            the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_raw_Model (     the_Longitude)     (the_Latitude    ).Id,
                                                         2 => the_raw_Model (Sum (the_Longitude, 5)) (the_Latitude    ).Id,
                                                         3 => the_raw_Model (     the_Longitude)     (the_Latitude + 5).Id);
            the_Triangle := the_Triangle + 1;


            the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_raw_Model (the_Longitude)          (the_Latitude + 5).Id,
                                                         2 => the_raw_Model (Sum (the_Longitude, 5)) (the_Latitude    ).Id,
                                                         3 => the_raw_Model (Sum (the_Longitude, 5)) (the_Latitude + 5).Id);
            the_Triangle := the_Triangle + 1;


            the_Latitude := the_Latitude + 5;
            exit when       the_Latitude = 85;
         end loop;

         exit when        the_Longitude = 355;
         the_Longitude := the_Longitude + 5;
      end loop;

      the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_North_Pole,
                                                   2 => the_raw_Model (5) (-85).Id,
                                                   3 => the_raw_Model (0) (-85).Id);
      the_Triangle := the_Triangle + 1;

      the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_South_Pole,
                                                   2 => the_raw_Model (0) (85).Id,
                                                   3 => the_raw_Model (5) (85).Id);
      the_Triangle := the_Triangle + 1;


      the_latitude := -85;
      loop
         the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_raw_Model (0) (the_Latitude    ).Id,
                                                      2 => the_raw_Model (5) (the_Latitude    ).Id,
                                                      3 => the_raw_Model (0) (the_Latitude + 5).Id);
         the_Triangle := the_Triangle + 1;

         the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_raw_Model (0) (the_Latitude + 5).Id,
                                                      2 => the_raw_Model (5) (the_Latitude    ).Id,
                                                      3 => the_raw_Model (5) (the_Latitude + 5).Id);
         the_Triangle := the_Triangle + 1;


         the_Latitude := the_Latitude + 5;
         exit when       the_Latitude = 85;
      end loop;

      return the_mesh_Model;
   end mesh_Model_from;


end any_math.any_geometry.any_d3.any_Modeller.any_Forge;
