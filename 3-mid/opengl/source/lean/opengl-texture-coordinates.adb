package body openGL.Texture.Coordinates
is

   overriding
   function to_Coordinates (Self : in xz_Generator;   the_Vertices : access Sites) return Coordinates_2D
   is
      the_Coords : Coordinates_2D (1 .. the_Vertices'Length);
   begin
      for Each in the_Coords'Range
      loop
         declare
            the_Vertex : Site renames the_Vertices (Each);
            S, T       : Real;
         begin
            -- Normalise.
            --
            S := (  the_Vertex (1)
                  + Self.Normalise.S.Offset) * Self.Normalise.S.Scale;
            T := 1.0 -   (  the_Vertex (3)
                          + Self.Normalise.T.Offset)
                       * Self.Normalise.T.Scale;
            -- Tile.
            --
            S :=   (S + Self.Tile.S.Offset)
                 * Self.Tile.S.Scale;
            T :=   (T + Self.Tile.T.Offset)
                 * Self.Tile.T.Scale;

            the_Coords (Each).S := S;
            the_Coords (Each).T := T;
         end;
      end loop;

      return the_Coords;
   end to_Coordinates;



   overriding
   function to_Coordinates (Self : in xy_Generator;   the_Vertices : access Sites) return Coordinates_2D
   is
      the_Coords : Coordinates_2D (1 .. the_Vertices'Length);
   begin
      for Each in the_Coords'Range
      loop
         declare
            the_Vertex : Site renames the_Vertices (Index_t (Each));
            S, T       : Real;
         begin
            -- Normalise.
            --
            S :=   (the_Vertex (1) + Self.Normalise.S.Offset)
                 * Self.Normalise.S.Scale;
            T := 1.0 -   (  the_Vertex (2)
                          + Self.Normalise.T.Offset)
                       * Self.Normalise.T.Scale;
            -- Tile.
            --
            S :=   (S + Self.Tile.S.Offset)
                 * Self.Tile.S.Scale;
            T :=   (T + Self.Tile.T.Offset)
                 * Self.Tile.T.Scale;

            the_Coords (Each).S := S;
            the_Coords (Each).T := T;
         end;
      end loop;

      return the_Coords;
   end to_Coordinates;



   overriding
   function to_Coordinates (Self : in zy_Generator;   the_Vertices : access Sites) return Coordinates_2D
   is
      the_Coords : Coordinates_2D (1 .. the_Vertices'Length);
   begin
      for Each in the_Coords'Range
      loop
         declare
            the_Vertex : Site renames the_Vertices (Index_t (Each));
            S, T       : Real;
         begin
            -- Normalise.
            --
            S :=   (the_Vertex (3) + Self.Normalise.S.Offset)
                                   * Self.Normalise.S.Scale;
            T := 1.0 -   (  the_Vertex (2)
                          + Self.Normalise.T.Offset)
                       * Self.Normalise.T.Scale;
            -- Tile.
            --
            S :=   (S + Self.Tile.S.Offset)
                 * Self.Tile.S.Scale;
            T :=   (T + Self.Tile.T.Offset)
                 * Self.Tile.T.Scale;

            the_Coords (Each).S := S;
            the_Coords (Each).T := T;
         end;
      end loop;

      return the_Coords;
   end to_Coordinates;



   --  TODO: - Below does not cater for 'right edge' case where 's' should be
   --          1.0 rather than 0.0
   --
   --       - Would be possible given a known set of vertices
   --         ie - First vertex is North Pole,
   --            - Last  vertex is South Pole,
   --            - Middle vertices are a set of latitude rings.
   --              - Each rings first vertex site should map s => 0.0
   --              - Each rings last  vertex is a duplicate of the first and
   --                will be mapped to s => 1.0
   --
   overriding
   function to_Coordinates (Self : in mercator_Generator;   the_Vertices : access Sites) return Coordinates_2D
   is
      pragma Unreferenced (Self);
      the_Coords : Coordinates_2D (1 .. the_Vertices'Length);
   begin
      for Each in the_Coords'Range
      loop
         declare
            use real_Functions;

            the_Vertex : Site renames the_Vertices (Index_t (Each));

            x : Real renames the_Vertex (1);
            y : Real renames the_Vertex (2);
            z : Real renames the_Vertex (3);

            Degrees_90  : constant      := Pi / 2.0;
            Degrees_180 : constant      := Pi;
            Radius      : constant Real := SqRt (x * x  +  y * y  +  z * z);

            Latitude    : constant Real := arcSin (y / Radius);
            Longitude   :          Real;
         begin
            if         z = 0.0
              and then x = 0.0
            then
               the_Coords (Each).S := 0.5;
            else
               Longitude           := arcTan (-z, x);
               the_Coords (Each).S := (Longitude / Degrees_180 + 1.0) / 2.0;
            end if;

            the_Coords (Each).T := (Latitude / Degrees_90 + 1.0) / 2.0;
         end;
      end loop;

      return the_Coords;
   end to_Coordinates;


end openGL.Texture.Coordinates;
