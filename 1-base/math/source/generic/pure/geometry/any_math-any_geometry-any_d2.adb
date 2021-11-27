package body any_Math.any_Geometry.any_d2
is

   ---------
   --  Sites
   --

   function Distance (From, To : Site) return Real
   is
      use Functions;
   begin
      return SqRt (  (To (1) - From (1)) ** 2
                   + (To (2) - From (2)) ** 2);
   end Distance;



   function to_Polar (Self : in Site) return polar_Site
   is
      use any_Math.complex_Reals;
      the_Complex : constant Complex := compose_from_Cartesian (Self (1),
                                                                Self (2));
   begin
      return (Angle  => Argument (the_Complex),
              Extent => Modulus  (the_Complex));
   end to_Polar;



   function to_Site (Self : in polar_Site) return Site
   is
      use any_Math.complex_Reals;
      the_Complex : constant Complex := compose_from_Polar (Modulus  => Self.Extent,
                                                            Argument => Self.Angle);
   begin
      return (the_Complex.Re,
              the_Complex.Im);
   end to_Site;



   function Angle (Self : in Site) return Radians
   is
      use any_Math.complex_Reals;
      the_Complex : constant Complex := compose_from_Cartesian (Self (1),
                                                                Self (2));
   begin
      return Argument (the_Complex);
   end Angle;



   function Extent (Self : in Site) return Real
   is
      use any_Math.complex_Reals;
      the_Complex : constant Complex := compose_from_Cartesian (Self (1),
                                                                Self (2));
   begin
      return Modulus (the_Complex);
   end Extent;



   ---------
   --  Lines
   --

   function to_Line (Anchor : in Site;
                     Angle  : in Radians) return Line
   is
      use Functions;
   begin
      return (Kind     => anchored_Gradient,
              Anchor   => Anchor,
              Gradient => Tan (Angle));   -- TODO: What about infinite gradient ? ie 90 and 270 degrees ?
   end to_Line;



   function to_Line (Site_1,
                     Site_2 : in Site) return Line
   is
   begin
      return (Kind  => two_Points,
              Sites => (Site_1,
                        Site_2));
   end to_Line;



   function X_of (Self : in Line;   Y : in Real) return Real
   is
   begin
      return
          (Y - Self.Anchor (2))  /  Self.Gradient
        + Self.Anchor (1);
   end X_of;



   function Y_of (Self : in Line;   X : in Real) return Real
   is
   begin
      return
          Self.Gradient * (X - Self.Anchor (1))
        + Self.Anchor (2);
   end Y_of;



   function Gradient (Self : in Line) return Real
   is
      Run  : constant Real := Self.Sites (2)(1) - Self.Sites (1)(1);
   begin
      if Run = 0.0
      then
         return Real'Last;
      else
         return   (Self.Sites (2) (2) - Self.Sites (1) (2))
                / Run;
      end if;
   end Gradient;



   ----------
   --  Bounds
   --

   function to_bounding_Box (Self : Sites) return bounding_Box
   is
      Result : bounding_Box := null_Bounds;
   begin
      for Each in Self'Range
      loop
         Result.Lower (1) := Real'Min  (Result.Lower (1),  Self (Each)(1));
         Result.Lower (2) := Real'Min  (Result.Lower (2),  Self (Each)(2));

         Result.Upper (1) := Real'Max  (Result.Upper (1),  Self (Each)(1));
         Result.Upper (2) := Real'Max  (Result.Upper (2),  Self (Each)(2));
      end loop;

      return Result;
   end to_bounding_Box;



   function Extent (Self : in bounding_Box;   Dimension : in Index) return Real
   is
   begin
      return Self.Upper (Dimension) - Self.Lower (Dimension);
   end Extent;



   function "or" (Left : in bounding_Box;   Right : in Site) return bounding_Box
   is
      Result : bounding_Box;
   begin
      for i in Right'Range
      loop
         if Right (i) < Left.Lower (i)
         then   Result.Lower (i) := Right (i);
         else   Result.Lower (i) := Left.Lower (i);
         end if;

         if Right (i) > Left.Upper (i)
         then   Result.Upper (i) := Right (i);
         else   Result.Upper (i) := Left.Upper (i);
         end if;
      end loop;

      return Result;
   end "or";



   function "or" (Left : in bounding_Box;   Right : in bounding_Box) return bounding_Box
   is
      Result : bounding_Box := Left or Right.Lower;
   begin
      Result := Result or Right.Upper;
      return Result;
   end "or";




   function "+" (Left : in bounding_Box;   Right : in Vector_2) return bounding_Box
   is
   begin
      return (Left.Lower + Right,
              Left.Upper + Right);
   end "+";



   function Image (Self : bounding_Box) return String
   is
   begin
      return    "(Lower => " & Image (Self.Lower)
             & ", Upper => " & Image (Self.Upper) & ")";
   end Image;



   ------------
   -- Triangles
   --

   procedure check (Self : in Triangle)
   is
   begin
      if   Self.Vertices (1) = Self.Vertices (2)
        or Self.Vertices (1) = Self.Vertices (3)
        or Self.Vertices (2) = Self.Vertices (3)
      then
         raise Degenerate;
      end if;

      declare
         L1 : constant Line := to_Line (Self.Vertices (1),  Self.Vertices (2));
         L2 : constant Line := to_Line (Self.Vertices (2),  Self.Vertices (3));
         L3 : constant Line := to_Line (Self.Vertices (3),  Self.Vertices (1));

         M1 : constant Real := Gradient (L1);
         M2 : constant Real := Gradient (L2);
         M3 : constant Real := Gradient (L3);
      begin
         if   M1 = M2
           or M1 = M3
           or M2 = M3
         then
            raise Colinear with
                "  G1: " & Image (M1)
              & "  G2: " & Image (M2)
              & "  G3: " & Image (M3);
         end if;
      end;
   end check;
   pragma Unreferenced (check);



   --  function Area (Self : in Triangle) return Real
   --  --
   --  -- This is an implementation of Heron's formula.
   --  -- It is numerically unstable with very small angles.
   --  --
   --  is
   --     use Functions;
   --
   --     A : constant Real := Distance (Self.Vertices (1),  Self.Vertices (2));
   --     B : constant Real := Distance (Self.Vertices (2),  Self.Vertices (3));
   --     C : constant Real := Distance (Self.Vertices (3),  Self.Vertices (1));
   --
   --     S : constant Real := (A + B + C) / 2.0;                 -- Semi-perimeter.
   --
   --  begin
   --     return Real (SqRt (S * (S - A) * (S - B) * (S - C)));   -- Herons formula.
   --  end Area;



   function Area (Self : in Triangle) return Real
   --
   -- This is a numerically stable implementation of Heron's formula.
   -- See: https://en.wikipedia.org/wiki/Heron%27s_formula#Numerical_stability.
   --
   is
      use Functions;

      a : Real := Distance (Self.Vertices (1),  Self.Vertices (2));
      b : Real := Distance (Self.Vertices (2),  Self.Vertices (3));
      c : Real := Distance (Self.Vertices (3),  Self.Vertices (1));

      D : Real;
   begin
      -- Sort the lengths such that a >= b >= c.
      --
      if c > b then swap (b, c); end if;
      if a < b then swap (a, b); end if;
      if b < c then swap (b, c); end if;

      D :=   (a + (b + c))
           * (c - (a - b))
           * (c + (a - b))
           * (a + (b - c));

      if D <= 0.0
      then
         return 0.0;
      end if;

      return 0.25 * SqRt (D);
   end Area;



   function Perimeter (Self : Triangle) return Real
   is
   begin
      return
          Distance (Self.Vertices (1), Self.Vertices (2))
        + Distance (Self.Vertices (2), Self.Vertices (3))
        + Distance (Self.Vertices (3), Self.Vertices (1));
   end Perimeter;



   function prior_Vertex (Self : in Triangle;    to_Vertex : in Positive) return Site
   is
   begin
      if to_Vertex = 1
      then   return Self.Vertices (3);
      else   return Self.Vertices (to_Vertex - 1);
      end if;
   end prior_Vertex;



   function next_Vertex (Self : in Triangle;    to_Vertex : in Positive) return Site
   is
   begin
      if to_Vertex = 3
      then   return Self.Vertices (1);
      else   return Self.Vertices (to_Vertex + 1);
      end if;
   end next_Vertex;



   function Angle (Self : in Triangle;   at_Vertex : in Positive) return Radians
   is
      use Functions;

      a     : constant Real := Distance (next_Vertex   (Self, to_vertex => at_Vertex),
                                         prior_Vertex  (Self, to_vertex => at_Vertex));
      b     : constant Real := Distance (Self.Vertices (at_Vertex),                     next_Vertex  (Self, to_vertex => at_Vertex));
      c     : constant Real := Distance (Self.Vertices (at_Vertex),                     prior_Vertex (Self, to_vertex => at_Vertex));

      cos_A : constant Real := (b**2 + c**2 - a**2) / (2.0 * b * c);

   begin
      if    cos_A < -1.0 then   return to_Radians (180.0);
      elsif cos_A >  1.0 then   return 0.0;
      else                      return arcCos (cos_A);
      end if;
   end Angle;



   ----------
   -- Circles
   --

   function Area (Self : Circle) return Real
   is
   begin
      return Pi * Self.Radius**2;
   end Area;



   function Perimeter (Self : Circle) return Real
   is
   begin
      return 2.0 * Pi * Self.Radius;
   end Perimeter;




   -----------
   -- Polygons
   --

   function Centroid (Self : in Polygon) return Site
   is
      Result : Site := Origin_2d;
   begin
      for i in 1 .. Self.Vertex_Count
      loop
         Result := Result + Self.Vertices (i);
      end loop;

      Result := Result / Real (Self.Vertex_Count);
      return Result;
   end Centroid;



   procedure center (Self : in out Polygon)
   is
      Center : constant Site := Centroid (Self);
   begin
      for i in 1 .. Self.Vertex_Count
      loop
         Self.Vertices (i) := Self.Vertices (i) - Center;
      end loop;
   end center;



   function prior_Vertex (Self : in Polygon;    to_Vertex : in Positive) return Site
   is
   begin
      if To_Vertex = 1
      then   return Self.Vertices (Self.Vertex_Count);
      else   return Self.Vertices (to_Vertex - 1);
      end if;
   end prior_Vertex;



   function next_Vertex (Self : in Polygon;    to_Vertex : in Positive) return Site
   is
   begin
      if to_Vertex = Self.Vertex_Count
      then   return Self.Vertices (1);
      else   return Self.Vertices (to_Vertex + 1);
      end if;
   end next_Vertex;



   function is_Triangle (Self : in Polygon) return Boolean
   is
   begin
      return Self.Vertex_Count = 3;
   end is_Triangle;



   function is_Clockwise (Self : in Polygon) return Boolean
   is
      i : constant Site := Self.Vertices (1);
      j : constant Site := Self.Vertices (1);
      k : constant Site := Self.Vertices (1);

      z : Real :=    (j (1) - i (1))
                   * (k (2) - j (2));
   begin
      z := z -   (j (2) - i (2))
               * (k (1) - j (1));

      return z < 0.0;
   end is_Clockwise;



   function is_Convex (Self : in Polygon) return Boolean
   is
      negative_Found,
      positive_Found : Boolean := False;

   begin
      if is_Triangle (Self)
      then
         return True;     -- All triangles are convex.
      end if;

      for i in 1 .. Self.Vertex_Count
      loop
         declare
            k0 : constant Site := Self.Vertices (i);


            function get_k1 return Site
            is
            begin
               if i = Self.Vertex_Count
               then   return Self.Vertices (1);
               else   return Self.Vertices (i + 1);
               end if;
            end get_k1;

            k1 : constant Site := get_k1;


            function get_k2 return Site
            is
            begin
               if    i = Self.Vertex_Count - 1 then   return Self.Vertices (1);
               elsif i = Self.Vertex_Count     then   return Self.Vertices (2);
               else                                   return Self.Vertices (i + 2);
               end if;
            end get_k2;

            k2 : constant Site := get_k2;


            function get_Crossproduct return Real
            is
               dx1 : constant Real := k1 (1) - k0 (1);
               dy1 : constant Real := k1 (2) - k0 (2);

               dx2 : constant Real := k2 (1) - k1 (1);
               dy2 : constant Real := k2 (2) - k1 (2);
            begin
               return dx1 * dy2  -  dy1 * dx2;
            end get_Crossproduct;

            Crossproduct : constant Real := get_Crossproduct;

         begin
            if Crossproduct > 0.0
            then
               if negative_Found
               then
                  return False;
               end if;

               positive_Found := True;

            elsif Crossproduct < 0.0
            then
               if positive_Found
               then
                  return False;
               end if;

               negative_Found := True;
            end if;
         end;
      end loop;

      return True;
   end is_Convex;



   function Area (Self : Polygon) return Real
   is
      Result : Real := 0.0;
   begin
      for i in 2 .. Self.Vertex_Count - 1
      loop
         Result := Result + Area (Triangle' (Vertices => (Self.Vertices (1),
                                                          Self.Vertices (i),
                                                          Self.Vertices (i + 1))));
      end loop;

      return Result;
   end Area;



   function Perimeter (Self : Polygon) return Real
   is
      Result : Real := Distance (Self.Vertices (1),
                                 Self.Vertices (Self.Vertex_Count));
   begin
      for i in 1 .. Self.Vertex_Count - 1
      loop
         Result := Result + Distance (Self.Vertices (i),
                                      Self.Vertices (i + 1));
      end loop;

      return Result;
   end Perimeter;



   function Angle (Self : in Polygon;   at_Vertex : in Positive) return Radians
   is
      Tri : constant Triangle := (vertices => (Self.Vertices (at_Vertex),
                                               next_Vertex   (Self, at_Vertex),
                                               prior_Vertex  (Self, at_Vertex)));
   begin
      return Angle (Tri, 1);
   end Angle;



   function Image (Self : in Polygon) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Polygon image (TODO)";
   end Image;


end any_Math.any_Geometry.any_d2;
