

generic
package any_Math.any_Geometry.any_d2
--
--  Provides a namespace and core types for 2D geometry.
--
is
   pragma Pure;


   ---------
   --  Sites
   --


   -- Cartesian
   --
   subtype Site  is Vector_2;                                 -- 2D cartesian coordinates.
   type    Sites is array (Positive range <>) of Site;

   function Distance (From, To : Site) return Real;


   -- Polar
   --
   type polar_Site is                                         -- 2D polar coordinates.
      record
         Angle  : Radians;
         Extent : Real;
      end record;

   function to_Polar (Self : in Site)       return polar_Site;
   function to_Site  (Self : in polar_Site) return Site;

   function Angle    (Self : in Site) return Radians;
   function Extent   (Self : in Site) return Real;



   ---------
   --  Lines
   --

   type Line is private;

   function to_Line  (Anchor : in Site;
                      Angle  : in Radians) return Line;

   function to_Line  (Site_1,
                      Site_2 : in Site)    return Line;

   function X_of     (Self   : in Line;   Y : in Real) return Real;
   function Y_of     (Self   : in Line;   X : in Real) return Real;

   function Gradient (Self   : in Line) return Real;



   ----------
   --  Bounds
   --

   type bounding_Box is
      record
         Lower,
         Upper : Site;
      end record;

   null_Bounds : constant bounding_Box;


   function to_bounding_Box (Self : Sites) return bounding_Box;


   function "or"   (Left : in bounding_Box;   Right : in Site)         return bounding_Box;
   --
   --  Returns the bounds expanded to include the vector.

   function "or"   (Left : in bounding_Box;   Right : in bounding_Box) return bounding_Box;
   --
   --  Returns the bounds expanded to include both Left and Right.


   function "+"    (Left : in bounding_Box;   Right : in Vector_2)     return bounding_Box;
   --
   --  Returns the bounds translated by the vector.


   function Extent (Self : in bounding_Box;   Dimension : in Index)    return Real;
   function Image  (Self : in bounding_Box)                            return String;



   ----------
   -- Circles
   --

   type Circle is
      record
         Radius : Real;
      end record;

   function Area      (Self : Circle) return Real;
   function Perimeter (Self : Circle) return Real;



   -----------
   -- Polygons
   --

   type Polygon (Vertex_Count : Positive) is
      record
         Vertices : Sites (1 .. Vertex_Count);
      end record;

   function  Area         (Self : in     Polygon) return Real;     -- Polygon must be convex.
   function  Perimeter    (Self : in     Polygon) return Real;
   function  Angle        (Self : in     Polygon;   at_Vertex : in Positive) return Radians;

   function  is_Triangle  (Self : in     Polygon) return Boolean;
   function  is_Convex    (Self : in     Polygon) return Boolean;
   function  is_Clockwise (Self : in     Polygon) return Boolean;

   function  Centroid     (Self : in     Polygon) return Site;
   procedure center       (Self : in out Polygon);

   function  prior_Vertex (Self : in     Polygon;    to_Vertex : in Positive) return Site;
   function   next_Vertex (Self : in     Polygon;    to_Vertex : in Positive) return Site;

   function  Image        (Self : in     Polygon) return String;



   ------------
   -- Triangles
   --

   type Triangle is
      record
         Vertices : Sites (1 .. 3);
      end record;

   function Area         (Self : in Triangle)                            return Real;
   function Perimeter    (Self : in Triangle)                            return Real;
   function Angle        (Self : in Triangle;   at_Vertex : in Positive) return Radians;

   function prior_Vertex (Self : in Triangle;   to_Vertex : in Positive) return Site;
   function  next_Vertex (Self : in Triangle;   to_Vertex : in Positive) return Site;

   Degenerate,
   Colinear  : exception;



private

   type Line_Format is (anchored_Gradient, two_Points);

   type Line (Kind : Line_Format := Line_Format'First) is
      record
         case Kind is
            when anchored_Gradient =>
               Anchor   : Site;
               Gradient : Real;

            when two_Points =>
               Sites    : any_d2.Sites (1 .. 2);
         end case;
      end record;


   null_Bounds : constant bounding_Box := (lower => (Real'Last,  Real'Last),
                                           upper => (Real'First, Real'First));
end any_Math.any_Geometry.any_d2;
