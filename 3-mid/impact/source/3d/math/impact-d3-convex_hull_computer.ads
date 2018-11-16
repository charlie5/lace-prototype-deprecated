with impact.d3.Containers,
     ada.containers.Vectors;

private
with Interfaces,
     System;


 
package impact.d3.convex_hull_Computer
--
--  Convex hull implementation based on Preparata and Hong
--
--  See http://code.google.com/p/bullet/issues/detail?id=275
--  Ole Kniemeyer, MAXON Computer GmbH
--
is

   --- Edge
   --

   type Edge is tagged
      record
         next,
         m_reverse,
         targetVertex : Integer;
      end record;

   type Edge_view is access all Edge'Class;
   type Edges     is array (Positive range <>) of aliased Edge;



   function getSourceVertex (Self : access Edge) return Integer;
   function getTargetVertex (Self : in     Edge) return Integer;

   function getNextEdgeOfVertex (Self : access Edge) return Edge_view;    --         clockwise list of all edges of a vertex
   function getNextEdgeOfFace   (Self : access Edge) return Edge_view;    -- counter-clockwise list of all edges of a face
   function getReverseEdge      (Self : access Edge) return Edge_view;


   package edge_Vectors is new ada.containers.Vectors (Positive, Edge_view);
   subtype edge_Vector  is     edge_Vectors.Vector;






   --- impact.d3.convex_HullComputer
   --

   type Item is tagged
      record
         vertices : impact.d3.Containers.vector_3_Vector;    -- Vertices of the output hull.
         edges    :                       edge_Vector;    -- Edges of the output hull.
         faces    : impact.d3.Containers. integer_Vector;    -- Faces of the convex hull. Each entry is an index into the "edges" array pointing to an edge of the face.
                                                             -- Faces are planar n-gons.
      end record;



   function compute (Self : access Item'Class;   coords       : access math.Real;
                                                 stride       : in     Integer;
                                                 count        : in     Integer;
                                                 shrink       : in     math.Real;
                                                 shrinkClamp  : in     math.Real) return math.Real;
   --
   --                  Compute convex hull of "count" vertices stored in "coords". "stride" is the difference in bytes
   --                  between the addresses of consecutive vertices. If "shrink" is positive, the convex hull is shrunken
   --                  by that amount (each face is moved by "shrink" length units towards the center along its normal).
   --                  If "shrinkClamp" is positive, "shrink" is clamped to not exceed "shrinkClamp * innerRadius", where "innerRadius"
   --                  is the minimum distance of a face to the center of the convex hull.
   --
   --                  The returned value is the amount by which the hull has been shrunken. If it is negative, the amount was so large
   --                  that the resulting convex hull is empty.
   --
   --                  The output convex hull can be found in the member variables "vertices", "edges", "faces".



   function compute (Self : access Item'Class;   coords       : access Long_Float;
                                                 stride       : in     Integer;
                                                 count        : in     Integer;
                                                 shrink       : in     math.Real;
                                                 shrinkClamp  : in     math.Real) return math.Real;
   --
   --  same as above, but double precision







private



   function compute (Self : access Item'Class;   coords       : in     system.Address;
                                                 doubleCoords : in     Boolean;
                                                 stride       : in     Integer;
                                                 count        : in     Integer;
                                                 the_shrink   : in     math.Real;
                     shrinkClamp  : in     math.Real) return math.Real;

--                  impact.d3.Scalar compute(const void* coords, bool doubleCoords, int stride, int count, impact.d3.Scalar shrink, impact.d3.Scalar shrinkClamp);



   subtype  int32_t is interfaces.Integer_32;
   subtype  int64_t is interfaces.Integer_64;
   subtype uint32_t is interfaces.Unsigned_32;
   subtype uint64_t is interfaces.Unsigned_64;




   --- Point64
   --

   type Point64 is tagged
      record
         x : int64_t;
         y : int64_t;
         z : int64_t;
      end record;




   --- Point32
   --

   type Point32 is tagged
      record
         x, y, z : int32_t;
         index   : Integer;
      end record;

   overriding function  "=" (Self : in Point32;   Other : in Point32) return Boolean;

   function dot (Self : in Point32;   b : in Point32    ) return int64_t;
   function dot (Self : in Point32;   b : in Point64'Class) return int64_t;

   function cross (Self : in Point32'Class;   b : in Point32'Class) return Point64;
   function cross (Self : in Point32'Class;   b : in Point64'Class) return Point64;




   --- Rational64
   --

   type Rational64 is tagged
      record
         numerator,
         denominator : uint64_t;

         sign        : Integer;
      end record;


   function isNaN (Self : in Rational64) return Boolean;
   function compare (Self, b : in Rational64) return Integer;




end impact.d3.convex_hull_Computer;
