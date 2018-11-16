

package impact.d3.collision.simplex_Solver
--
--  impact.d3.collision.simplex_Solver can incrementally calculate distance between origin and up to 4 vertices

--  Used by GJK or Linear Casting. Can be implemented by the Johnson-algorithm or alternative approaches based on
--  voronoi regions or barycentric coordinates.
--
is
   use Math;



   VORONOI_SIMPLEX_MAX_VERTS : constant := 5;

   type Vector_3_array is array (Positive range 1 .. VORONOI_SIMPLEX_MAX_VERTS) of math.Vector_3;



   type Item is interface;


   procedure destruct       (Self : in out Item)                                                        is null;
   procedure reset          (Self : in out Item)                                                        is abstract;
   procedure addVertex      (Self : in out Item;   w, p, q : in Vector_3)                               is abstract;

   function  closest        (Self : access Item;   v : access Vector_3) return Boolean                  is abstract;
   function  maxVertex      (Self : in     Item) return Real                                          is abstract;
   function  fullSimplex    (Self : in     Item) return Boolean                                         is abstract;
   function  getSimplex     (Self : in     Item;   pBuf, qBuf, yBuf : access Vector_3_array) return Integer   is abstract;
   function  inSimplex      (Self : in     Item;   w : in  Vector_3) return Boolean                     is abstract;

   procedure backup_closest (Self : in out Item;   v : out Vector_3)                                    is abstract;

   function  emptySimplex   (Self : in     Item) return Boolean                                         is abstract;

   procedure compute_points (Self : in out Item;   p1, p2 : out Vector_3)                               is abstract;

   function  numVertices    (Self : in     Item) return Integer                                         is abstract;

end impact.d3.collision.simplex_Solver;
