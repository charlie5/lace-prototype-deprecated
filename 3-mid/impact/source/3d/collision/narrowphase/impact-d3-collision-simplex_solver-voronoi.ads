with impact.d3.collision.simplex_Solver;



package impact.d3.collision.simplex_Solver.voronoi
--
--  Provides an implementation of the closest point distance algorithm from a 1-4 points simplex to the origin.
--
--  Can be used with GJK, as an alternative to Johnson distance algorithm.
--
is



   --  disable next constant or use defaultCollisionConfiguration->getSimplexSolver()->setEqualVertexThreshold(0.f) to disable/configure
   --
   BT_USE_EQUAL_VERTEX_THRESHOLD : constant Boolean := True;


   VORONOI_DEFAULT_EQUAL_VERTEX_THRESHOLD : constant := 0.0001;




   --- btUsageBitfield
   --

   type btUsageBitfield is
      record
         usedVertexA        : Boolean := False;
         usedVertexB        : Boolean := False;
         usedVertexC        : Boolean := False;
         usedVertexD        : Boolean := False;

         unused1        : Boolean;
         unused2        : Boolean;
         unused3        : Boolean;
         unused4        : Boolean;
      end record;


   for btUsageBitfield use
      record
          usedVertexA        at 0 range 0 .. 0;
          usedVertexB        at 0 range 1 .. 1;
          usedVertexC        at 0 range 2 .. 2;
          usedVertexD        at 0 range 3 .. 3;
          unused1        at 0 range 4 .. 4;
          unused2        at 0 range 5 .. 5;
          unused3        at 0 range 6 .. 6;
          unused4        at 0 range 7 .. 7;
      end record;

   for btUsageBitfield'Size      use 8;
   for btUsageBitfield'Alignment use 1;   -- tbd: check this


   procedure reset (Self : out btUsageBitfield);




   --- btSubSimplexClosestResult
   --

   type btSubSimplexClosestResult is
      record
         m_closestPointOnSimplex : math.Vector_3;
         m_usedVertices          : btUsageBitfield;   -- MASK for m_usedVertices stores the simplex vertex-usage, using the MASK, if m_usedVertices & MASK then the related vertex is used
         m_barycentricCoords     : math.Vector_4;
         m_degenerate            : Boolean;
      end record;


   procedure reset                      (Self : in out btSubSimplexClosestResult);
   procedure setBarycentricCoordinates  (Self :    out btSubSimplexClosestResult;   a, b, c, d : in math.Real := 0.0);

   function  isValid                    (Self : in     btSubSimplexClosestResult) return Boolean;





   ---------------------------
   --- impact.d3.collision.simplex_Solver.voronoi
   --

   type Item is new impact.d3.collision.simplex_Solver.item with
      record
         m_numVertices    : Integer;

         m_simplexVectorW : impact.d3.collision.simplex_Solver.Vector_3_array;
         m_simplexPointsP : impact.d3.collision.simplex_Solver.Vector_3_array;
         m_simplexPointsQ : impact.d3.collision.simplex_Solver.Vector_3_array;

         m_cachedP1,
         m_cachedP2,
         m_cachedV,
         m_lastW                : math.Vector_3;

         m_equalVertexThreshold : math.Real := VORONOI_DEFAULT_EQUAL_VERTEX_THRESHOLD;
         m_cachedValidClosest   : Boolean;

         m_cachedBC             : aliased btSubSimplexClosestResult;
         m_needsUpdate          : Boolean;
      end record;




   overriding procedure reset          (Self : in out Item);
   overriding procedure addVertex      (Self : in out Item;   w, p, q : in math.Vector_3);

   overriding function  closest        (Self : access Item;   v : access math.Vector_3) return Boolean;
   overriding function  maxVertex      (Self : in     Item) return Real;
   overriding function  fullSimplex    (Self : in     Item) return Boolean;
   overriding function  getSimplex     (Self : in     Item;   pBuf, qBuf, yBuf : access impact.d3.collision.simplex_Solver.Vector_3_array) return Integer;
   overriding function  inSimplex      (Self : in     Item;   w : in  math.Vector_3) return Boolean;

   overriding procedure backup_closest (Self : in out Item;   v : out math.Vector_3);

   overriding function  emptySimplex   (Self : in     Item) return Boolean;

   overriding procedure compute_points (Self : in out Item;   p1, p2 : out math.Vector_3);

   overriding function  numVertices    (Self : in     Item) return Integer;



   procedure removeVertex   (Self : in out Item;   Index : in Integer);
   procedure reduceVertices (Self : in out Item;   usedVerts : in btUsageBitfield);

   function  updateClosestVectorAndPoints (Self : access Item) return Boolean;

   function  closestPtPointTetrahedron (Self : in Item;   p           : in     math.Vector_3;
                                                          a, b, c, d  : in     math.Vector_3;
                                                          finalResult : access btSubSimplexClosestResult) return Boolean;

   function  pointOutsideOfPlane (Self : in Item;   p           : in     math.Vector_3;
                                  a, b, c, d  : in     math.Vector_3) return Boolean;

   function  closestPtPointTriangle (Self : in Item;   p           : in     math.Vector_3;
                                                       a, b, c     : in     math.Vector_3;
                                                       Result      : access btSubSimplexClosestResult) return Boolean;


   degenerate_Triangle : exception;



end impact.d3.collision.simplex_Solver.voronoi;











--  #endif //BT_VORONOI_SIMPLEX_SOLVER_H

