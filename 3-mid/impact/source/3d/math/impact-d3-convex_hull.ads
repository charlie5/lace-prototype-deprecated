with ada.Containers.Vectors;
with impact.d3.Containers;



package impact.d3.convex_Hull
--
--
--
is


   --- TUIntArray
   --

--     subtype TUIntArray is bullet.Containers.Unsigned_Vector;
   subtype TUIntArray is Containers.integer_Vector;

   type access_vector_3_Array is access vector_3_Array;




   --  HullFlag
   --
   type HullFlag is new Flags;

   QF_TRIANGLES     : constant HullFlag := 1;             -- report results as triangles, not polygons.
   QF_REVERSE_ORDER : constant HullFlag := 2;             -- reverse order of the triangle indices.
   QF_DEFAULT       : constant HullFlag := QF_TRIANGLES;




   --- HullError
   --

   type HullError is (QE_OK, QE_FAIL);






   --- HullResult
   --

   type HullResult is tagged
      record
         mPolygons          : Boolean := True;                            -- True if indices represents polygons, false indices are triangles.

         mNumOutputVertices : Natural := 0;                               -- Number of vertices in the output hull.
         m_OutputVertices   : access_Vector_3_Array;                      -- Array of vertices.

         mNumIndices        : Natural := 0;                               -- The total number of indices.
         m_Indices          : Containers.Integer_Vector;           -- pointer to indices.

         mNumFaces          : Natural := 0;                               -- The number of faces produced.
      end record;
   --
   --  If triangles, then indices are array indexes into the vertex list.
   --  If polygons,  then indices are in the form (number of points in face) (p1, p2, p3, ..) etc.






   --- HullDesc
   --

   type Vector_3_view is access all math.Vector_3;

   type HullDesc is tagged
      record
        mFlags         : HullFlag;                      -- flags to use when generating the convex hull.
        mVcount        : Natural;                       -- number of vertices in the input point cloud
        mVertices      : access vector_3_Array;  -- the array of vertices.
        mVertexStride  : Positive;                      -- the stride of each vertex, in bytes.
        mNormalEpsilon : math.Real;                     -- the epsilon for removing duplicates.  This is a normalized value, if normalized bit is on.
        mMaxVertices   : Natural;                       -- maximum number of vertices to be considered for the hull!
        mMaxFaces      : Natural;
      end record;


   function to_HullDesc return HullDesc;

--          HullDesc(void)
--          {
--                  mFlags          = QF_DEFAULT;
--                  mVcount         = 0;
--                  mVertices       = 0;
--                  mVertexStride   = sizeof(impact.d3.Vector);
--                  mNormalEpsilon  = 0.001f;
--                  mMaxVertices        = 4096; // maximum number of points to be considered for a convex hull.
--                  mMaxFaces        = 4096;
--          };



   function to_HullDesc (flag     : in     HullFlag;
                         vcount   : in     Natural;
                         vertices : access vector_3_Array;
                         stride   : in     Positive := math.Vector_3'Size / 8) return HullDesc;

--          HullDesc(HullFlag flag,
--                   unsigned int vcount,
--                   const impact.d3.Vector *vertices,
--                   unsigned int stride = sizeof(impact.d3.Vector))
--          {
--                  mFlags          = flag;
--                  mVcount         = vcount;
--                  mVertices       = vertices;
--                  mVertexStride   = stride;
--                  mNormalEpsilon  = impact.d3.Scalar(0.001);
--                  mMaxVertices    = 4096;
--          }





   function HasHullFlag (Self : in HullDesc;   Flag : in HullFlag) return Boolean;

--          bool HasHullFlag(HullFlag flag) const
--          {
--                  if ( mFlags & flag ) return true;
--                  return false;
--          }



   procedure SetHullFlag (Self : in out  HullDesc;   Flag : in HullFlag);

--          void SetHullFlag(HullFlag flag)
--          {
--                  mFlags|=flag;
--          }




   procedure ClearHullFlag (Self : in out  HullDesc;   Flag : in HullFlag);

--          void ClearHullFlag(HullFlag flag)
--          {
--                  mFlags&=~flag;
--          }





   --- btPlane
   --

   type btPlane is tagged
      record
         normal : math.Vector_3;
         dist   : math.Real;        -- distance below origin - the D from plane equasion Ax+By+Cz+D=0
      end record;



   function to_btPlane return btPlane;

   --                          btPlane() : normal(), dist(0){}



   function to_btPlane (n : in math.Vector_3;
                        d : in math.Real  ) return btPlane;

--                          btPlane(const impact.d3.Vector &n,impact.d3.Scalar d) : normal(n), dist(d){}


   package btPlane_Vectors is new ada.Containers.Vectors (Positive, btPlane);





   --- HalfEdge
   --

   type HalfEdge is tagged
      record
         ea : Integer;          -- the other half of the edge (index into edges list)
         v  : Integer;          -- the vertex at the start of this edge (index into vertices list)
         p  : Integer;          -- the facet on which this edge lies (index into facets list)
      end record;


   function to_HalfEdge (ea : Integer;
                         v  : Integer;
                         p  : Integer) return HalfEdge;

   --  HalfEdge(short _ea,unsigned char _v, unsigned char _p):ea(_ea),v(_v),p(_p){}


   package HalfEdge_Vectors is new ada.Containers.Vectors (Positive, HalfEdge);





   --- ConvexH
   --

   type ConvexH is tagged
      record
        vertices : Containers.Vector_3_Vector;
        edges    : HalfEdge_Vectors .Vector;
        facets   : btPlane_Vectors  .Vector;
      end record;


   function to_ConvexH (vertices_size,
                        edges_size,
                        facets_size : in Positive) return ConvexH;






   --- int4
   --

   type int4 is tagged
      record
         x, y, z, w : aliased Integer;
      end record;


   function to_int4 (x, y, z, w : in Integer) return int4;

--          int4(int _x,int _y, int _z,int _w){x=_x;y=_y;z=_z;w=_w;}


   function Element (Self : in int4;   i : in Integer) return Integer;

--          const int& operator[](int i) const {return (&x)[i];}



   function Element (Self : access int4;   i : in Integer) return access Integer;

--          int& operator[](int i) {return (&x)[i];}








   --- PHullResult
   --

   type PHullResult is tagged
      record
         mFaceCount  : Natural := 0;

         mVcount     : Natural := 0;
         mVertices   : access Vector_3_array;

         mIndexCount : Natural := 0;
         m_Indices   : aliased TUIntArray;
      end record;








   ----------------
   --- HullLibrary
   --

   --  The HullLibrary class can create a convex hull from a collection of vertices, using the ComputeHull method.
   --  The impact.d3.shape_Hull class uses this HullLibrary to create a approximate convex mesh given a general (non-polyhedral) convex shape.

   type HullLibrary is tagged private;



   function m_vertexIndexMapping (Self : access HullLibrary) return access Containers.Integer_Vector;



   function CreateConvexHull (Self : access HullLibrary;   desc   : in     HullDesc  'Class;     -- describes the input request
                                                           result : access HullResult'Class)     -- contains the result
                              return HullError;


   function ReleaseResult    (Self : in HullLibrary;   result : access HullResult'Class)     -- release memory allocated for this result, we are done with it.
                              return HullError;







private


   --- int3
   --

   type int3 is tagged
      record
         x, y, z : aliased Integer;
      end record;

   package int3_Forge
   is
      function to_int3 (x, y, z : in Integer) return int3;
   end int3_Forge;

   function Element (Self : in     int3;   i : in Integer) return        Integer;
   function Element (Self : access int3;   i : in Integer) return access Integer;








   --- btHullTriangle;
   --

   type btHullTriangle is new int3 with
      record
         n    : aliased int3;
         id   : Integer;
         vmax : Integer;
         rise : math.Real;
      end record;

   type btHullTriangle_view is access all btHullTriangle'Class;


   procedure destruct (Self : in out btHullTriangle) is null;


   function to_btHullTriangle (a, b, c : in Integer) return btHullTriangle;

--          btHullTriangle(int a,int b,int c):int3(a,b,c),n(-1,-1,-1)
--          {
--                  vmax=-1;
--                  rise = impact.d3.Scalar(0.0);
--          }



   function neib (Self : access btHullTriangle;   a, b : in Integer) return access Integer;

--  int &btHullTriangle::neib(int a,int b)
--  {
--          static int er=-1;
--          int i;
--          for(i=0;i<3;i++)
--          {
--                  int i1=(i+1)%3;
--                  int i2=(i+2)%3;
--                  if((*this)[i]==a && (*this)[i1]==b) return n[i2];
--                  if((*this)[i]==b && (*this)[i1]==a) return n[i2];
--          }
--          btAssert(0);
--          return er;
--  }



   package btHullTriangle_Vectors is new ada.Containers.Vectors (Positive, btHullTriangle_view);





   --- HullLibrary
   --

   type HullLibrary is tagged
      record
        m_tris               :         btHullTriangle_Vectors.Vector;
        m_vertexIndexMapping : aliased Containers.Integer_Vector;
      end record;



   function ComputeHull (Self : access HullLibrary;   vcount       : in     Natural        ;
                                                      vertices     : access Vector_3_array  ;
                                                      result       : access PHullResult'Class;
                                                      vlimit       : in     Natural        ) return Boolean;



   function  allocateTriangle   (Self : access HullLibrary;   a, b, c      : in Integer          ) return btHullTriangle_view;
   procedure deAllocateTriangle (Self : in out HullLibrary;   the_Triangle : in out btHullTriangle_view);
   procedure b2bfix             (Self : in out HullLibrary;   s, t         : in btHullTriangle_view);
   procedure removeb2b          (Self : in out HullLibrary;   s, t         : in out btHullTriangle_view);




   procedure checkit            (Self : in out HullLibrary;   t            : in btHullTriangle_view);


   function  extrudable         (Self : in     HullLibrary;   epsilon      : in math.Real        ) return btHullTriangle_view;


   function  calchull    (Self : access HullLibrary;   verts        : access Vector_3_array;
                                                       verts_count  : in     Natural;
                                                       tris_out     : access TUIntArray;
                                                       tris_count   : access Natural;
                                                       vlimit       : in     Integer      ) return Integer;


   function  calchullgen (Self : access HullLibrary;   verts        : access Vector_3_array;
                                                       verts_count  : in     Natural;
                                                       the_vlimit   : in     Natural    ) return Integer;


   function  FindSimplex        (Self : in     HullLibrary;   verts        : access Vector_3_array;
                                                              verts_count  : in     Natural;
                                                              allow        : access Containers.integer_Vector) return int4'Class;

--     function  ConvexHCrop        (Self : in     HullLibrary;   convex       : access ConvexH'Class;
--                                                                slice        : in     btPlane'Class) return access ConvexH'Class;


   procedure extrude (Self : in out HullLibrary;   t0           : in out btHullTriangle_view;
                                                   v            : in     Integer);

--     function  test_cube          (Self : in     HullLibrary) return access ConvexH'Class;


   procedure BringOutYourDead (Self : in out HullLibrary;   verts        : access Vector_3_Array;
                                                            vcount       : in     Natural;
                                                            overts       : access Vector_3_Array;
                                                            ocount       :    out Natural;
                                                            indices      : access Containers.integer_Vector;
                                                            indexcount   : in     Natural);
   --
   --          BringOutYourDead (John Ratcliff): When you create a convex hull you hand it a large input set of vertices forming a 'point cloud'.
   --
   --          After the hull is generated it give you back a set of polygon faces which index the *original* point cloud.

   --          The thing is, often times, there are many 'dead vertices' in the point cloud that are on longer referenced by the hull.

   --          The routine 'BringOutYourDead' find only the referenced vertices, copies them to an new buffer, and re-indexes the hull
   --   so that it is a minimal representation.



   function  CleanupVertices (Self : access HullLibrary;   svcount       : in     Natural;
                                                           svertices     : access vector_3_Array;
                                                           stride        : in     Integer;

                                                           vcount        : access Natural;                    -- output number of vertices
                                                           vertices      : access vector_3_Array;      -- location to store the results

                                                           normalepsilon : in     math.Real;
                                                           scale         : access math.Vector_3) return Boolean;

end impact.d3.convex_Hull;
