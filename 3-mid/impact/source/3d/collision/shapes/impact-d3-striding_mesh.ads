with interfaces.c;
with impact.d3.triangle_Callback;
with swig.Pointers;
with impact.d3.Containers;

--  #include "LinearMath/impact.d3.Vector.h"
--  #include "impact.d3.triangle_Callback.h"
--  #include "impact.d3.Shape.concave.h"


package impact.d3.striding_Mesh
--
--    The impact.d3.striding_Mesh is the interface class for high performance generic access to triangle meshes, used
--  in combination with impact.d3.Shape.concave.triangle_mesh.bvh and some other collision shapes.
--
--    Using index striding of 3*sizeof(integer) it can use triangle arrays, using index striding of 1*sizeof(integer) it can handle triangle strips.
--
--    It allows for sharing graphics and collision meshes. Also it provides locking/unlocking of graphics meshes that are in gpu memory.
--
is


   type Item is abstract tagged private;
   type View is access all Item'Class;


   procedure destruct (Self : in out Item);



   procedure InternalProcessAllTriangles (Self : in    Item;   callback         : access impact.d3.triangle_Callback.btInternalTriangleIndexCallback'Class;
                                                               aabbMin, aabbMax : in     math.Vector_3);


   procedure calculateAabbBruteForce     (Self : in    Item;   aabbMin, aabbMax :    out     math.Vector_3);
   --
   --  Brute force method to calculate aabb.



--     type unsigned_char_Pointer  is access all interfaces.c.unsigned_char;
--     type unsigned_char_Pointers is array (Positive range <>) of unsigned_char_Pointer;



   procedure getLockedVertexIndexBase    (Self : in out Item;   vertexbase  :    out impact.d3.Containers.real_Pointer;
                                                                stride      : in out Integer;
                                                                indexbase   :    out swig.Pointers.unsigned_Pointer;
                                                                indexstride : in out Integer;
                                                                numfaces    : in out Integer;
                                                                subpart     : in     Integer := 0)
   is abstract;
   --
   --    Get read and write access to a subpart of a triangle mesh.
   --
   --    This subpart has a continuous array of vertices and indices
   --  in this way the mesh can be handled as chunks of memory with striding
   --  very similar to OpenGL vertexarray support
   --  make a call to unLockVertexBase when the read and write access is finished.

   --                  virtual void        getLockedVertexIndexBase(unsigned char **vertexbase, int& numverts,                   PHY_ScalarType& type, int& stride,
   --                                                    unsigned char **indexbase,  int& indexstride, int& numfaces, PHY_ScalarType& indicestype,int subpart=0)=0;




   procedure getLockedReadOnlyVertexIndexBase (Self : in    Item;   vertexbase  :    out impact.d3.Containers.real_Pointer;
                                                                    numverts    :    out Integer;
                                                                    stride      :    out Integer;
                                                                    indexbase   :    out swig.Pointers.unsigned_Pointer; -- unsigned_char_Pointer;
                                                                    indexstride :    out Integer;
                                                                    numfaces    :    out Integer;
                                                                    subpart     : in     Integer := 0)
   is abstract;

   --                  virtual void        getLockedReadOnlyVertexIndexBase (const unsigned char **vertexbase, int& numverts,PHY_ScalarType& type, int& stride,
   --                                                             const unsigned char **indexbase,int & indexstride,int& numfaces,
   --                                                             PHY_ScalarType& indicestype,int subpart=0) const=0;



   procedure unLockVertexBase (Self : in out Item;   subpart : in Integer)
   is abstract;
   --
   --  Finishes the access to a subpart of the triangle mesh.
   --  Make a call to unLockVertexBase when the read and write access (using getLockedVertexIndexBase) is finished.



   procedure unLockReadOnlyVertexBase (Self : in Item;   subpart : in Integer)
   is abstract;



   function getNumSubParts (Self : in Item) return Natural
                            is abstract;
   --
   --  getNumSubParts returns the number of seperate subparts.
   --  Each subpart has a continuous array of vertices and indices.



   procedure preallocateVertices (Self : in out Item;   numverts : in Integer)
   is abstract;

   procedure preallocateIndices  (Self : in out Item;   numindices : in Integer)
   is abstract;



   function hasPremadeAabb (Self : in Item) return Boolean;

   --                  virtual bool        hasPremadeAabb() const { return false; }



   procedure setPremadeAabb  (Self : in out Item;   aabbMin, aabbMax : in     math.Vector_3)   is null;
   procedure getPremadeAabb  (Self : in     Item;   aabbMin, aabbMax :    out math.Vector_3)   is null;



   function  getScaling (Self : in     Item)         return math.Vector_3;
   procedure setScaling (Self :    out Item;   scaling : in math.Vector_3);

--                  const impact.d3.Vector&        getScaling() const {
--                          return m_scaling;
--                  }


--                  void        setScaling(const impact.d3.Vector& scaling)
--                  {
--                          m_scaling = scaling;
--                  }




   type btIntIndexData is
      record
         m_value : Integer;
      end record;





   type btShortIntIndexData is
      record
         m_value : Short_Integer;
         m_pad   : String (1 .. 2);
      end record;




   type short_Integers is array (Positive range <>) of Short_Integer;

   type btShortIntIndexTripletData is
      record
         m_values : short_Integers (1 .. 3);
         m_pad    : String         (1 .. 2);

      end record;




   type unsigned_Chars is array (Positive range <>) of interfaces.C.unsigned_char;

   type btCharIndexTripletData is
      record
         m_values : unsigned_Chars (1 .. 3);
         m_pad    : Character;
      end record;







private

   type Item is abstract tagged
      record
         m_scaling : math.Vector_3 := (1.0, 1.0, 1.0);
      end record;



end impact.d3.striding_Mesh;




