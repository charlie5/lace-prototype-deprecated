with ada.Containers.Vectors;
with impact.d3.striding_Mesh;
with swig.Pointers;
with impact.d3.Containers;



package impact.d3.striding_Mesh.triangle_index_vertex_array
--
--  The impact.d3.striding_Mesh.triangle_index_vertex_array allows to access multiple triangle meshes, by indexing into existing triangle/index arrays.
--
--  Additional meshes can be added using addIndexedMesh.
--
--  No duplcate is made of the vertex/index data, it only indexes into external vertex/index arrays,
--  so keep those arrays around during the lifetime of this impact.d3.striding_Mesh.triangle_index_vertex_array.
is

   type Item is new impact.d3.striding_Mesh.item with private;





--     type unsigned_char_pointer is access all interfaces.c.unsigned_char;



   --  The btIndexedMesh indexes a single vertex and index array. Multiple btIndexedMesh objects can be passed into a impact.d3.striding_Mesh.triangle_index_vertex_array using addIndexedMesh.
   --  Instead of the number of indices, we pass the number of triangles.

   type btIndexedMesh is
      record
         m_numTriangles        : Integer;
         m_triangleIndexBase   : swig.Pointers.unsigned_Pointer;
         m_triangleIndexStride : Integer;

         m_numVertices         : Integer;
         m_vertexBase          : impact.d3.Containers.real_Pointer;
--           m_vertexBase          : Integer;    -- index
         m_vertexStride        : Integer;

         --  The index type is set when adding an indexed mesh to the
         --  impact.d3.striding_Mesh.triangle_index_vertex_array, do not set it manually
         --
--           m_indexType : PHY_ScalarType := PHY_INTEGER;

         --  The vertex type has a default type similar to Bullet's precision mode (float or double)
         --  but can be set manually if you for example run Bullet with double precision but have
         --  mesh data in single precision.
         --
--           m_vertexType : PHY_ScalarType := PHY_DOUBLE;
      end record;



   package btIndexedMesh_Vectors is new ada.Containers.Vectors (Positive, btIndexedMesh);
   subtype IndexedMeshArray      is btIndexedMesh_Vectors.Vector;







   -------------------------------
   --- impact.d3.striding_Mesh.triangle_index_vertex_array
   --

   overriding procedure destruct (Self : in out Item);




   overriding procedure getLockedVertexIndexBase (Self : in out Item;   vertexbase  :    out impact.d3.Containers.real_Pointer;
                                                             stride      : in out Integer;
                                                             indexbase   :    out swig.Pointers.unsigned_Pointer;
                                                             indexstride : in out Integer;
                                                             numfaces    : in out Integer;
                                       subpart     : in     Integer := 0);


   overriding procedure getLockedReadOnlyVertexIndexBase (Self : in    Item;   vertexbase  :    out impact.d3.Containers.real_Pointer;
                                                                    numverts    :    out Integer;
                                                                    stride      :    out Integer;
                                                                    indexbase   :    out swig.Pointers.unsigned_Pointer; -- unsigned_char_Pointer;
                                                                    indexstride :    out Integer;
                                                                    numfaces    :    out Integer;
                                                                    subpart     : in     Integer := 0);


   overriding procedure unLockVertexBase         (Self : in out Item;   subpart : in Integer);
   overriding procedure unLockReadOnlyVertexBase (Self : in     Item;   subpart : in Integer);


   overriding function  getNumSubParts (Self : in Item) return Natural;



   overriding procedure preallocateVertices (Self : in out Item;   numverts   : in Integer)   is null;
   overriding procedure preallocateIndices  (Self : in out Item;   numindices : in Integer)   is null;




   overriding procedure setPremadeAabb  (Self : in out Item;   aabbMin, aabbMax : in     math.Vector_3);
   overriding procedure getPremadeAabb  (Self : in     Item;   aabbMin, aabbMax :    out math.Vector_3);






   procedure addIndexedMesh      (Self : in out Item;   mesh : in  btIndexedMesh);
   function  getIndexedMeshArray (Self : access Item) return access IndexedMeshArray;
   overriding function  hasPremadeAabb      (Self : in     Item) return Boolean;







private

   type Item is new impact.d3.striding_Mesh.item with
      record
         m_indexedMeshes : aliased IndexedMeshArray;

         m_aabbMin,
         m_aabbMax       : math.Vector_3;

         m_hasAabb       : Boolean := False;
      end record;







end impact.d3.striding_Mesh.triangle_index_vertex_array;


--          //just to be backwards compatible
--          impact.d3.striding_Mesh.triangle_index_vertex_array(int numTriangles,int* triangleIndexBase,int triangleIndexStride,int numVertices,impact.d3.Scalar* vertexBase,int vertexStride);
