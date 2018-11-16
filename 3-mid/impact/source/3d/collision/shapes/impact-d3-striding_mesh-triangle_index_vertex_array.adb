--  #include "impact.d3.striding_Mesh.triangle_index_vertex_array.h"



package body impact.d3.striding_Mesh.triangle_index_vertex_array
--
--
--
is


   procedure addIndexedMesh  (Self : in out Item;   mesh : in  btIndexedMesh)
   is
   begin
      Self.m_indexedMeshes.append (mesh);
--        Self.m_indexedMeshes[m_indexedMeshes.size()-1].m_indexType = indexType;
   end addIndexedMesh;





   overriding function getNumSubParts (Self : in Item) return Natural
   is
   begin
      return Natural (Self.m_indexedMeshes.Length);
   end getNumSubParts;






   function  getIndexedMeshArray (Self : access Item) return access IndexedMeshArray
   is
   begin
      return Self.m_indexedMeshes'Access;
   end getIndexedMeshArray;







   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;





   overriding procedure getLockedVertexIndexBase (Self : in out Item;   vertexbase  :    out impact.d3.Containers.real_Pointer;
                                                             stride      : in out Integer;
                                                             indexbase   :    out swig.Pointers.unsigned_Pointer;
                                                             indexstride : in out Integer;
                                                             numfaces    : in out Integer;
                                       subpart     : in     Integer := 0)
   is
      pragma Assert (subpart < Self.getNumSubParts);
      mesh : btIndexedMesh renames Self.m_indexedMeshes (subpart);

   begin
--        numverts     := mesh.m_numVertices;
      vertexbase   := mesh.m_vertexBase;


--        vertexStride := mesh.m_vertexStride;

      numfaces     := mesh.m_numTriangles;

      indexbase    := mesh.m_triangleIndexBase;
      indexstride  := mesh.m_triangleIndexStride;

--          indicestype := mesh.m_indexType;
--     type = mesh.m_vertexType;
   end getLockedVertexIndexBase;








   overriding procedure getLockedReadOnlyVertexIndexBase (Self : in    Item;   vertexbase  :    out impact.d3.Containers.real_Pointer;
                                                                    numverts    :    out Integer;
                                                                    stride      :    out Integer;
                                                                    indexbase   :    out swig.Pointers.unsigned_Pointer; -- unsigned_char_Pointer;
                                                                    indexstride :    out Integer;
                                                                    numfaces    :    out Integer;
                                               subpart     : in     Integer := 0)
   is
      mesh : btIndexedMesh renames Self.m_indexedMeshes (subpart);

   begin
      numverts     := mesh.m_numVertices;
      vertexbase   := mesh.m_vertexBase;

      --     type = mesh.m_vertexType;

      Stride       := mesh.m_vertexStride;

      numfaces     := mesh.m_numTriangles;
      indexbase    := mesh.m_triangleIndexBase;
      indexstride  := mesh.m_triangleIndexStride;
--        indicestype  := mesh.m_indexType;
   end getLockedReadOnlyVertexIndexBase;







   overriding procedure unLockVertexBase         (Self : in out Item;   subpart : in Integer)
   is
      pragma Unreferenced (Self, subpart);
   begin
      return;
   end unLockVertexBase;





   overriding procedure unLockReadOnlyVertexBase (Self : in     Item;   subpart : in Integer)
   is
      pragma Unreferenced (Self, subpart);
   begin
      return;
   end unLockReadOnlyVertexBase;






   overriding procedure setPremadeAabb  (Self : in out Item;   aabbMin, aabbMax : in     math.Vector_3)
   is
   begin
      Self.m_aabbMin := aabbMin;
      Self.m_aabbMax := aabbMax;
      Self.m_hasAabb := True;
   end setPremadeAabb;






   overriding procedure getPremadeAabb  (Self : in     Item;   aabbMin, aabbMax :    out math.Vector_3)
   is
   begin
      aabbMin := Self.m_aabbMin;
      aabbMax := Self.m_aabbMax;
   end getPremadeAabb;






   overriding function  hasPremadeAabb      (Self : in     Item) return Boolean
   is
   begin
      return Self.m_hasAabb;
   end hasPremadeAabb;



end impact.d3.striding_Mesh.triangle_index_vertex_array;





--  Just to be backwards compatible ...
--

--  impact.d3.striding_Mesh.triangle_index_vertex_array::impact.d3.striding_Mesh.triangle_index_vertex_array(int numTriangles,int* triangleIndexBase,int triangleIndexStride,int numVertices,impact.d3.Scalar* vertexBase,int vertexStride)
--  : m_hasAabb(0)
--  {
--          btIndexedMesh mesh;
--
--          mesh.m_numTriangles = numTriangles;
--          mesh.m_triangleIndexBase = (const unsigned char *)triangleIndexBase;
--          mesh.m_triangleIndexStride = triangleIndexStride;
--          mesh.m_numVertices = numVertices;
--          mesh.m_vertexBase = (const unsigned char *)vertexBase;
--          mesh.m_vertexStride = vertexStride;
--
--          addIndexedMesh(mesh);
--
--  }

