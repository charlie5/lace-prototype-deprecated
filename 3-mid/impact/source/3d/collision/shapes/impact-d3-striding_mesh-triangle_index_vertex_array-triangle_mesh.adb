with ada.Unchecked_Deallocation;
with impact.d3.striding_Mesh.triangle_index_vertex_array;
with impact.d3.Vector;
with Interfaces.C;
--  #include "impact.d3.striding_Mesh.triangle_index_vertex_array.triangle_mesh.h"



package body impact.d3.striding_Mesh.triangle_index_vertex_array.triangle_mesh
--
--
--
is


   function to_striding_Mesh return Item
   is
      Self      : aliased Item;
      meshIndex : impact.d3.striding_Mesh.triangle_index_vertex_array.btIndexedMesh;

   begin
      Self.m_weldingThreshold := 0.0;

      meshIndex.m_numTriangles        := 0;
      meshIndex.m_numVertices         := 0;
--        meshIndex.m_indexType           := PHY_INTEGER;
      meshIndex.m_triangleIndexBase   := null;
      meshIndex.m_triangleIndexStride := 3 * Integer'Size/8;
      meshIndex.m_vertexBase          := null;
      meshIndex.m_vertexStride        := math.Vector_3'Size / 8;

      Self.getIndexedMeshArray.append (meshIndex);

      Self.getIndexedMeshArray.all (1).m_numTriangles        := Self.m_indices_Count / 3;
      Self.getIndexedMeshArray.all (1).m_triangleIndexBase   := null;
--        Self.m_indexedMeshes (1).m_indexType           := PHY_INTEGER;
      Self.getIndexedMeshArray.all (1).m_triangleIndexStride := 3 * Integer'Size/8;

      Self.getIndexedMeshArray.all (1).m_numVertices  := Self.m_vertex_Count;
      Self.getIndexedMeshArray.all (1).m_vertexBase   := null;
      Self.getIndexedMeshArray.all (1).m_vertexStride := math.Vector_3'Size / 8;


      return Self;
   end to_striding_Mesh;






   procedure addTriangle (Self : in out Item;   vertex0, vertex1, vertex2 : in math.Vector_3;
                                                removeDuplicateVertices   : in Boolean      := False)
   is
   begin
      Self.getIndexedMeshArray.all (1).m_numTriangles := Self.getIndexedMeshArray.all (1).m_numTriangles + 1;

      Self.addIndex (Self.findOrAddVertex (vertex0, removeDuplicateVertices));
      Self.addIndex (Self.findOrAddVertex (vertex1, removeDuplicateVertices));
      Self.addIndex (Self.findOrAddVertex (vertex2, removeDuplicateVertices));
   end addTriangle;






   function getNumTriangles (Self : in     Item) return Natural
   is
   begin
      return Self.m_indices_Count / 3;
   end getNumTriangles;







   function  findOrAddVertex (Self : access Item;   vertex                  : in math.Vector_3;
                                                    removeDuplicateVertices : in Boolean    ) return Natural
   is
      use impact.d3.Vector, Math;
   begin
      --  return index of new/existing vertex

      --  todo: could use acceleration structure for this

      if removeDuplicateVertices then

         for i in 1 .. Self.m_vertex_Count
         loop
            if length2 (to_Math (Self.m_4componentVertices (i)) - vertex)  <=  Self.m_weldingThreshold then
               return i;
            end if;
         end loop;

      end if;


      Self.getIndexedMeshArray.all (1).m_numVertices := Self.getIndexedMeshArray.all (1).m_numVertices + 1;

      Self.m_vertex_Count                             := Self.m_vertex_Count + 1;
      Self.m_4componentVertices (Self.m_vertex_Count) := c_Vector_3 (vertex);

      Self.getIndexedMeshArray.all (1).m_vertexBase := Self.m_4componentVertices (1)(1)'Access;


      return Self.m_vertex_Count - 1;
   end findOrAddVertex;





   procedure addIndex        (Self : in out Item;   index : in Integer)
   is
   begin
      Self.m_indices_Count := Self.m_indices_Count + 1;

      if Self.m_indices_Count > Self.m_32bitIndices'Length then
         declare
            procedure free is new ada.Unchecked_Deallocation (Containers.Unsigneds, access_Unsigneds);

            new_Indices : constant access_Unsigneds := new Containers.Unsigneds (1 .. 2 * Self.m_32bitIndices'Length);

         begin
            new_Indices (Self.m_32bitIndices'Range) := Self.m_32bitIndices.all;
            free (Self.m_32bitIndices);
            Self.m_32bitIndices                     := new_Indices;
         end;
      end if;


      Self.m_32bitIndices (Self.m_indices_Count)           := Interfaces.c.unsigned (index);

      Self.getIndexedMeshArray.all (1).m_triangleIndexBase := Self.m_32bitIndices (1)'Access;
   end addIndex;



end impact.d3.striding_Mesh.triangle_index_vertex_array.triangle_mesh;
