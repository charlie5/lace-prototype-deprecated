with impact.d3.Containers;


--  #include "impact.d3.striding_Mesh.triangle_index_vertex_array.h"
--  #include "LinearMath/impact.d3.Vector.h"
--  #include "LinearMath/btAlignedObjectArray.h"





package impact.d3.striding_Mesh.triangle_index_vertex_array.triangle_mesh
--
--  The impact.d3.striding_Mesh.triangle_index_vertex_array.triangle_mesh class is a convenience class derived from impact.d3.striding_Mesh.triangle_index_vertex_array, that provides storage for a
--  concave triangle mesh. It can be used as data for the impact.d3.Shape.concave.triangle_mesh.bvh.
--
--  It allows either 32bit or 16bit indices, and 4 (x-y-z-w) or 3 (x-y-z) component vertices.
--
--  If you want to share triangle/index data between graphics mesh and collision mesh (impact.d3.Shape.concave.triangle_mesh.bvh), you can
--  directly use impact.d3.striding_Mesh.triangle_index_vertex_array or derive your own class from impact.d3.striding_Mesh.
--
--  Performance of impact.d3.striding_Mesh.triangle_index_vertex_array.triangle_mesh and impact.d3.striding_Mesh.triangle_index_vertex_array used in a impact.d3.Shape.concave.triangle_mesh.bvh is the same.
--
is


   type Item is new impact.d3.striding_Mesh.triangle_index_vertex_array.item with private;



   function to_striding_Mesh return Item;



   procedure addTriangle (Self : in out Item;   vertex0, vertex1, vertex2 : in math.Vector_3;
                                                removeDuplicateVertices   : in Boolean      := False);
   --
   --  By default addTriangle won't search for duplicate vertices, because the search is very slow for large triangle meshes.
   --  In general it is better to directly use impact.d3.striding_Mesh.triangle_index_vertex_array instead.


   function getNumTriangles (Self : in     Item) return Natural;






   --- Internal methods ~ use addTriangle instead.

   function  findOrAddVertex (Self : access Item;   vertex                  : in math.Vector_3;
                                                    removeDuplicateVertices : in Boolean    ) return Natural;

   procedure addIndex        (Self : in out Item;   index : in Integer);








private


   type access_Vector_3_array is access c_Vector_3_array;
   type access_Unsigneds      is access Containers.Unsigneds;


   type Item is new impact.d3.striding_Mesh.triangle_index_vertex_array.Item with
      record
         m_weldingThreshold      : math.Real;

--           m_4componentVertices    : bullet.Containers.vector_3_Vector;
         m_4componentVertices    : access_Vector_3_array;
         m_vertex_Count          : Natural := 0;

--           m_3componentVertices    : btAlignedObjectArray<float>;

         m_32bitIndices          : access_Unsigneds;
         m_indices_Count         : Natural := 0;
--           m_32bitIndices          : aliased bullet.Containers.integer_Vector;
--  --           m_16bitIndices          : btAlignedObjectArray<unsigned short int>;

--           m_use32bitIndices,
--           m_use4componentVertices : Boolean
      end record;


end impact.d3.striding_Mesh.triangle_index_vertex_array.triangle_mesh;
