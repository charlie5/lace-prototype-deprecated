with impact.d3.Containers;
with impact.d3.Shape.convex;
with Interfaces.C;



package impact.d3.shape_Hull
--
--  The impact.d3.shape_Hull class takes a impact.d3.Shape.convex, builds a simplified convex hull using impact.d3.convex_Hull and provides triangle indices and vertices.
--
--  It can be useful for to simplify a complex convex object and for visualization of a non-polyhedral convex object.
--
--  It approximates the convex hull using the supporting vertex of 42 directions.
--
is

   type Item is tagged private;



   --- Forge
   --

   function  to_shape_Hull (shape : in impact.d3.Shape.convex.view) return Item;

   procedure destruct (Self : in out Item);




   --- Attributes
   --

   function  buildHull        (Self : access Item;   margin : in math.Real) return Boolean;

   function  numTriangles     (Self : in     Item) return Natural;
   function  numVertices      (Self : in     Item) return Natural;
   function  numIndices       (Self : in     Item) return Natural;

   function  getVertexPointer (Self : in     Item) return access math.Vector_3;
   function  getIndexPointer (Self : in Item) return access Interfaces.C.Unsigned;






private

   type access_vector_3_array is access Vector_3_array;
   type access_Unsigneds      is access Containers.Unsigneds;



   type Item is tagged
      record
         m_vertices   : access_vector_3_array;
         m_indices    : access_Unsigneds;

         m_numIndices : Natural;
         m_shape      : impact.d3.Shape.convex.view;
      end record;

end impact.d3.shape_Hull;
