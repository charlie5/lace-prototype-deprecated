with Impact.d3.Containers;
with Impact.d3.convex_Polyhedron;
with Impact.d3.Collision.Detector.discrete;

package Impact.d3.Collision.polyhedral_contact_Clipping
   --
   --  Clips a face to the back of a plane
   --
is
   use Math;

   subtype btVertexArray is Containers.vector_3_Vector;

   procedure clipHullAgainstHull
     (separatingNormal1 : in Math.Vector_3;
      hullA, hullB      : in Impact.d3.convex_Polyhedron.Item'Class;
      transA, transB    : in Transform_3d;
      minDist, maxDist  : in Math.Real;
      resultOut         : out Impact.d3.Collision.Detector.discrete.Result'
     Class);

   procedure clipFaceAgainstHull
     (separatingNormal : in Math.Vector_3;
      hullA            : in Impact.d3.convex_Polyhedron.Item'Class;
      transA           : in Transform_3d;
      worldVertsB1     : in out btVertexArray;
      minDist, maxDist : in Math.Real;
      resultOut        : out Impact.d3.Collision.Detector.discrete.Result'
     Class);

   function findSeparatingAxis
     (hullA, hullB   : in Impact.d3.convex_Polyhedron.Item'Class;
      transA, transB : in Transform_3d;
      sep            : out Math.Vector_3)
      return           Boolean;

   procedure clipFace
     (pVtxIn        : in btVertexArray;
      ppVtxOut      : out btVertexArray;
      planeNormalWS : in Math.Vector_3;
      planeEqWS     : in Math.Real);
   --
   --  the clipFace method is used internally

end Impact.d3.Collision.polyhedral_contact_Clipping;
