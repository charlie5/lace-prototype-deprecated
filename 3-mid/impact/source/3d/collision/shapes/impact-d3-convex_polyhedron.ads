with Ada.Containers.Vectors;

package Impact.d3.convex_Polyhedron
   --
   --
   --
is
   use Math;

   TEST_INTERNAL_OBJECTS : constant Boolean := True;

   package integer_Vectors is new Ada.Containers.Vectors (Positive, Integer);
   subtype Integer_Vector is integer_Vectors.Vector;

   --- Plane
   --

   type Plane is array (1 .. 4) of Math.Real;

   --- btFace
   --

   type btFace is record
      m_indices : Integer_Vector;
      --      btAlignedObjectArray<int>        m_connectedFaces;

      m_plane : Plane;
   end record;

   use type Math.Vector_3;
   package vector_3_Vectors is new Ada.Containers.Vectors (
      Positive,
      Math.Vector_3);
   subtype vector_3_Vector is vector_3_Vectors.Vector;

   package btFace_Vectors is new Ada.Containers.Vectors (Positive, btFace);
   subtype btFace_Vector is btFace_Vectors.Vector;

   --- impact.d3.convex_Polyhedron
   --

   type Item is tagged record
      m_vertices    : vector_3_Vector;
      m_faces       : btFace_Vector;
      m_uniqueEdges : vector_3_Vector;

      m_localCenter : Math.Vector_3;
      m_extents     : Math.Vector_3;
      m_radius      : Math.Real;
      mC            : Math.Vector_3;
      mE            : Math.Vector_3;
   end record;

   procedure destruct (Self : in out Item) is null;

   procedure initialize (Self : in out Item);

   function testContainment (Self : in Item) return Boolean;

   procedure project
     (Self     : in Item;
      trans    : in Transform_3d;
      dir      : in Math.Vector_3;
      min, max : out Math.Real);

end Impact.d3.convex_Polyhedron;
