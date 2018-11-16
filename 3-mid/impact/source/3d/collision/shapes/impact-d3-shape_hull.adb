with ada.unchecked_Deallocation;
with impact.d3.convex_Hull;



package body impact.d3.shape_Hull
is


   ------------
   --- Globals
   --

   NUM_UNITSPHERE_POINTS : constant := 42;


   procedure free is new ada.unchecked_Deallocation (vector_3_array,       access_Vector_3_Array);
   procedure free is new ada.unchecked_Deallocation (Containers.unsigneds, access_Unsigneds);



   ------------------------
   --- getUnitSpherePoints
   --

   getUnitSpherePoints : array (1 .. NUM_UNITSPHERE_POINTS + 2*impact.d3.Shape.convex.MAX_PREFERRED_PENETRATION_DIRECTIONS) of math.Vector_3
     := ((0.000000, -0.000000, -1.000000),
         (0.723608, -0.525725, -0.447219),
         (-0.276388, -0.850649, -0.447219),
         (-0.894426, -0.000000, -0.447216),
         (-0.276388,  0.850649, -0.447220),
         (0.723608,  0.525725, -0.447219),
         (0.276388, -0.850649,  0.447220),
         (-0.723608, -0.525725,  0.447219),
         (-0.723608,  0.525725,  0.447219),
         (0.276388,  0.850649,  0.447219),
         (0.894426,  0.000000,  0.447216),
         (-0.000000,  0.000000,  1.000000),
         (0.425323, -0.309011, -0.850654),
         (-0.162456, -0.499995, -0.850654),
         (0.262869, -0.809012, -0.525738),
         (0.425323,  0.309011, -0.850654),
         (0.850648, -0.000000, -0.525736),
         (-0.525730, -0.000000, -0.850652),
         (-0.688190, -0.499997, -0.525736),
         (-0.162456,  0.499995, -0.850654),
         (-0.688190,  0.499997, -0.525736),
         (0.262869,  0.809012, -0.525738),
         (0.951058,  0.309013,  0.000000),
         (0.951058, -0.309013,  0.000000),
         (0.587786, -0.809017,  0.000000),
         (0.000000, -1.000000,  0.000000),
         (-0.587786, -0.809017,  0.000000),
         (-0.951058, -0.309013, -0.000000),
         (-0.951058,  0.309013, -0.000000),
         (-0.587786,  0.809017, -0.000000),
         (-0.000000,  1.000000, -0.000000),
         (0.587786,  0.809017, -0.000000),
         (0.688190, -0.499997,  0.525736),
         (-0.262869, -0.809012,  0.525738),
         (-0.850648,  0.000000,  0.525736),
         (-0.262869,  0.809012,  0.525738),
         (0.688190,  0.499997,  0.525736),
         (0.525730,  0.000000,  0.850652),
         (0.162456, -0.499995,  0.850654),
         (-0.425323, -0.309011,  0.850654),
         (-0.425323,  0.309011,  0.850654),
         (0.162456,  0.499995,  0.850654),
         others => <>);




   ----------
   --- Forge
   --


   function  to_shape_Hull (shape : in impact.d3.Shape.convex.view) return Item
   is
      Self : Item;
   begin
      Self.m_shape      := shape;
      Self.m_numIndices := 0;

      return Self;
   end to_shape_Hull;




   procedure destruct (Self : in out Item)
   is
   begin
      free (Self.m_indices);
      free (Self.m_vertices);
   end destruct;






   ---------------
   --- Attributes
   --

   function buildHull    (Self : access Item;   margin : in math.Real) return Boolean
   is
      pragma Unreferenced (margin);
      use Interfaces;
      use type impact.d3.convex_Hull.HullError;

      numSampleDirections : Integer      := NUM_UNITSPHERE_POINTS;

      numPDA              : constant Integer      := Self.m_shape.getNumPreferredPenetrationDirections;
      norm                : math.Vector_3;

      supportPoints       : aliased vector_3_array := (1 .. NUM_UNITSPHERE_POINTS + 2*impact.d3.Shape.convex.MAX_PREFERRED_PENETRATION_DIRECTIONS => <>);

      hd                  :         impact.d3.convex_Hull.HullDesc;
      hl                  : aliased impact.d3.convex_Hull.HullLibrary;
      hr                  : aliased impact.d3.convex_Hull.HullResult;

      unused : impact.d3.convex_Hull.hullError;
      pragma Unreferenced (unused);

   begin
      if numPDA > 0 then

         for i in 1 .. numPDA
         loop
            Self.m_shape.getPreferredPenetrationDirection (i, norm);

            getUnitSpherePoints (numSampleDirections) := norm;
            numSampleDirections                       := numSampleDirections + 1;
         end loop;

      end if;


      for i in 1 .. numSampleDirections
      loop
         supportPoints (i) := Self.m_shape.localGetSupportingVertex (getUnitSpherePoints (i));
      end loop;


      hd.mFlags        := impact.d3.convex_Hull.QF_TRIANGLES;
      hd.mVcount       := numSampleDirections;

      hd.mVertices     := supportPoints'Unchecked_Access;
      hd.mVertexStride := math.Vector_3'Size / 8;


      if hl.CreateConvexHull (hd, hr'Access) = impact.d3.convex_Hull.QE_FAIL then
         return False;
      end if;


      declare
         new_Vertices : constant access_vector_3_array := new vector_3_array (1 .. hr.mNumOutputVertices);
      begin
         new_Vertices (Self.m_vertices'Range) := Self.m_vertices.all;
         free (Self.m_vertices);
         Self.m_vertices                      := new_Vertices;
      end;


      for i in 1 .. hr.mNumOutputVertices
      loop
         Self.m_vertices (1) := hr.m_OutputVertices (i);
      end loop;


      Self.m_numIndices := hr.mNumIndices;

      declare
         new_Indices : constant access_Unsigneds := new Containers.Unsigneds (1 .. Self.m_numIndices);
      begin
         new_Indices (Self.m_indices'Range) := Self.m_indices.all;
         free (Self.m_indices);
         Self.m_indices                     := new_Indices;
      end;


      for i in 1 .. Self.m_numIndices
      loop
         Self.m_indices (i) := C.unsigned (hr.m_Indices.Element (i));
      end loop;

      unused := hl.ReleaseResult (hr'Access);     -- Free temporary hull result that we just copied


      return True;
   end buildHull;






   function  numTriangles (Self : in Item) return Natural
   is
   begin
      return Self.m_numIndices / 3;
   end numTriangles;





   function  numVertices  (Self : in Item) return Natural
   is
   begin
      return Self.m_vertices'Length;
   end numVertices;




   function  numIndices   (Self : in Item) return Natural
   is
   begin
      return Self.m_numIndices;
   end numIndices;






   function  getVertexPointer (Self : in Item) return access math.Vector_3
   is
   begin
      return Self.m_vertices (1)'Access;
   end getVertexPointer;




   function  getIndexPointer (Self : in Item) return access Interfaces.C.Unsigned
   is
   begin
      return Self.m_indices (1)'Access;
   end getIndexPointer;



end impact.d3.shape_Hull;
