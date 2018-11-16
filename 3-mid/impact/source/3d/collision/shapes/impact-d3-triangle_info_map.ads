with ada.Containers.Hashed_Maps;
with Ada.Unchecked_Conversion;


--  #include "LinearMath/btHashMap.h"
--  #include "LinearMath/btSerializer.h"


package impact.d3.triangle_info_Map
--
--  The impact.d3.triangle_info_Map stores edge angle information for some triangles.
--  You can compute this information yourself or using btGenerateInternalEdgeInfo.
--
is


   --  The btTriangleInfo structure stores information to adjust collision normals to avoid collisions against internal edges
   --
   type btTriangleInfo is
      record
         m_flags         : Integer   := 0;

         m_edgeV0V1Angle : math.Real := 2.0 * math.Pi;
         m_edgeV1V2Angle : math.Real := 2.0 * math.Pi;
         m_edgeV2V0Angle : math.Real := 2.0 * math.Pi;
      end record;



   function Hash                       is new ada.Unchecked_Conversion (Integer, ada.Containers.Hash_Type);

   package  btInternalTriangleInfoMaps is new ada.Containers.Hashed_Maps (Integer, btTriangleInfo, Hash, "=");
   subtype  btInternalTriangleInfoMap   is     btInternalTriangleInfoMaps.Map;

   --  typedef btHashMap<btHashInt,btTriangleInfo> btInternalTriangleInfoMap;




   type Item is new btInternalTriangleInfoMap with private;



   --  for btTriangleInfo m_flags
   --
   TRI_INFO_V0V1_CONVEX       : constant := 1;
   TRI_INFO_V1V2_CONVEX       : constant := 2;
   TRI_INFO_V2V0_CONVEX       : constant := 4;

   TRI_INFO_V0V1_SWAP_NORMALB : constant := 8;
   TRI_INFO_V1V2_SWAP_NORMALB : constant := 16;
   TRI_INFO_V2V0_SWAP_NORMALB : constant := 32;







private

   type Item is new btInternalTriangleInfoMap with
      record
         null;
      end record;



end impact.d3.triangle_info_Map;




--
--  struct        impact.d3.triangle_info_Map : public btInternalTriangleInfoMap
--  {
--          impact.d3.Scalar        m_convexEpsilon;///used to determine if an edge or contact normal is convex, using the dot product
--          impact.d3.Scalar        m_planarEpsilon; ///used to determine if a triangle edge is planar with zero angle
--          impact.d3.Scalar        m_equalVertexThreshold; ///used to compute connectivity: if the distance between two vertices is smaller than m_equalVertexThreshold, they are considered to be 'shared'
--          impact.d3.Scalar        m_edgeDistanceThreshold; ///used to determine edge contacts: if the closest distance between a contact point and an edge is smaller than this distance threshold it is considered to "hit the edge"
--          impact.d3.Scalar        m_maxEdgeAngleThreshold; //ignore edges that connect triangles at an angle larger than this m_maxEdgeAngleThreshold
--          impact.d3.Scalar        m_zeroAreaThreshold; ///used to determine if a triangle is degenerate (length squared of cross product of 2 triangle edges < threshold)
--
--
--          impact.d3.triangle_info_Map()
--          {
--                  m_convexEpsilon = 0.00f;
--                  m_planarEpsilon = 0.0001f;
--                  m_equalVertexThreshold = impact.d3.Scalar(0.0001)*impact.d3.Scalar(0.0001);
--                  m_edgeDistanceThreshold = impact.d3.Scalar(0.1);
--                  m_zeroAreaThreshold = impact.d3.Scalar(0.0001)*impact.d3.Scalar(0.0001);
--                  m_maxEdgeAngleThreshold = SIMD_2_PI;
--          }
--          virtual ~impact.d3.triangle_info_Map() {}
--
--          virtual        int        calculateSerializeBufferSize() const;
--
--          ///fills the dataBuffer and returns the struct name (and 0 on failure)
--          virtual        const char*        serialize(void* dataBuffer, btSerializer* serializer) const;
--
--          void        deSerialize(struct impact.d3.triangle_info_MapData& data);
--
--  };






--  struct        btTriangleInfoData
--  {
--          int                        m_flags;
--          float        m_edgeV0V1Angle;
--          float        m_edgeV1V2Angle;
--          float        m_edgeV2V0Angle;
--  };




--  struct        impact.d3.triangle_info_MapData
--  {
--          int                                        *m_hashTablePtr;
--          int                                        *m_nextPtr;
--          btTriangleInfoData        *m_valueArrayPtr;
--          int                                        *m_keyArrayPtr;
--
--          float        m_convexEpsilon;
--          float        m_planarEpsilon;
--          float        m_equalVertexThreshold;
--          float        m_edgeDistanceThreshold;
--          float        m_zeroAreaThreshold;
--
--          int                m_nextSize;
--          int                m_hashTableSize;
--          int                m_numValues;
--          int                m_numKeys;
--          char        m_padding[4];
--  };

