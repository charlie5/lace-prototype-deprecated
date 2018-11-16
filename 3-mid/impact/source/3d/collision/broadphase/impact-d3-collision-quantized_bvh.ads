with interfaces.c;


package impact.d3.collision.quantized_Bvh
--
--  The impact.d3.collision.quantized_Bvh class stores an AABB tree that can be quickly traversed on CPU and Cell SPU.
--
--  It is used by the impact.d3.Shape.concave.triangle_mesh.bvh as midphase, and by the btMultiSapBroadphase.
--  It is recommended to use quantization for better performance and lower memory requirements.
--
is

   type Item is tagged private;





   type unsigned_shorts is array (Positive range <>) of interfaces.c.unsigned_short;


   type btBvhSubtreeInfoData is
      record
         m_rootNodeIndex,
         m_subtreeSize      : Integer;

         m_quantizedAabbMin : unsigned_shorts (1 .. 3);
         m_quantizedAabbMax : unsigned_shorts (1 .. 3);
      end record;





   subtype Double_Vector_3 is math.Vector_3;



   type optimizedNodeDoubleData is
      record
         m_aabbMinOrg : Double_Vector_3;
         m_aabbMaxOrg : Double_Vector_3;

         m_escapeIndex,
         m_subPart,
         m_triangleIndex : Integer;

         --          char        m_pad[4];
      end record;




   type NodeData is
      record
         m_quantizedAabbMin,
         m_quantizedAabbMax           : unsigned_shorts (1 .. 3);

         m_escapeIndexOrTriangleIndex : Integer;
      end record;




   type DoubleData is
      record
         m_bvhAabbMin,
         m_bvhAabbMax,
         m_bvhQuantization             : Double_Vector_3;

         m_curNodeIndex,
         m_useQuantization,
         m_numContiguousLeafNodes,
         m_numQuantizedContiguousNodes : Integer;

         m_contiguousNodesPtr          : access optimizedNodeDoubleData;
         m_quantizedContiguousNodesPtr : access NodeData;

         m_traversalMode,
         m_numSubtreeHeaders           : Integer;

         m_subTreeInfoPtr              : access btBvhSubtreeInfoData;
      end record;












   subtype Data              is Double_Vector_3;
   subtype optimizedNodeData is optimizedNodeDoubleData;

   DataName : constant String := "impact.d3.collision.quantized_Bvh.DoubleData";





   --  ///for code readability:
--  typedef btAlignedObjectArray<impact.d3.collision.quantized_Bvh.optimizedNode>        NodeArray;
--  typedef btAlignedObjectArray<impact.d3.collision.quantized_BvhNode>        QuantizedNodeArray;
--  typedef btAlignedObjectArray<btBvhSubtreeInfo>                BvhSubtreeInfoArray;
--



   MAX_SUBTREE_SIZE_IN_BYTES : constant := 2048;
   --
   --  Note: currently we have 16 bytes per quantized node



   MAX_NUM_PARTS_IN_BITS     : constant := 10;
   --
   --  10 gives the potential for 1024 parts, with at most 2^21 (2097152) (minus one
   --   actually) triangles each (since the sign bit is reserved








   --  impact.d3.collision.quantized_BvhNode is a compressed aabb node, 16 bytes.
   --  Node can be used for leafnode or internal node. Leafnodes can point to 32-bit triangle index (non-negative range).
   --
   type Node is
      record
         m_quantizedAabbMin : unsigned_shorts (1 .. 3);            -- 6 bytes
         m_quantizedAabbMax : unsigned_shorts (1 .. 3);            -- 6 bytes

         m_escapeIndexOrTriangleIndex : Integer;                 -- 4 bytes
      end record;



--
--  struct impact.d3.collision.quantized_BvhNode
--  {
--
--          bool isLeafNode() const
--          {
--                  //skipindex is negative (internal node), triangleindex >=0 (leafnode)
--                  return (m_escapeIndexOrTriangleIndex >= 0);
--          }
--          int getEscapeIndex() const
--          {
--                  btAssert(!isLeafNode());
--                  return -m_escapeIndexOrTriangleIndex;
--          }
--          int        getTriangleIndex() const
--          {
--                  btAssert(isLeafNode());
--                  // Get only the lower bits where the triangle index is stored
--                  return (m_escapeIndexOrTriangleIndex&~((~0)<<(31-MAX_NUM_PARTS_IN_BITS)));
--          }
--          int        getPartId() const
--          {
--                  btAssert(isLeafNode());
--                  // Get only the highest bits where the part index is stored
--                  return (m_escapeIndexOrTriangleIndex>>(31-MAX_NUM_PARTS_IN_BITS));
--          }
--  }
--  ;









private

   type Item is tagged
      record
         null;
      end record;





   procedure dummy;


end impact.d3.collision.quantized_Bvh;







--  /// impact.d3.collision.quantized_Bvh.optimizedNode contains both internal and leaf node information.
--  /// Total node size is 44 bytes / node. You can use the compressed version of 16 bytes.
--
--  ATTRIBUTE_ALIGNED16 (struct) impact.d3.collision.quantized_Bvh.optimizedNode
--  {
--          BT_DECLARE_ALIGNED_ALLOCATOR();
--
--          //32 bytes
--          impact.d3.Vector        m_aabbMinOrg;
--          impact.d3.Vector        m_aabbMaxOrg;
--
--          //4
--          int        m_escapeIndex;
--
--          //8
--          //for child nodes
--          int        m_subPart;
--          int        m_triangleIndex;
--
--  //pad the size to 64 bytes
--          char        m_padding[20];
--  };






--  ///btBvhSubtreeInfo provides info to gather a subtree of limited size
--
--  ATTRIBUTE_ALIGNED16(class) btBvhSubtreeInfo
--  {
--  public:
--          BT_DECLARE_ALIGNED_ALLOCATOR();
--
--          //12 bytes
--          unsigned short int        m_quantizedAabbMin[3];
--          unsigned short int        m_quantizedAabbMax[3];
--          //4 bytes, points to the root of the subtree
--          int                        m_rootNodeIndex;
--          //4 bytes
--          int                        m_subtreeSize;
--          int                        m_padding[3];
--
--          btBvhSubtreeInfo()
--          {
--                  //memset(&m_padding[0], 0, sizeof(m_padding));
--          }
--
--
--          void        setAabbFromQuantizeNode(const impact.d3.collision.quantized_BvhNode& quantizedNode)
--          {
--                  m_quantizedAabbMin[0] = quantizedNode.m_quantizedAabbMin[0];
--                  m_quantizedAabbMin[1] = quantizedNode.m_quantizedAabbMin[1];
--                  m_quantizedAabbMin[2] = quantizedNode.m_quantizedAabbMin[2];
--                  m_quantizedAabbMax[0] = quantizedNode.m_quantizedAabbMax[0];
--                  m_quantizedAabbMax[1] = quantizedNode.m_quantizedAabbMax[1];
--                  m_quantizedAabbMax[2] = quantizedNode.m_quantizedAabbMax[2];
--          }
--  }
--  ;





--  class btNodeOverlapCallback
--  {
--  public:
--          virtual ~btNodeOverlapCallback() {};
--
--          virtual void processNode(int subPart, int triangleIndex) = 0;
--  };












--  class impact.d3.collision.quantized_Bvh
--  {
--  public:
--          enum btTraversalMode
--          {
--                  TRAVERSAL_STACKLESS = 0,
--                  TRAVERSAL_STACKLESS_CACHE_FRIENDLY,
--                  TRAVERSAL_RECURSIVE
--          };
--
--  protected:
--
--
--          impact.d3.Vector                        m_bvhAabbMin;
--          impact.d3.Vector                        m_bvhAabbMax;
--          impact.d3.Vector                        m_bvhQuantization;
--
--          int                                        m_bulletVersion;        //for serialization versioning. It could also be used to detect endianess.
--
--          int                                        m_curNodeIndex;
--          //quantization data
--          bool                                m_useQuantization;
--
--
--
--          NodeArray                        m_leafNodes;
--          NodeArray                        m_contiguousNodes;
--          QuantizedNodeArray        m_quantizedLeafNodes;
--          QuantizedNodeArray        m_quantizedContiguousNodes;
--
--          btTraversalMode        m_traversalMode;
--          BvhSubtreeInfoArray                m_SubtreeHeaders;
--
--          //This is only used for serialization so we don't have to add serialization directly to btAlignedObjectArray
--          mutable int m_subtreeHeaderCount;
--
--
--
--
--
--          ///two versions, one for quantized and normal nodes. This allows code-reuse while maintaining readability (no template/macro!)
--          ///this might be refactored into a virtual, it is usually not calculated at run-time
--          void        setInternalNodeAabbMin(int nodeIndex, const impact.d3.Vector& aabbMin)
--          {
--                  if (m_useQuantization)
--                  {
--                          quantize(&m_quantizedContiguousNodes[nodeIndex].m_quantizedAabbMin[0] ,aabbMin,0);
--                  } else
--                  {
--                          m_contiguousNodes[nodeIndex].m_aabbMinOrg = aabbMin;
--
--                  }
--          }
--          void        setInternalNodeAabbMax(int nodeIndex,const impact.d3.Vector& aabbMax)
--          {
--                  if (m_useQuantization)
--                  {
--                          quantize(&m_quantizedContiguousNodes[nodeIndex].m_quantizedAabbMax[0],aabbMax,1);
--                  } else
--                  {
--                          m_contiguousNodes[nodeIndex].m_aabbMaxOrg = aabbMax;
--                  }
--          }
--
--          impact.d3.Vector getAabbMin(int nodeIndex) const
--          {
--                  if (m_useQuantization)
--                  {
--                          return unQuantize(&m_quantizedLeafNodes[nodeIndex].m_quantizedAabbMin[0]);
--                  }
--                  //non-quantized
--                  return m_leafNodes[nodeIndex].m_aabbMinOrg;
--
--          }
--          impact.d3.Vector getAabbMax(int nodeIndex) const
--          {
--                  if (m_useQuantization)
--                  {
--                          return unQuantize(&m_quantizedLeafNodes[nodeIndex].m_quantizedAabbMax[0]);
--                  }
--                  //non-quantized
--                  return m_leafNodes[nodeIndex].m_aabbMaxOrg;
--
--          }
--
--
--          void        setInternalNodeEscapeIndex(int nodeIndex, int escapeIndex)
--          {
--                  if (m_useQuantization)
--                  {
--                          m_quantizedContiguousNodes[nodeIndex].m_escapeIndexOrTriangleIndex = -escapeIndex;
--                  }
--                  else
--                  {
--                          m_contiguousNodes[nodeIndex].m_escapeIndex = escapeIndex;
--                  }
--
--          }
--
--          void mergeInternalNodeAabb(int nodeIndex,const impact.d3.Vector& newAabbMin,const impact.d3.Vector& newAabbMax)
--          {
--                  if (m_useQuantization)
--                  {
--                          unsigned short int quantizedAabbMin[3];
--                          unsigned short int quantizedAabbMax[3];
--                          quantize(quantizedAabbMin,newAabbMin,0);
--                          quantize(quantizedAabbMax,newAabbMax,1);
--                          for (int i=0;i<3;i++)
--                          {
--                                  if (m_quantizedContiguousNodes[nodeIndex].m_quantizedAabbMin[i] > quantizedAabbMin[i])
--                                          m_quantizedContiguousNodes[nodeIndex].m_quantizedAabbMin[i] = quantizedAabbMin[i];
--
--                                  if (m_quantizedContiguousNodes[nodeIndex].m_quantizedAabbMax[i] < quantizedAabbMax[i])
--                                          m_quantizedContiguousNodes[nodeIndex].m_quantizedAabbMax[i] = quantizedAabbMax[i];
--
--                          }
--                  } else
--                  {
--                          //non-quantized
--                          m_contiguousNodes[nodeIndex].m_aabbMinOrg.setMin(newAabbMin);
--                          m_contiguousNodes[nodeIndex].m_aabbMaxOrg.setMax(newAabbMax);
--                  }
--          }
--
--          void        swapLeafNodes(int firstIndex,int secondIndex);
--
--          void        assignInternalNodeFromLeafNode(int internalNode,int leafNodeIndex);
--
--  protected:
--
--
--
--          void        buildTree        (int startIndex,int endIndex);
--
--          int        calcSplittingAxis(int startIndex,int endIndex);
--
--          int        sortAndCalcSplittingIndex(int startIndex,int endIndex,int splitAxis);
--
--          void        walkStacklessTree(btNodeOverlapCallback* nodeCallback,const impact.d3.Vector& aabbMin,const impact.d3.Vector& aabbMax) const;
--
--          void        walkStacklessQuantizedTreeAgainstRay(btNodeOverlapCallback* nodeCallback, const impact.d3.Vector& raySource, const impact.d3.Vector& rayTarget, const impact.d3.Vector& aabbMin, const impact.d3.Vector& aabbMax, int startNodeIndex,int endNodeIndex) const;
--          void        walkStacklessQuantizedTree(btNodeOverlapCallback* nodeCallback,unsigned short int* quantizedQueryAabbMin,unsigned short int* quantizedQueryAabbMax,int startNodeIndex,int endNodeIndex) const;
--          void        walkStacklessTreeAgainstRay(btNodeOverlapCallback* nodeCallback, const impact.d3.Vector& raySource, const impact.d3.Vector& rayTarget, const impact.d3.Vector& aabbMin, const impact.d3.Vector& aabbMax, int startNodeIndex,int endNodeIndex) const;
--
--          ///tree traversal designed for small-memory processors like PS3 SPU
--          void        walkStacklessQuantizedTreeCacheFriendly(btNodeOverlapCallback* nodeCallback,unsigned short int* quantizedQueryAabbMin,unsigned short int* quantizedQueryAabbMax) const;
--
--          ///use the 16-byte stackless 'skipindex' node tree to do a recursive traversal
--          void        walkRecursiveQuantizedTreeAgainstQueryAabb(const impact.d3.collision.quantized_BvhNode* currentNode,btNodeOverlapCallback* nodeCallback,unsigned short int* quantizedQueryAabbMin,unsigned short int* quantizedQueryAabbMax) const;
--
--          ///use the 16-byte stackless 'skipindex' node tree to do a recursive traversal
--          void        walkRecursiveQuantizedTreeAgainstQuantizedTree(const impact.d3.collision.quantized_BvhNode* treeNodeA,const impact.d3.collision.quantized_BvhNode* treeNodeB,btNodeOverlapCallback* nodeCallback) const;
--
--
--
--
--          void        updateSubtreeHeaders(int leftChildNodexIndex,int rightChildNodexIndex);
--
--  public:
--
--          BT_DECLARE_ALIGNED_ALLOCATOR();
--
--          impact.d3.collision.quantized_Bvh();
--
--          virtual ~impact.d3.collision.quantized_Bvh();
--
--
--          ///***************************************** expert/internal use only *************************
--          void        setQuantizationValues(const impact.d3.Vector& bvhAabbMin,const impact.d3.Vector& bvhAabbMax,impact.d3.Scalar quantizationMargin=impact.d3.Scalar(1.0));
--          QuantizedNodeArray&        getLeafNodeArray() {                        return        m_quantizedLeafNodes;        }
--          ///buildInternal is expert use only: assumes that setQuantizationValues and LeafNodeArray are initialized
--          void        buildInternal();
--          ///***************************************** expert/internal use only *************************
--
--          void        reportAabbOverlappingNodex(btNodeOverlapCallback* nodeCallback,const impact.d3.Vector& aabbMin,const impact.d3.Vector& aabbMax) const;
--          void        reportRayOverlappingNodex (btNodeOverlapCallback* nodeCallback, const impact.d3.Vector& raySource, const impact.d3.Vector& rayTarget) const;
--          void        reportBoxCastOverlappingNodex(btNodeOverlapCallback* nodeCallback, const impact.d3.Vector& raySource, const impact.d3.Vector& rayTarget, const impact.d3.Vector& aabbMin,const impact.d3.Vector& aabbMax) const;
--
--                  SIMD_FORCE_INLINE void quantize(unsigned short* out, const impact.d3.Vector& point,int isMax) const
--          {
--
--                  btAssert(m_useQuantization);
--
--                  btAssert(point.getX() <= m_bvhAabbMax.getX());
--                  btAssert(point.getY() <= m_bvhAabbMax.getY());
--                  btAssert(point.getZ() <= m_bvhAabbMax.getZ());
--
--                  btAssert(point.getX() >= m_bvhAabbMin.getX());
--                  btAssert(point.getY() >= m_bvhAabbMin.getY());
--                  btAssert(point.getZ() >= m_bvhAabbMin.getZ());
--
--                  impact.d3.Vector v = (point - m_bvhAabbMin) * m_bvhQuantization;
--                  ///Make sure rounding is done in a way that unQuantize(quantizeWithClamp(...)) is conservative
--                  ///end-points always set the first bit, so that they are sorted properly (so that neighbouring AABBs overlap properly)
--                  ///@todo: double-check this
--                  if (isMax)
--                  {
--                          out[0] = (unsigned short) (((unsigned short)(v.getX()+impact.d3.Scalar(1.)) | 1));
--                          out[1] = (unsigned short) (((unsigned short)(v.getY()+impact.d3.Scalar(1.)) | 1));
--                          out[2] = (unsigned short) (((unsigned short)(v.getZ()+impact.d3.Scalar(1.)) | 1));
--                  } else
--                  {
--                          out[0] = (unsigned short) (((unsigned short)(v.getX()) & 0xfffe));
--                          out[1] = (unsigned short) (((unsigned short)(v.getY()) & 0xfffe));
--                          out[2] = (unsigned short) (((unsigned short)(v.getZ()) & 0xfffe));
--                  }
--
--
--  #ifdef DEBUG_CHECK_DEQUANTIZATION
--                  impact.d3.Vector newPoint = unQuantize(out);
--                  if (isMax)
--                  {
--                          if (newPoint.getX() < point.getX())
--                          {
--                                  printf("unconservative X, diffX = %f, oldX=%f,newX=%f\n",newPoint.getX()-point.getX(), newPoint.getX(),point.getX());
--                          }
--                          if (newPoint.getY() < point.getY())
--                          {
--                                  printf("unconservative Y, diffY = %f, oldY=%f,newY=%f\n",newPoint.getY()-point.getY(), newPoint.getY(),point.getY());
--                          }
--                          if (newPoint.getZ() < point.getZ())
--                          {
--
--                                  printf("unconservative Z, diffZ = %f, oldZ=%f,newZ=%f\n",newPoint.getZ()-point.getZ(), newPoint.getZ(),point.getZ());
--                          }
--                  } else
--                  {
--                          if (newPoint.getX() > point.getX())
--                          {
--                                  printf("unconservative X, diffX = %f, oldX=%f,newX=%f\n",newPoint.getX()-point.getX(), newPoint.getX(),point.getX());
--                          }
--                          if (newPoint.getY() > point.getY())
--                          {
--                                  printf("unconservative Y, diffY = %f, oldY=%f,newY=%f\n",newPoint.getY()-point.getY(), newPoint.getY(),point.getY());
--                          }
--                          if (newPoint.getZ() > point.getZ())
--                          {
--                                  printf("unconservative Z, diffZ = %f, oldZ=%f,newZ=%f\n",newPoint.getZ()-point.getZ(), newPoint.getZ(),point.getZ());
--                          }
--                  }
--  #endif //DEBUG_CHECK_DEQUANTIZATION
--
--          }
--
--
--          SIMD_FORCE_INLINE void quantizeWithClamp(unsigned short* out, const impact.d3.Vector& point2,int isMax) const
--          {
--
--                  btAssert(m_useQuantization);
--
--                  impact.d3.Vector clampedPoint(point2);
--                  clampedPoint.setMax(m_bvhAabbMin);
--                  clampedPoint.setMin(m_bvhAabbMax);
--
--                  quantize(out,clampedPoint,isMax);
--
--          }
--
--          SIMD_FORCE_INLINE impact.d3.Vector        unQuantize(const unsigned short* vecIn) const
--          {
--                          impact.d3.Vector        vecOut;
--                          vecOut.setValue(
--                          (impact.d3.Scalar)(vecIn[0]) / (m_bvhQuantization.getX()),
--                          (impact.d3.Scalar)(vecIn[1]) / (m_bvhQuantization.getY()),
--                          (impact.d3.Scalar)(vecIn[2]) / (m_bvhQuantization.getZ()));
--                          vecOut += m_bvhAabbMin;
--                          return vecOut;
--          }
--
--          ///setTraversalMode let's you choose between stackless, recursive or stackless cache friendly tree traversal. Note this is only implemented for quantized trees.
--          void        setTraversalMode(btTraversalMode        traversalMode)
--          {
--                  m_traversalMode = traversalMode;
--          }
--
--
--          SIMD_FORCE_INLINE QuantizedNodeArray&        getQuantizedNodeArray()
--          {
--                  return        m_quantizedContiguousNodes;
--          }
--
--
--          SIMD_FORCE_INLINE BvhSubtreeInfoArray&        getSubtreeInfoArray()
--          {
--                  return m_SubtreeHeaders;
--          }
--
--  ////////////////////////////////////////////////////////////////////
--
--          /////Calculate space needed to store BVH for serialization
--          unsigned calculateSerializeBufferSize() const;
--
--          /// Data buffer MUST be 16 byte aligned
--          virtual bool serialize(void *o_alignedDataBuffer, unsigned i_dataBufferSize, bool i_swapEndian) const;
--
--          ///deSerializeInPlace loads and initializes a BVH from a buffer in memory 'in place'
--          static impact.d3.collision.quantized_Bvh *deSerializeInPlace(void *i_alignedDataBuffer, unsigned int i_dataBufferSize, bool i_swapEndian);
--
--          static unsigned int getAlignmentSerializationPadding();
--  //////////////////////////////////////////////////////////////////////
--
--
--          virtual        int        calculateSerializeBufferSizeNew() const;
--
--          ///fills the dataBuffer and returns the struct name (and 0 on failure)
--          virtual        const char*        serialize(void* dataBuffer, btSerializer* serializer) const;
--
--          virtual        void deSerializeFloat(struct impact.d3.collision.quantized_BvhFloatData& quantizedBvhFloatData);
--
--          virtual        void deSerializeDouble(struct impact.d3.collision.quantized_BvhDoubleData& quantizedBvhDoubleData);
--
--
--  ////////////////////////////////////////////////////////////////////
--
--          SIMD_FORCE_INLINE bool isQuantized()
--          {
--                  return m_useQuantization;
--          }
--
--  private:
--          // Special "copy" constructor that allows for in-place deserialization
--          // Prevents impact.d3.Vector's default constructor from being called, but doesn't inialize much else
--          // ownsMemory should most likely be false if deserializing, and if you are not, don't call this (it also changes the function signature, which we need)
--          impact.d3.collision.quantized_Bvh(impact.d3.collision.quantized_Bvh &other, bool ownsMemory);
--
--  }
--  ;
--
--
