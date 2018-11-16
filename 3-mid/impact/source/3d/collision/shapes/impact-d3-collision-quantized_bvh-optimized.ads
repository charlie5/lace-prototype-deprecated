
with impact.d3.collision.quantized_Bvh;


--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.quantized_Bvh.h"
--
--  class impact.d3.striding_Mesh;
--


package impact.d3.collision.quantized_Bvh.optimized
--
--  The impact.d3.collision.quantized_Bvh.optimized extends the impact.d3.collision.quantized_Bvh to create AABB tree for triangle meshes, through the impact.d3.striding_Mesh.
--
--  Contains contributions from Disney Studios.
--
is

   type Item is new impact.d3.collision.quantized_Bvh.item with null record;






private

      procedure dummy;


end impact.d3.collision.quantized_Bvh.optimized;




--
--  ATTRIBUTE_ALIGNED16(class) impact.d3.collision.quantized_Bvh.optimized : public impact.d3.collision.quantized_Bvh
--  {
--
--  public:
--          BT_DECLARE_ALIGNED_ALLOCATOR();
--
--  protected:
--
--  public:
--
--          impact.d3.collision.quantized_Bvh.optimized();
--
--          virtual ~impact.d3.collision.quantized_Bvh.optimized();
--
--          void        build(impact.d3.striding_Mesh* triangles,bool useQuantizedAabbCompression, const impact.d3.Vector& bvhAabbMin, const impact.d3.Vector& bvhAabbMax);
--
--          void        refit(impact.d3.striding_Mesh* triangles,const impact.d3.Vector& aabbMin,const impact.d3.Vector& aabbMax);
--
--          void        refitPartial(impact.d3.striding_Mesh* triangles,const impact.d3.Vector& aabbMin, const impact.d3.Vector& aabbMax);
--
--          void        updateBvhNodes(impact.d3.striding_Mesh* meshInterface,int firstNode,int endNode,int index);
--
--          /// Data buffer MUST be 16 byte aligned
--          virtual bool serializeInPlace(void *o_alignedDataBuffer, unsigned i_dataBufferSize, bool i_swapEndian) const
--          {
--                  return impact.d3.collision.quantized_Bvh::serialize(o_alignedDataBuffer,i_dataBufferSize,i_swapEndian);
--
--          }
--
--          ///deSerializeInPlace loads and initializes a BVH from a buffer in memory 'in place'
--          static impact.d3.collision.quantized_Bvh.optimized *deSerializeInPlace(void *i_alignedDataBuffer, unsigned int i_dataBufferSize, bool i_swapEndian);
--
--
--  };

