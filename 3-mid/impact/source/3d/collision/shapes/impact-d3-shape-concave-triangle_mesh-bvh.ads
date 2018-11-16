with impact.d3.Shape.concave.triangle_mesh,
     impact.d3.collision.quantized_Bvh.optimized;
with impact.d3.triangle_Callback;
with impact.d3.triangle_info_Map;


--  #include "impact.d3.Shape.concave.triangle_mesh.h"
--  #include "impact.d3.collision.quantized_Bvh.optimized.h"
--  #include "LinearMath/btAlignedAllocator.h"





package impact.d3.Shape.concave.triangle_mesh.bvh
--
--    The impact.d3.Shape.concave.triangle_mesh.bvh is a static-triangle mesh shape with several optimizations, such as bounding volume
--  hierarchy and cache friendly traversal for PlayStation 3 Cell SPU. It is recommended to enable useQuantizedAabbCompression
--  for better memory usage.
--
--    It takes a triangle mesh as input, for example a impact.d3.striding_Mesh.triangle_index_vertex_array.triangle_mesh or impact.d3.striding_Mesh.triangle_index_vertex_array. The impact.d3.Shape.concave.triangle_mesh.bvh class
--  allows for triangle mesh deformations by a refit or partialRefit method.
--
--    Instead of building the bounding volume hierarchy acceleration structure, it is also possible to serialize (save)
--  and deserialize (load) the structure from disk.
--
--    See Demos\ConcaveDemo\ConcavePhysicsDemo.cpp for an example.
--
is

   type Item is new impact.d3.Shape.concave.triangle_mesh.item with private;
   type View is access all Item'Class;


   procedure performRaycast (Self : in out Item;   callback             : access impact.d3.triangle_Callback.Item'Class;
                                                   raySource, rayTarget : in     math.Vector_3);

   --          void performRaycast (impact.d3.triangle_Callback* callback, const impact.d3.Vector& raySource, const impact.d3.Vector& rayTarget);



private


   type Item is new impact.d3.Shape.concave.triangle_mesh.item with
      record
         m_bvh                        : access impact.d3.collision.quantized_Bvh.optimized.item;
         m_triangleInfoMap            : access impact.d3.triangle_info_Map.item;

         m_useQuantizedAabbCompression,
         m_ownsBvh                    : Boolean;

         --          bool m_pad[11];    // need padding due to alignment
      end record;






end impact.d3.Shape.concave.triangle_mesh.bvh;



--  impact.d3.Shape.concave.triangle_mesh.bvh : public impact.d3.Shape.concave.triangle_mesh
--  {
--
--
--  public:
--
--          BT_DECLARE_ALIGNED_ALLOCATOR();
--
--
--          impact.d3.Shape.concave.triangle_mesh.bvh(impact.d3.striding_Mesh* meshInterface, bool useQuantizedAabbCompression, bool buildBvh = true);
--
--          ///optionally pass in a larger bvh aabb, used for quantization. This allows for deformations within this aabb
--          impact.d3.Shape.concave.triangle_mesh.bvh(impact.d3.striding_Mesh* meshInterface, bool useQuantizedAabbCompression,const impact.d3.Vector& bvhAabbMin,const impact.d3.Vector& bvhAabbMax, bool buildBvh = true);
--
--          virtual ~impact.d3.Shape.concave.triangle_mesh.bvh();
--
--          bool getOwnsBvh () const
--          {
--                  return m_ownsBvh;
--          }
--
--
--
--          void performConvexcast (impact.d3.triangle_Callback* callback, const impact.d3.Vector& boxSource, const impact.d3.Vector& boxTarget, const impact.d3.Vector& boxMin, const impact.d3.Vector& boxMax);
--
--          virtual void        processAllTriangles(impact.d3.triangle_Callback* callback,const impact.d3.Vector& aabbMin,const impact.d3.Vector& aabbMax) const;
--
--          void        refitTree(const impact.d3.Vector& aabbMin,const impact.d3.Vector& aabbMax);
--
--          ///for a fast incremental refit of parts of the tree. Note: the entire AABB of the tree will become more conservative, it never shrinks
--          void        partialRefitTree(const impact.d3.Vector& aabbMin,const impact.d3.Vector& aabbMax);
--
--          //debugging
--          virtual const char*        getName()const {return "BVHTRIANGLEMESH";}
--
--
--          virtual void        setLocalScaling(const impact.d3.Vector& scaling);
--
--          impact.d3.collision.quantized_Bvh.optimized*        getOptimizedBvh()
--          {
--                  return m_bvh;
--          }
--
--          void        setOptimizedBvh(impact.d3.collision.quantized_Bvh.optimized* bvh, const impact.d3.Vector& localScaling=impact.d3.Vector(1,1,1));
--
--          void    buildOptimizedBvh();
--
--          bool        usesQuantizedAabbCompression() const
--          {
--                  return        m_useQuantizedAabbCompression;
--          }
--
--          void        setTriangleInfoMap(impact.d3.triangle_info_Map* triangleInfoMap)
--          {
--                  m_triangleInfoMap = triangleInfoMap;
--          }
--
--          const impact.d3.triangle_info_Map*        getTriangleInfoMap() const
--          {
--                  return m_triangleInfoMap;
--          }
--
--          impact.d3.triangle_info_Map*        getTriangleInfoMap()
--          {
--                  return m_triangleInfoMap;
--          }
--
--  };
--
