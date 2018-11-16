with impact.d3.Shape.concave,
     impact.d3.triangle_Callback,
     impact.d3.striding_Mesh;


package impact.d3.Shape.concave.triangle_mesh
--
--  The impact.d3.Shape.concave.triangle_mesh is an internal concave triangle mesh interface.
--
--  Don't use this class directly, use impact.d3.Shape.concave.triangle_mesh.bvh instead.
--
is

   type Item is new impact.d3.Shape.concave.Item with private;



   --- Forge
   --
   overriding procedure destruct (Self : in out Item);




   --- Attributes

   --


   overriding procedure setLocalScaling          (Self : in out Item;   scaling : in math.Vector_3);
   overriding function  getLocalScaling          (Self : in     Item)         return math.Vector_3;

   overriding function  getName                  (Self : in     Item)        return String;

   function  localGetSupportingVertex (Self : in     Item;   vec : in math.Vector_3) return math.Vector_3;

   function  getMeshInterface         (Self : in     Item) return access impact.d3.striding_Mesh.item'Class;

   function  getLocalAabbMin          (Self : in     Item) return math.Vector_3;
   function  getLocalAabbMax          (Self : in     Item) return math.Vector_3;





   --  Operations
   --

   procedure recalcLocalAabb       (Self : in out Item);

   overriding procedure processAllTriangles   (Self : in     Item;   callback         : access impact.d3.triangle_Callback.Item'Class;
                                                          aabbMin, aabbMax : in     math.Vector_3);

   overriding procedure calculateLocalInertia (Self : in     Item;   mass    : in     math.Real;
                                                          inertia :    out math.Vector_3);

   overriding procedure getAabb               (Self : in     Item;   t                : in     Transform_3d;
                                                          aabbMin, aabbMax :    out math.Vector_3);





private

   type Item is new impact.d3.Shape.concave.Item with
      record
         m_localAabbMin,
         m_localAabbMax  : math.Vector_3;

         m_meshInterface : impact.d3.striding_Mesh.view;
      end record;


   function to_triangle_mesh_Shape (meshInterface : access impact.d3.striding_Mesh.Item'Class) return Item'Class;





   --- SupportVertexCallback
   --

   type SupportVertexCallback is new impact.d3.triangle_Callback.Item with
      record
         m_supportVertexLocal : math.Vector_3;

         m_worldTrans         : Transform_3d;
         m_maxDot             : math.Real;
         m_supportVecLocal    : math.Vector_3;
      end record;



   function to_SupportVertexCallback (supportVecWorld : in math.Vector_3;
                                      trans           : in Transform_3d) return SupportVertexCallback;

   overriding procedure processTriangle       (Self : in out SupportVertexCallback;   triangle      : access math.Matrix_3x3;
                                                                           partId        : in     Integer;
                                                                           triangleIndex : in     Integer      );

   function  GetSupportVertexLocal (Self : in     SupportVertexCallback) return math.Vector_3;




end impact.d3.Shape.concave.triangle_mesh;
