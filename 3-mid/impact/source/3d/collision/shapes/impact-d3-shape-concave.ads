with impact.d3.Shape;
with impact.d3.triangle_Callback;



package impact.d3.Shape.concave
--
--  The impact.d3.Shape.concave class provides an interface for non-moving (static) concave shapes.
--  It has been implemented by the impact.d3.Shape.concave.static_plane, impact.d3.Shape.concave.triangle_mesh.bvh and impact.d3.Shape.concave.height_field_terrain.
--
is



   type Item is abstract new impact.d3.Shape.item with private;
   type View is access all Item'Class;



--  /// PHY_ScalarType enumerates possible scalar types.
--  /// See the impact.d3.striding_Mesh or impact.d3.Shape.concave.height_field_terrain for its use
--  typedef enum PHY_ScalarType {
--          PHY_FLOAT,
--          PHY_DOUBLE,
--          PHY_INTEGER,
--          PHY_SHORT,
--          PHY_FIXEDPOINT88,
--          PHY_UCHAR
--  } PHY_ScalarType;




   --- Forge
   --
   procedure define   (Self : in out Item);
   overriding procedure destruct (Self : in out Item);





   --- Attributes
   --

   overriding procedure setMargin (Self : in out Item;   margin : in Real);
   overriding function  getMargin (Self : in     Item)        return Real;



   procedure processAllTriangles (Self : in     Item;   callback         : access impact.d3.triangle_Callback.Item'Class;
                                                        aabbMin, aabbMax : in     math.Vector_3)
   is abstract;






private

   type Item is abstract new impact.d3.Shape.item with
      record
         m_collisionMargin : math.Real;
      end record;



end impact.d3.Shape.concave;




