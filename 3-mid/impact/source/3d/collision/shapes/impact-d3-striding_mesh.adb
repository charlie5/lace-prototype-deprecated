with impact.d3.Vector;
with impact.d3.Matrix;


package body impact.d3.striding_Mesh
is

   use Interfaces;



   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;




   procedure InternalProcessAllTriangles (Self : in    Item;   callback         : access impact.d3.triangle_Callback.btInternalTriangleIndexCallback'Class;
                                                               aabbMin, aabbMax : in     math.Vector_3)
   is
      pragma Unreferenced (aabbMin, aabbMax);
      numtotalphysicsverts : Integer := 0;
--        part                 : Integer;
      graphicssubparts     : constant Integer := Item'Class (Self).getNumSubParts;

      vertexbase   : aliased impact.d3.Containers.real_Pointer;   -- unsigned_char_Pointer;
      indexbase    : aliased swig.Pointers.unsigned_Pointer; -- unsigned_char_Pointer;
      graphicsbase : aliased impact.d3.Containers.real_Pointer;   -- access  math.Real;

      indexstride : Integer;
      stride,
      numverts,
      numtriangles : Integer;
--        gfxindex : Integer;

--        triangle    : aliased bullet.Vector_3_array (1..3);
      triangle    : aliased math.Matrix_3x3;
      meshScaling : math.Vector_3               := Self.getScaling;

   begin
      --  If the number of parts is big, the performance might drop due to the innerloop switch on indextype.
      --
      for part in 1 .. graphicssubparts
      loop
         Item'Class (Self).getLockedReadOnlyVertexIndexBase (vertexbase, numverts,    stride,
                                                             indexbase,  indexstride, numtriangles,
                                                             part);

         numtotalphysicsverts := numtotalphysicsverts  +  numtriangles * 3;   -- upper bound


         for gfxindex in 1 .. numtriangles
         loop
            declare
               use impact.d3.Matrix;
               use impact.d3.Containers.real_Pointers;
               use Swig.Pointers;
               use Swig.Pointers.c_unsigned_Pointers, swig.Pointers.c_double_Pointers;
               use type C.unsigned;

               tri_indices_1 : constant swig.Pointers.unsigned_Pointer := indexbase  +  C.ptrdiff_t (gfxindex * indexstride + 0);
               tri_indices_2 : constant swig.Pointers.unsigned_Pointer := indexbase  +  C.ptrdiff_t (gfxindex * indexstride + 1);
               tri_indices_3 : constant swig.Pointers.unsigned_Pointer := indexbase  +  C.ptrdiff_t (gfxindex * indexstride + 2);

               gb_1, gb_2, gb_3 : impact.d3.Containers.real_Pointer;
            begin
               graphicsbase := (vertexbase  +  c.ptrdiff_t (tri_indices_1.all * c.unsigned (stride)));

               gb_1 := graphicsbase;
               gb_2 := graphicsbase + 1;
               gb_3 := graphicsbase + 2;

               Row (triangle'Access, 1).all := (math.Real (gb_1.all) * math.Real (meshScaling (1)),
                                                math.Real (gb_2.all) * math.Real (meshScaling (2)),
                                                math.Real (gb_3.all) * math.Real (meshScaling (3)));


               graphicsbase := (vertexbase  +  c.ptrdiff_t (tri_indices_2.all * c.unsigned (stride)));   -- tri_indices (2) * stride);

               gb_1 := graphicsbase;
               gb_2 := graphicsbase + 1;
               gb_3 := graphicsbase + 2;

               Row (triangle'Access, 2).all := (math.Real (gb_1.all) * math.Real (meshScaling (1)),
                                                math.Real (gb_2.all) * math.Real (meshScaling (2)),
                                                math.Real (gb_3.all) * math.Real (meshScaling (3)));


               graphicsbase := (vertexbase  +  c.ptrdiff_t (tri_indices_3.all * c.unsigned (stride)));   -- tri_indices_3.all * stride);

               gb_1 := graphicsbase;
               gb_2 := graphicsbase + 1;
               gb_3 := graphicsbase + 2;

               Row (triangle'Access, 3).all := (math.Real (gb_1.all) * math.Real (meshScaling (1)),
                                                math.Real (gb_2.all) * math.Real (meshScaling (2)),
                                                math.Real (gb_3.all) * math.Real (meshScaling (3)));


               callback.internalProcessTriangleIndex (triangle'Access, part, gfxindex);
            end;
         end loop;

         Item'Class (Self).unLockReadOnlyVertexBase (part);
      end loop;

   end InternalProcessAllTriangles;







   procedure calculateAabbBruteForce     (Self : in    Item;   aabbMin, aabbMax :    out     math.Vector_3)
   is

      type AabbCalculationCallback is new impact.d3.triangle_Callback.btInternalTriangleIndexCallback with
         record
            m_aabbMin : math.Vector_3 := (BT_LARGE_FLOAT,  BT_LARGE_FLOAT,  BT_LARGE_FLOAT);
            m_aabbMax : math.Vector_3 := (-BT_LARGE_FLOAT, -BT_LARGE_FLOAT, -BT_LARGE_FLOAT);
         end record;


      procedure internalProcessTriangleIndex (Self : in out AabbCalculationCallback;   triangle      : in     Vector_3_array;
                                                                                       partId        : in     Integer;
                                                                                       triangleIndex : in     Integer)
      is
         pragma Unreferenced (partId, triangleIndex);
         use impact.d3.Vector;
      begin
         setMin (Self.m_aabbMin,  triangle (1));
         setMax (Self.m_aabbMax,  triangle (1));

         setMin (Self.m_aabbMin,  triangle (2));
         setMax (Self.m_aabbMax,  triangle (2));

         setMin (Self.m_aabbMin,  triangle (3));
         setMax (Self.m_aabbMax,  triangle (3));
      end internalProcessTriangleIndex;


      aabbCallback : aliased AabbCalculationCallback;

   begin
      --  First calculate the total aabb for all triangles.
      --
      aabbMin := (-BT_LARGE_FLOAT, -BT_LARGE_FLOAT, -BT_LARGE_FLOAT);
      aabbMax := (BT_LARGE_FLOAT,  BT_LARGE_FLOAT,  BT_LARGE_FLOAT);

      Self.InternalProcessAllTriangles (aabbCallback'Access,  aabbMin, aabbMax);

      aabbMin := aabbCallback.m_aabbMin;
      aabbMax := aabbCallback.m_aabbMax;
   end calculateAabbBruteForce;







   function hasPremadeAabb (Self : in Item) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end hasPremadeAabb;







   function  getScaling (Self : in     Item)         return math.Vector_3
   is
   begin
      return Self.m_scaling;
   end getScaling;




   procedure setScaling (Self :    out Item;   scaling : in math.Vector_3)
   is
   begin
      Self.m_scaling := scaling;
   end setScaling;



end impact.d3.striding_Mesh;
