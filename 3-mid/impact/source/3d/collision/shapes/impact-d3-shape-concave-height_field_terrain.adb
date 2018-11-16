with impact.d3.Vector,
     impact.d3.Transform,
     impact.d3.Matrix,
     impact.d3.collision.Proxy,
     Interfaces;


package body impact.d3.Shape.concave.height_field_terrain
is



   ------------
   --- Utility
   --

   function getQuantized (x : in math.Real) return Integer
   is
   begin
      if x < 0.0 then
         return Integer (x - 0.5);
      end if;

      return Integer (x + 0.5);
   end getQuantized;








   ----------
   --- Forge
   --

   function to_height_field_terrain_Shape (heightStickWidth, heightStickLength : in     Integer  ;
                                           heightfieldData                     : access math.Vector;
                                           heightScale                         : in     math.Real;
                                           minHeight, maxHeight                : in     math.Real;
                                           upAxis                              : in     Integer  ;
                                           flipQuadEdges                       : in     Boolean  ) return Item
   is
      Self : Item;
   begin
      Self.initialize (heightStickWidth, heightStickLength,
                       heightfieldData,
                       heightScale,
                       minHeight,        maxHeight,
                       upAxis,
                       flipQuadEdges);
      return Self;
   end to_height_field_terrain_Shape;





   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;






   ---------------
   --- Attributes
   --

   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out math.Vector_3 )
   is
      use impact.d3.Vector, impact.d3.Matrix, impact.d3.Transform, math.Vectors;

      halfExtents : constant math.Vector_3 := Scaled (Self.m_localAabbMax - Self.m_localAabbMin, by => Self.m_localScaling) * 0.5;
      localOrigin : math.Vector_3 := math.Origin_3d;  -- ?? ... what is this for ?

      abs_b  : aliased math.Matrix_3x3 := absolute  (getBasis (t));
      center : constant math.Vector_3  := getOrigin (t);

      extent : constant math.Vector_3 :=   (dot (Row (abs_b'Access, 1).all,  halfExtents),
                                   dot (Row (abs_b'Access, 2).all,  halfExtents),
                                   dot (Row (abs_b'Access, 3).all,  halfExtents))
                                + (Self.getMargin, Self.getMargin, Self.getMargin);

   begin
      localOrigin (Self.m_upAxis) := (Self.m_minHeight + Self.m_maxHeight)  *  0.5;
      localOrigin                 := Scaled (localOrigin, by => Self.m_localScaling);

      aabbMin := center - extent;
      aabbMax := center + extent;
   end getAabb;







   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3)
   is
   begin
      Self.m_localScaling := scaling;
   end setLocalScaling;





   overriding function  getLocalScaling (Self : in     Item)         return math.Vector_3
   is
   begin
      return Self.m_localScaling;
   end getLocalScaling;




   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      pragma Unreferenced (Self, mass);
   begin
      inertia := (0.0, 0.0, 0.0);        -- moving concave objects not supported
   end calculateLocalInertia;





   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "HEIGHTFIELD";
   end getName;






   --  Process all triangles within the provided axis-aligned bounding box
   --
   --    basic algorithm:
   --      - convert input aabb to local coordinates (scale down and shift for local origin)
   --      - convert input aabb to a range of heightfield grid points (quantize)
   --      - iterate over all triangles in that subset of the grid
   --
   --
   overriding procedure processAllTriangles      (Self : in     Item;   callback              : access impact.d3.triangle_Callback.Item'Class;
                                                             aabbMin, aabbMax      : in     math.Vector_3)
   is
      use impact.d3.Vector,   math.Vectors;

      --  scale down the input aabb's so they are in local (non-scaled) coordinates
      --
      localAabbMin     : math.Vector_3 := Scaled (aabbMin, by => (1.0 / Self.m_localScaling (1),
                                                                  1.0 / Self.m_localScaling (2),
                                                                  1.0 / Self.m_localScaling (3)));
      localAabbMax     : math.Vector_3 := Scaled (aabbMax, by => (1.0 / Self.m_localScaling (1),
                                                                  1.0 / Self.m_localScaling (2),
                                                                  1.0 / Self.m_localScaling (3)));

      quantizedAabbMin,
      quantizedAabbMax : math.Integers (1 .. 3);

      j, x             : Integer;

   begin
      --  account for local origin
      --
      localAabbMin := localAabbMin + Self.m_localOrigin;
      localAabbMax := localAabbMax + Self.m_localOrigin;

      --  quantize the aabbMin and aabbMax, and adjust the start/end ranges
      --
      Self.quantizeWithClamp (quantizedAabbMin, localAabbMin, False);
      Self.quantizeWithClamp (quantizedAabbMax, localAabbMax, True);

      --  expand the min/max quantized values
      --  this is to catch the case where the input aabb falls between grid points!
      --
      for i in 1 .. 3
      loop
         quantizedAabbMin (i) := quantizedAabbMin (i) - 1;
         quantizedAabbMax (i) := quantizedAabbMax (i) + 1;
      end loop;

      declare
         startX : Integer := 0;
         endX   : Integer := Self.m_heightStickWidth  - 1;
         startJ : Integer := 0;
         endJ   : Integer := Self.m_heightStickLength - 1;

      begin
         case Self.m_upAxis
         is
         when 1 =>
            if quantizedAabbMin (2) > startX then   startX := quantizedAabbMin (2);   end if;
            if quantizedAabbMax (2) < endX   then   endX   := quantizedAabbMax (2);   end if;
            if quantizedAabbMin (3) > startJ then   startJ := quantizedAabbMin (3);   end if;
            if quantizedAabbMax (3) < endJ   then   endJ   := quantizedAabbMax (3);   end if;

         when 2 =>
            if quantizedAabbMin (1) > startX then   startX := quantizedAabbMin (1);   end if;
            if quantizedAabbMax (1) < endX   then   endX   := quantizedAabbMax (1);   end if;
            if quantizedAabbMin (3) > startJ then   startJ := quantizedAabbMin (3);   end if;
            if quantizedAabbMax (3) < endJ   then   endJ   := quantizedAabbMax (3);   end if;

         when 3 =>
            if quantizedAabbMin (1) > startX then   startX := quantizedAabbMin (1);   end if;
            if quantizedAabbMax (1) < endX   then   endX   := quantizedAabbMax (1);   end if;
            if quantizedAabbMin (2) > startJ then   startJ := quantizedAabbMin (2);   end if;
            if quantizedAabbMax (2) < endJ   then   endJ   := quantizedAabbMax (2);   end if;

         when others =>
            raise Program_Error;
         end case;



         j := startJ;

         while j < endJ
         loop
            x := startX;

            while x < endX
            loop
               declare
                  use type interfaces.Unsigned_32;

                  vertices     : array (1 .. 3) of math.Vector_3;
                  the_Vertices : aliased math.Matrix_3x3;

                  procedure set_the_Vertices
                  is
                  begin
                     the_Vertices := (1 => (vertices (1)(1), vertices (1)(2), vertices (1)(3)),
                                      2 => (vertices (2)(1), vertices (2)(2), vertices (2)(3)),
                                      3 => (vertices (3)(1), vertices (3)(2), vertices (3)(3)));
                  end set_the_Vertices;

               begin
                  if        Self.m_flipQuadEdges
                    or else (       Self.m_useDiamondSubdivision
                             and then (interfaces.Unsigned_32 (j + x) and 1)  =  0)
                  then
                     --  first triangle
                     Self.getVertex (x,     j,  vertices (1));
                     Self.getVertex (x + 1,   j,  vertices (2));
                     Self.getVertex (x + 1, j + 1,  vertices (3));

                     set_the_Vertices;
                     callback.processTriangle (the_Vertices'Access, x, j);

                     --  second triangle
                     Self.getVertex (x,     j,  vertices (1));
                     Self.getVertex (x + 1, j + 1,  vertices (2));
                     Self.getVertex (x,   j + 1,  vertices (3));

                     set_the_Vertices;
                     callback.processTriangle (the_Vertices'Access, x, j);

                  else
                     --  first triangle
                     Self.getVertex (x,     j,  vertices (1));
                     Self.getVertex (x,   j + 1,  vertices (2));
                     Self.getVertex (x + 1,   j,  vertices (3));

                     set_the_Vertices;
                     callback.processTriangle (the_Vertices'Access, x, j);

                     --  second triangle
                     Self.getVertex (x + 1,   j,  vertices (1));
                     Self.getVertex (x,   j + 1,  vertices (2));
                     Self.getVertex (x + 1, j + 1,  vertices (3));

                     set_the_Vertices;
                     callback.processTriangle (the_Vertices'Access, x, j);
                  end if;
               end;

               x := x + 1;
            end loop;

            j := j + 1;
         end loop;
      end;
   end processAllTriangles;








   procedure setUseDiamondSubdivision (Self :    out Item;   useDiamondSubdivision : in     Boolean := True)
   is
   begin
      Self.m_useDiamondSubdivision := useDiamondSubdivision;
   end setUseDiamondSubdivision;







   --  This returns the "raw" (user's initial) height, not the actual height.
   --  The actual height needs to be adjusted to be relative to the center of the heightfield's AABB.
   --
   function  getRawHeightFieldValue (Self : in Item;   x, y    : in     Integer) return math.Real
   is
   begin
      return Self.m_heightfieldDataFloat (((y - 1) * Self.m_heightStickWidth) + x);
   end getRawHeightFieldValue;









   --  Given input vector, return quantized version
   --
   --    This routine is basically determining the gridpoint indices for a given
   --    input vector, answering the question: "which gridpoint is closest to the
   --    provided point?".
   --
   --    "with clamp" means that we restrict the point to be in the heightfield's
   --    axis-aligned bounding box.
   --
   procedure quantizeWithClamp (Self : in Item;   the_Out :    out Math.Integers;
                                                  point   : in     math.Vector_3;
                                                  isMax   : in     Boolean    )
   is
      pragma Unreferenced (isMax);
      use impact.d3.Vector;

      clampedPoint : math.Vector_3 := point;

   begin
      setMax (clampedPoint, Self.m_localAabbMin);
      setMin (clampedPoint, Self.m_localAabbMax);

      the_Out (1) := getQuantized (clampedPoint (1));
      the_Out (2) := getQuantized (clampedPoint (2));
      the_Out (3) := getQuantized (clampedPoint (3));
   end quantizeWithClamp;







   --  This returns the vertex in bullet-local coordinates.
   --
   procedure getVertex (Self : in Item;   x, y    : in     Integer;
                                          vertex  :    out math.Vector_3)
   is
      use impact.d3.Vector;

      pragma Assert (x >= 1);
      pragma Assert (y >= 0);
      pragma Assert (x <  Self.m_heightStickWidth);
      pragma Assert (y < Self.m_heightStickLength);

      height : constant math.Real := Self.getRawHeightFieldValue (x, y);

   begin
      case Self.m_upAxis
      is
      when 0 =>
         vertex := (height - Self.m_localOrigin (1),
                    (-Self.m_width   / 2.0) + math.Real (x),
                    (-Self.m_length  / 2.0) + math.Real (y));

      when 1 =>
         vertex := ((-Self.m_width  / 2.0) + math.Real (x),
                    height - Self.m_localOrigin (2),
                    (-Self.m_length / 2.0) + math.Real (y));

      when 2 =>
         vertex := ((-Self.m_width  / 2.0) + math.Real (x),
                    (-Self.m_length / 2.0) + math.Real (y),
                    height - Self.m_localOrigin (3));

      when others =>
         raise Program_Error;
      end case;


      vertex := Scaled (vertex, by => Self.m_localScaling);
   end getVertex;










   procedure initialize (Self : in out Item;   heightStickWidth, heightStickLength : in     Integer;
                                               heightfieldData                     : access math.Vector;
                                               heightScale                         : in     math.Real;
                                               minHeight, maxHeight                : in     math.Real;
                                               upAxis                              : in     Integer;
                                               flipQuadEdges                       : in     Boolean)
   is
      use math.Vectors;

      --  validation
      --
      pragma Assert (heightStickWidth  > 1);
      pragma Assert (heightStickLength > 1);
      pragma Assert (heightfieldData  /= null);
      pragma Assert (minHeight        <= maxHeight);
      pragma Assert (       upAxis  >= 1
                     and then upAxis  < 4);
   begin
      --  initialize member variables
      --
      Self.setShapeType               (impact.d3.collision.Proxy.TERRAIN_SHAPE_PROXYTYPE);
      Self.m_heightStickWidth       := heightStickWidth;
      Self.m_heightStickLength      := heightStickLength;
      Self.m_minHeight              := minHeight;
      Self.m_maxHeight              := maxHeight;
      Self.m_width                  := math.Real (heightStickWidth  - 1);
      Self.m_length                 := math.Real (heightStickLength - 1);
      Self.m_heightScale            := heightScale;
      Self.m_heightfieldDataFloat   := heightfieldData;
--        Self.m_heightDataType         := hdt;
      Self.m_flipQuadEdges          := flipQuadEdges;
      Self.m_useDiamondSubdivision  := False;
      Self.m_upAxis                 := upAxis;
      Self.m_localScaling           := (1.0, 1.0, 1.0);


      --  determine min/max axis-aligned bounding box (aabb) values
      --
      case Self.m_upAxis
      is
      when 1 =>
         Self.m_localAabbMin := (Self.m_minHeight,                0.0,              0.0);
         Self.m_localAabbMax := (Self.m_maxHeight,       Self.m_width,    Self.m_length);

      when 2 =>
         Self.m_localAabbMin := (           0.0,   Self.m_minHeight,              0.0);
         Self.m_localAabbMax := (  Self.m_width,   Self.m_maxHeight,    Self.m_length);

      when 3 =>
         Self.m_localAabbMin := (           0.0,                0.0, Self.m_minHeight);
         Self.m_localAabbMax := (  Self.m_width,      Self.m_length, Self.m_maxHeight);

      when others =>
         raise Program_Error;
      end case;


      --  remember origin (defined as exact middle of aabb)
      --
      Self.m_localOrigin := 0.5 * (Self.m_localAabbMin + Self.m_localAabbMax);
   end initialize;



end impact.d3.Shape.concave.height_field_terrain;
