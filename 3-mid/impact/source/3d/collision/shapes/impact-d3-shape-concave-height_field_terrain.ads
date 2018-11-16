with impact.d3.Shape.concave;
with impact.d3.triangle_Callback;




package impact.d3.Shape.concave.height_field_terrain
--
--  Simulates a 2D heightfield terrain.
--
--    The caller is responsible for maintaining the heightfield array; this
--    class does not make a copy.
--
--    The heightfield can be dynamic so long as the min/max height values
--    capture the extremes (heights must always be in that range).
--
--    The local origin of the heightfield is assumed to be the exact
--    center (as determined by width and length and height, with each
--    axis multiplied by the localScaling).
--
--    \b NOTE: be careful with coordinates.  If you have a heightfield with a local
--    min height of -100m, and a max height of +500m, you may be tempted to place it
--    at the origin (0,0) and expect the heights in world coordinates to be
--    -100 to +500 meters.
--    Actually, the heights will be -300 to +300m, because bullet will re-center
--    the heightfield based on its AABB (which is determined by the min/max
--    heights).  So keep in mind that once you create a impact.d3.Shape.concave.height_field_terrain
--    object, the heights will be adjusted relative to the center of the AABB.  This
--    is different to the behavior of many rendering engines, but is useful for
--    physics engines.
--
--    Most (but not all) rendering and heightfield libraries assume upAxis = 1
--    (that is, the y-axis is "up").  This class allows any of the 3 coordinates
--    to be "up".  Make sure your choice of axis is consistent with your rendering
--    system.
--
--    The heightfield heights are determined from the data type used for the
--    heightfieldData array.
--
--     - PHY_UCHAR: height at a point is the uchar value at the
--         grid point, multipled by heightScale.  uchar isn't recommended
--         because of its inability to deal with negative values, and
--         low resolution (8-bit).
--
--     - PHY_SHORT: height at a point is the short int value at that grid
--         point, multipled by heightScale.
--
--     - PHY_FLOAT: height at a point is the float value at that grid
--         point.  heightScale is ignored when using the float heightfield
--         data type.
--
--    Whatever the caller specifies as minHeight and maxHeight will be honored.
--    The class will not inspect the heightfield to discover the actual minimum
--    or maximum heights.  These values are used to determine the heightfield's
--    axis-aligned bounding box, multiplied by localScaling.
--
--    For usage and testing see the TerrainDemo.
--
is


   type Item is new impact.d3.Shape.concave.Item with private;






   ----------
   --- Forge
   --

   function to_height_field_terrain_Shape (heightStickWidth,
                                           heightStickLength    : in     Integer  ;
                                           heightfieldData      : access math.Vector;
                                           heightScale          : in     math.Real;
                                           minHeight, maxHeight : in     math.Real;
                                           upAxis               : in     Integer  ;
                                           flipQuadEdges        : in     Boolean  ) return Item;
   --
   --  This constructor supports a range of heightfield data types, and allows for a non-zero minimum height value.
   --
   --  'heightScale' is needed for any integer-based heightfield data types.



   overriding procedure destruct (Self : in out Item);






   ---------------
   --- Attributes
   --

   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                                        aabbMin, aabbMax :    out math.Vector_3 );

   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3);
   overriding function  getLocalScaling (Self : in     Item)         return math.Vector_3;


   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);


   overriding function  getName   (Self : in     Item)        return String;

--          virtual const char*        getName()const {return "HEIGHTFIELD";}





   overriding procedure processAllTriangles      (Self : in     Item;   callback              : access impact.d3.triangle_Callback.Item'Class;
                                                             aabbMin, aabbMax      : in     math.Vector_3);

   procedure setUseDiamondSubdivision (Self :    out Item;   useDiamondSubdivision : in     Boolean := True);

--          void setUseDiamondSubdivision(bool useDiamondSubdivision=true) { m_useDiamondSubdivision = useDiamondSubdivision;}






private

   type Item is new impact.d3.Shape.concave.Item with
      record
         m_localAabbMin          : math.Vector_3;
         m_localAabbMax          : math.Vector_3;
         m_localOrigin           : math.Vector_3;

         --  terrain data
         m_heightStickWidth      : Integer;
         m_heightStickLength     : Integer;

         m_minHeight             : math.Real;
         m_maxHeight             : math.Real;
         m_width                 : math.Real;
         m_length                : math.Real;
         m_heightScale           : math.Real;

         m_heightfieldDataFloat  : access math.Vector;
--           m_heightDataType        : PHY_ScalarType;

         m_flipQuadEdges         : Boolean;
         m_useDiamondSubdivision : Boolean;

         m_upAxis                : Integer;
         m_localScaling          : math.Vector_3;
      end record;




   function  getRawHeightFieldValue (Self : in Item;   x, y    : in     Integer) return math.Real;

   procedure quantizeWithClamp (Self : in Item;   the_Out :    out Math.Integers   ;
                                                       point   : in     math.Vector_3;
                                isMax   : in     Boolean    );

   procedure getVertex              (Self : in Item;   x, y    : in     Integer;
                                                       vertex  :    out math.Vector_3);



   procedure initialize (Self : in out Item;   heightStickWidth, heightStickLength : in     Integer;
                                               heightfieldData                     : access math.Vector;
                                               heightScale                         : in     math.Real;
                                               minHeight, maxHeight                : in     math.Real;
                                               upAxis                              : in     Integer;
                         flipQuadEdges                       : in     Boolean);
   --
   --  Handles the work of constructors so that public constructors can be
   --  backwards-compatible without a lot of copy/paste.


end impact.d3.Shape.concave.height_field_terrain;
