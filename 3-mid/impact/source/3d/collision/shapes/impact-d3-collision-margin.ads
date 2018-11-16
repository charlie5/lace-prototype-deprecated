
package impact.d3.collision.Margin
is


   CONVEX_DISTANCE_MARGIN : constant := 0.04;
   --
   --  The CONVEX_DISTANCE_MARGIN is a default collision margin for convex collision shapes derived from impact.d3.Shape.convex.internal.
   --
   --  This collision margin is used by Gjk and some other algorithms
   --  Note that when creating small objects, you need to make sure to set a smaller collision margin, using the 'setMargin' API


end impact.d3.collision.Margin;
