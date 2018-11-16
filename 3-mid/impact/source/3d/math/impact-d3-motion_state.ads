

package impact.d3.motion_State
--
--  The impact.d3.motion_State interface class allows the dynamics world to synchronize and interpolate the updated world transforms with graphics.
--
--  For optimizations, potentially only moving objects get synchronized (using setWorldPosition/setWorldOrientation)
--
is
   use Math;



   type Item is abstract tagged null record;


   procedure destruct (Self : in out Item)   is null;

   procedure getWorldTransform (Self : in     Item;   worldTrans :    out Transform_3d)   is abstract;
   procedure setWorldTransform (Self : in out Item;   worldTrans : in     Transform_3d)   is abstract;
   --
   --  Bullet only calls the update of worldtransform for active objects.


end impact.d3.motion_State;
