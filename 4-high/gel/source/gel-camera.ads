with
     gel.World,

     openGL.Surface,
     openGL.Camera,
     openGL.Renderer.lean;

package gel.Camera
--
-- Models a camera.
--
is
   type Item  is new openGL.Camera.item with private;
   type View  is access all Camera.item'Class;

   type Views is array (Positive range <>) of View;

   ---------
   --  Forge
   --

   procedure free    (Self : in out View);


   --------------
   --  Operations
   --

   procedure render         (Self : in out Item;   the_World : in gel.World.view;
                                                   To        : in openGL.Surface.view);


private

   type Item  is new openGL.Camera.item with
      record
         null;
      end record;


end gel.Camera;
