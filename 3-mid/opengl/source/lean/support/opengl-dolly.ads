with
     openGL.Camera,
     ada.Characters.latin_1;

package openGL.Dolly
--
-- A utility which moves a camera via the keyboard.
--
is
   type Item (Camera : openGL.Camera.view) is tagged private;


   procedure Speed_is           (Self : in out Item;   Now : in Real);
   procedure evolve             (Self : in out Item);

   function  quit_Requested     (Self : in     Item) return Boolean;

   procedure get_last_Character (Self : in out Item;   the_Character : out Character;
                                                       Available     : out Boolean);


private

   type Item (Camera : openGL.Camera.view) is tagged
      record
         quit_Requested : Boolean   := False;
         last_Character : Character := ada.Characters.Latin_1.NUL;
         Speed          : Real      := 1.0;
      end record;

end openGL.Dolly;
