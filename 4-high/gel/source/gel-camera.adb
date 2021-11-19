with
     gel.Sprite,
     openGL.Visual,
     ada.unchecked_Deallocation;


package body gel.Camera
is

   --------
   -- Forge
   --

   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



   --------------
   --  Operations
   --

   procedure render (Self : in out Item;   the_World : in gel.World.view;
                                           To        : in openGL.Surface.view)
   is
      all_Sprites : gel.World.sprite_transform_Pairs renames the_World.sprite_Transforms;

      the_Visuals : openGL.Visual.views (1 .. all_Sprites'Length);
      Count       : Natural := 0;

      the_Sprite  : gel.Sprite.view;
   begin
      for i in all_Sprites'Range
      loop
         the_Sprite := all_Sprites (i).Sprite;

         if         not the_Sprite.is_Destroyed
           and then     the_Sprite.is_Visible
         then
            Count := Count + 1;

            the_Visuals (Count)            := the_Sprite.Visual;
            the_Visuals (Count).Transform_is (all_Sprites (i).Transform);
            the_Visuals (Count).Scale_is     ((1.0, 1.0, 1.0));

            the_Visuals (Count).program_Parameters_are (the_Sprite.program_Parameters);
         end if;
      end loop;

      Self.render (the_Visuals (1 .. Count));
   end render;


end gel.Camera;
