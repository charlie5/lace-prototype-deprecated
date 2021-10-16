with
     gel.Applet.gui_World,
     gel.Window.lumen,
     gel.Camera,
     gel.Forge,
     gel.Sprite,
     physics.Model,

     openGL.Model.any,

     float_Math,
     ada.Calendar,
     ada.Text_IO,
     ada.Exceptions;


procedure launch_opengl_Model
--
-- Shows a human head model imported from a wavefront '.obj' file
--   and a human body model imported from a collada   '.dae' file.
--
--
is
   use gel.Applet,
       ada.Calendar, ada.Text_IO, ada.Exceptions;

   package Math renames float_Math;
   use type math.Real;


   the_Applet : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("openGL Model", 500, 500);


   the_human_graphics_Model : aliased openGL.Model.any.item
     := (openGL.Model.item with
         model   => openGL.to_Asset ("./assets/opengl/model/human.obj"),
         texture => openGL.null_Asset,
         has_lucid_Texture => False,
         others => <>);

   the_human_physics_Model : constant physics.Model.view
     := physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => physics.Model.Cube,
                                                              half_Extents => (4.0, 1.0, 2.0)),
                                               Mass       => 1.0);


   the_Human : gel.Sprite.view
     := gel.Sprite.forge.new_Sprite (Name           => "Clarence",
                                     World          => the_Applet.gui_World.all'Access,
                                     at_Site        => gel.Math.Origin_3D,
                                     graphics_Model => the_human_graphics_Model'unchecked_Access,
                                     physics_Model  => the_human_physics_Model);

   the_Ground : constant gel.Sprite.view := gel.Forge.new_box_Sprite (the_Applet.gui_World,
                                                                      Mass => 0.0,
                                                                      Size => (50.0, 1.0, 50.0));
   next_render_Time : ada.calendar.Time;

begin
   the_Applet.gui_World.Gravity_is ((0.0, 0.0, 0.0));

   the_Applet.gui_World.add (the_Ground);              -- Add ground.

   the_Applet.gui_World.add (the_Human);               -- Add human.
   the_Human.Site_is ((0.0,  5.0,  0.0));              --

   the_Applet.gui_Camera.Site_is ((0.0, 1.5, 2.6));  -- Position the camera.
   the_Applet.enable_simple_Dolly (in_World => 1);     -- Enable user camera control via keyboards.
   the_Applet.Dolly.Speed_is (0.02);                   -- Slow down the rate at which the dolly moves.


   next_render_Time := ada.Calendar.clock;

   while the_Applet.is_open
   loop
      the_Applet.freshen;                              -- Evolve the world, handle any new events and update the display.

      next_render_Time := next_render_Time + 1.0/60.0;
      delay until next_render_Time;
   end loop;


   the_Applet.destroy;

exception
   when E : others =>
      put_Line (Exception_Information (E));
end launch_opengl_Model;
