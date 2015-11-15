with
     mmi.Applet.gui_World,
     mmi.Window.lumen,
     mmi.Camera,
     mmi.Forge,
     mmi.Sprite,
     mmi.physics_Model,

     openGL.Model.open_gl,

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
   use mmi.Applet,
       ada.Calendar, ada.Text_IO, ada.Exceptions;

   package Math renames float_Math;
   use type math.Real;


   the_Applet : constant mmi.Applet.gui_World.view := mmi.Forge.new_gui_Applet ("openGL Model", 500, 500);


   the_human_graphics_Model : aliased openGL.Model.open_gl.item
     := (openGL.Model.item with
         model   => openGL.to_Asset ("./assets/opengl/model/human.obj"),
         texture => openGL.null_Asset,
         has_lucid_Texture => False,
         others => <>);

   the_human_physics_Model : constant mmi.physics_Model.view
     := mmi.physics_Model.Forge.new_physics_Model (shape_Info => (kind         => mmi.physics_Model.Cube,
                                                                  half_extents => (4.0, 1.0, 2.0)),
                                                   mass       => 1.0);


   the_Human : mmi.Sprite.view
     := mmi.Sprite.forge.new_Sprite (Name           => "Clarence",
                                     World          => the_Applet.gui_World,
                                     graphics_Model => the_human_graphics_Model'unchecked_Access,
                                     physics_Model  => the_human_physics_Model);

   the_Ground : constant mmi.Sprite.view     := mmi.Forge.new_box_Sprite  (the_Applet.gui_World,
                                                                                 mass => 0.0,
                                                                                 size => (50.0, 1.0, 50.0));

   next_render_Time : ada.calendar.Time;

begin
   the_Applet.gui_World.Gravity_is ((0.0, 0.0, 0.0));

   the_Applet.gui_World.add (the_Human);               -- Add human.
   the_Human.Site_is ((0.0,  0.0,  0.0));              --

   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 18.0));   -- Position the camera.
   the_Applet.enable_simple_Dolly (in_World => 1);     -- Enable user camera control via keyboards.


   next_render_Time := ada.Calendar.clock;

   while the_Applet.is_open
   loop
      the_Applet.gui_World.evolve (by => 1.0/60.0);    -- Evolve the world.
      the_Applet.freshen;                              -- Handle any new events and update the screen.

      next_render_Time := next_render_Time + 1.0/60.0;
      delay until next_render_Time;
   end loop;


   the_Applet.destroy;

exception
   when E : others =>
      put_Line (Exception_Information (E));
end launch_opengl_Model;
