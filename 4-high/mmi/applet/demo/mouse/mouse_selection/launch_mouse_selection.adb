with mmi.Window.setup,
     mmi.Applet.gui_world,
     mmi.Camera,
     mmi.Mouse,

     mmi.Sprite,
     openGL.Model,
     openGL.Model.box,
     openGL.Model.sphere.lit_colored_textured,

     physics.Forge;

with opengl.Palette,
     float_Math.Algebra.linear.d3,
     lace.Response,
     lace.remote.Response,
     ada.Calendar;
with lace.event       .Utility,
     lace.event.remote.Utility,
     lace.event.remote.logger.text;
with lace.remote.Subject;
with lace.remote.Observer;
with ada.Text_IO;
with ada.Exceptions;
with mmi.Events;
with mmi.Forge;



procedure launch_mouse_Selection
--
-- Places a sphere sprite in the world and registers an event repsonse to
-- handle mouse clicks on the sprite.
--
is
   use lace.event       .Utility,
       lace.event.remote.Utility,
       ada.Text_IO;

   package Math renames float_Math;

begin
   lace.Event.remote.Utility.use_text_Logger ("event.log");
   lace.Event.remote.Utility.Logger.ignore (to_Kind (mmi.Mouse.motion_Event'Tag));


   declare
      use mmi.Applet,
          math.Algebra.linear.d3,
          lace.event.Utility,
          ada.Calendar;

      use type math.Real,  opengl.Real;

      the_Applet : mmi.Applet.gui_world.view := mmi.Forge.new_gui_Applet  ("mouse Selection",
                                                                           space_Kind => physics.Bullet);


      the_Ball : mmi.Sprite.view := mmi.Forge.new_ball_Sprite (the_Applet.World (1),
                                                                       mass => 0.0);


      type retreat_Sprite is new lace.Response.item with
         record
            Sprite : mmi.Sprite.view;
         end record;

      procedure respond (Self : in out retreat_Sprite;  to_Event : in lace.Event.Item'Class)
      is
         use float_Math;
      begin
         put_Line ("retreat_Sprite");
         Self.Sprite.Site_is (self.Sprite.Site - the_Applet.gui_Camera.world_Rotation * (0.0, 0.0, 1.0));
      end;

      retreat_Sprite_Response : aliased retreat_Sprite := (lace.Response.item with sprite => the_Ball);



      type advance_Sprite is new lace.Response.item with
         record
            Sprite : mmi.Sprite.view;
         end record;

      procedure respond (Self : in out advance_Sprite;  to_Event : in lace.Event.Item'Class)
      is
         use float_Math;
      begin
         put_Line ("advance_Sprite");
         Self.Sprite.Site_is (self.Sprite.Site + the_Applet.gui_Camera.world_Rotation * (0.0, 0.0, 1.0));
      end;

      advance_Sprite_Response : aliased advance_Sprite := (lace.Response.Item with sprite => the_Ball);


      next_render_Time : ada.calendar.Time;

   begin
      the_Ball.add (advance_Sprite_Response'unchecked_access,
                    to_Kind (mmi.events.sprite_click_down_Event'Tag),
                    the_Applet.Name);

      the_Ball.add (retreat_Sprite_Response'unchecked_access,
                    to_Kind (mmi.events.sprite_click_up_Event'Tag),
                    the_Applet.Name);

      the_Applet.gui_world .add      (the_Ball, and_Children => False);
      the_Applet.gui_Camera.Site_is  ((0.0, 0.0, 5.0));
      the_Applet.enable_simple_Dolly (in_World => 1);
      the_Applet.enable_Mouse (detect_Motion => False);


      next_render_Time := ada.calendar.Clock;

      while the_Applet.is_open loop
         the_Applet.gui_World.evolve (by => 1.0/60.0);
         the_Ball.respond;
         the_Applet.freshen;

         next_render_Time := next_render_Time + 1.0/60.0;
         delay until next_render_Time;
      end loop;

      the_Applet.destroy;
   end;

   lace.Event.remote.Utility.close;


exception
   when E : others =>
      lace.Event.remote.Utility.close;

      put_Line ("Exception detected in 'launch_mouse_Selection' ...");
      put_Line (ada.Exceptions.Exception_Information (E));
end launch_mouse_Selection;
