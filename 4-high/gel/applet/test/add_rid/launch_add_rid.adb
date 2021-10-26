with
     gel.Window.sdl,
     gel.Applet.gui_world,
     gel.Forge,
     gel.Sprite,
     gel.Joint,

     physics.Forge,

     opengl.Palette,
     float_math.Algebra.linear.d3,

     ada.Text_IO,
     ada.Exceptions;

pragma Unreferenced (gel.Window.sdl);



procedure launch_add_rid
--
--  Creates a chain of balls in a 2d space.
--
is
   package Math renames float_Math;

   use GEL,
       gel.Forge,
       gel.Applet,
       opengl.Palette,
       gel.Math,
       gel.linear_Algebra_2D,
       ada.Text_IO;

   use type openGL.Real;

   type Tests is (None,
                  add_rid_Joint,
                  add_rid_Object,
                  destroy_Object);

--     my_Test : Tests := None;
   my_Test : Tests := add_rid_Joint;
--     my_Test : Tests := add_rid_Object;
--     my_Test : Tests := destroy_Object;


   the_Applet : gel.Applet.gui_World.view := new_gui_Applet     ("Add/Rid Test",
                                                                 1536, 864,
                                                                 space_Kind => physics.Box2D);
   the_Ground : gel.Sprite          .view := new_rectangle_Sprite (the_Applet.gui_World,
                                                                   mass   =>   0.0,
                                                                   width  => 100.0,
                                                                   height =>   1.0,
                                                                   color  => apple_Green);
begin
   the_Applet.gui_World .Gravity_is    ((0.0, -10.0,   0.0));
   the_Applet.gui_Camera.Site_is       ((0.0, -30.0, 100.0));
   the_Applet.Renderer  .Background_is (Grey);
   the_Applet.enable_simple_Dolly      (in_world => gui_world.gui_world_Id);

   the_Ground.Site_is ((0.0, -40.0, 0.0));
   the_Applet.gui_World.add (the_Ground, and_Children => False);


   --  Add joints.
   --
   declare
      use Math, math.Algebra.linear.d3, math.Vectors;

      ball_Count      : constant                       := 39; -- 256;
      the_root_Ball   : constant gel.Sprite.view       :=                         new_circle_Sprite (the_Applet.gui_World, mass =>  0.0);
      the_Balls       : constant gel.Sprite.views      := (1 .. ball_Count - 1 => new_circle_Sprite (the_Applet.gui_World, mass =>  1.0),
                                                           ball_Count          => new_circle_Sprite (the_Applet.gui_World, mass => 10.0));

      mid_Ball_Id     : constant Index                 := Index (the_Balls'First + the_Balls'Last) / 2;
      mid_Ball        :          gel.Sprite.view  renames the_Balls (mid_Ball_Id);
      mid_Ball_initial_Offset
                      :          Vector_3;
   begin
      --  the_root_Ball.Site_is ((0.0,  0.0,  0.0));

      declare
         Frame_A   : constant math.Matrix_4x4       := math.Identity_4x4;
         Frame_B   : constant math.Matrix_4x4       := math.Identity_4x4;

         Parent    :          gel.Sprite.view := the_root_Ball;
         new_Joint :          gel.Joint .view;
      begin
         for i in the_Balls'Range
         loop
            the_Balls (i).Site_is ((Real (-i),  0.0,  0.0));

--              Parent.attach_via_Hinge (the_Child         => the_Balls (i),
--                                       Frame_in_parent   => Frame_A,
--                                       Frame_in_child    => Frame_B,
--                                       Limits            => (to_Radians (-180.0),
--                                                             to_Radians ( 180.0)),
--                                       collide_Connected => False,
--                                       new_joint         => new_Joint);

            Parent.attach_via_Hinge (the_Child  => the_Balls (i),
                                     pivot_Axis => (0.0, 0.0, 1.0),
                                     low_Limit  => to_Radians (-180.0),
                                     high_Limit => to_Radians ( 180.0),
                                     new_joint  => new_Joint);

            if i = mid_Ball_Id then
               mid_Ball_initial_Offset := the_Balls (i).Site - Parent.Site;
            end if;

            Parent := the_Balls (i);
         end loop;
      end;

      the_Applet.gui_World.add (the_root_Ball, and_Children => True);


      declare
         Counter    : Natural := 0;
         Added      : Boolean := True;
      begin
         while the_Applet.is_open
         loop
            Counter := Counter + 1;

            if false -- Counter mod (2 * 60) = 0
            then

               if Added then
                  case my_Test
                  is
                     when None =>
                        null;

                     when add_rid_Joint =>
                        the_Applet.gui_World.rid (mid_Ball.parent_Joint);

                     when add_rid_Object =>
--                          the_Applet.gui_World.rid (mid_Ball.parent_Joint);
                        the_Applet.gui_World.rid (mid_Ball, and_children => False);

                     when destroy_Object =>
                        the_Applet.gui_World.rid (mid_Ball, and_children => False);
                        the_Applet.gui_World.destroy (mid_Ball);
                        my_Test := None;
                  end case;

                  Added := False;

               else
                  case my_Test
                  is
                     when None =>
                        null;

                     when add_rid_Joint =>
                        mid_Ball.move (to_site =>  mid_Ball.parent_Joint.Sprite_A.Site
                                                 + mid_Ball_initial_Offset);
                        the_Applet.gui_World.add (mid_Ball.parent_Joint);

                     when add_rid_Object =>
--                          mid_Ball.move (to_site =>  mid_Ball.parent_Joint.Sprite_A.Site
--                                                   + mid_Ball_initial_Offset);
                        the_Applet.gui_World.add (mid_Ball, and_children => False);
--                          the_Applet.gui_World.add (mid_Ball.parent_Joint);

                     when destroy_Object =>
                        null;
                  end case;

                  Added := True;
               end if;

            end if;

            the_Applet.freshen;     -- Handle any new events and update the screen.
         end loop;
      end;
   end;

   gel.Applet.gui_world.free (the_Applet);


exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_add_rid;
