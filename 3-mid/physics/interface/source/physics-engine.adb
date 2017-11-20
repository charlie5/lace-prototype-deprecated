with
     physics.Forge,
     ada.unchecked_Deallocation,
     ada.Containers,
     ada.Calendar,
     ada.Text_IO;
with Ada.Exceptions;


package body physics.Engine
is
   use ada.Text_IO;


   protected body safe_command_Set
   is
      function  is_Empty return Boolean
      is
      begin
         return the_Count = 0;
      end is_Empty;


      procedure add   (the_Command : in     Command)
      is
      begin
         the_Count       := the_Count + 1;
         Set (the_Count) := the_Command;
      end add;


      procedure Fetch (To    :    out Commands;
                       Count :    out Natural)
      is
      begin
         To (1 .. the_Count) := Set (1 .. the_Count);
         Count               := the_Count;
         the_Count           := 0;
      end Fetch;
   end safe_command_Set;



   task body Evolver is
      use Math;
      use type Physics.Joint.view,
               ada.Containers.Count_Type;

      Stopped          : Boolean                   := True;
      Cycle            : ada.Containers.Count_Type := 0;
      next_render_Time : ada.Calendar.Time;

      max_joint_Force,
      max_joint_Torque : Real := 0.0;


      procedure free_Sprites
      is
--           the_free_Sprites : mmi.Object.views := the_World.free_sprite_Set;
      begin
--           for Each in the_free_Sprites'Range
--           loop
--              log ("Engine is Freeing sprite id: " & sprite_Id'Image (the_free_Sprites (Each).Id));
--
--              if the_free_Sprites (Each).owns_Graphics
--              then
--                 the_World.Renderer.free (the_free_Sprites (Each).Visual.Model);
--              end if;
--
--              mmi.Sprite.free (the_free_Sprites (Each));
--           end loop;
         null;

      end free_Sprites;


      procedure evolve
      is
      begin
         Cycle := Cycle + 1;

         do_engine_Commands:
         declare
            the_Commands  : Commands;
            Count         : Natural;

            command_Count : array (command_Kind) of Natural := (others => 0);

         begin
            Self.Commands.fetch (the_Commands, Count);

            for Each in 1 .. Count
            loop
               declare
                  the_Command : Command renames the_Commands (Each);
               begin
                  command_Count (the_Command.Kind) := command_Count (the_Command.Kind) + 1;

                  case the_Command.Kind
                  is
                     when scale_Object =>
                        the_Command.Sprite.activate;
                        the_Command.Sprite.Shape.Scale_is (the_Command.Scale);
                        the_Command.Sprite.Scale_is (the_Command.Scale);

                        Self.Space.update_Bounds (Object.view (the_Command.Sprite));


                     when update_Bounds =>
                        Self.Space.update_Bounds (Object.view (the_Command.Sprite));


                     when update_Site =>
                        the_Command.Sprite.Site_is (the_Command.Site);


                     when set_Speed =>
                        the_Command.Sprite.Speed_is (the_Command.Speed);


                     when set_xy_Spin =>
                        the_Command.Sprite.xy_Spin_is (the_Command.xy_Spin);


                     when add_Sprite =>
                        declare
                           procedure add (the_Sprite : in Object.view)
                           is
                           begin
--                                the_World.add (the_Sprite. physics_Model.all'Access);

--                                if the_Sprite.physics_Model.is_Tangible
--                                then
                                 Self.Space.add (the_Sprite);
--                                end if;

--                                begin
--                                   the_sprite_Transforms.insert  (the_Sprite, Identity_4x4);
--                                   the_Sprite.Solid.user_Data_is (the_Sprite);
--                                end;

--                                the_World.sprite_Count                     := the_World.sprite_Count + 1;
--                                the_World.Sprites (the_World.sprite_Count) := the_Sprite;
                           end add;

                        begin
                           add (the_Command.Sprite);
                        end;


                     when rid_Sprite =>
                        declare
                           function find (the_Sprite : in Object.view) return Index
                           is
                           begin
--                                for Each in 1 .. the_World.sprite_Count
--                                loop
--                                   if the_World.Sprites (Each) = the_Sprite
--                                   then
--                                      return Each;
--                                   end if;
--                                end loop;

                              raise constraint_Error with "no such sprite in world";
                              return 0;
                           end find;


                           procedure rid (the_Sprite : in Object.view)
                           is
                              use type Object.view;
                           begin
                              if the_Sprite /= null
                              then
--                                   if the_Sprite.physics_Model.is_Tangible
--                                   then
                                    Self.Space.rid (Object.view (the_Sprite));
--                                   end if;

--                                   if the_sprite_Transforms.contains (the_Sprite) then
--                                      the_sprite_Transforms.delete   (the_Sprite);
--                                   end if;

                              else
                                 raise program_Error;
                              end if;


                              declare
                                 use type math.Index;
                                 Id : Index;
                              begin
                                 Id := find (the_Sprite);

--                                   if Id <= the_World.sprite_Count
--                                   then
--                                      the_World.Sprites (1 .. the_World.sprite_Count - 1)
--                                        :=   the_World.Sprites (     1 .. Id - 1)
--                                           & the_World.Sprites (Id + 1 .. the_World.sprite_Count);
--                                   end if;

--                                   the_World.sprite_Count := the_World.sprite_Count - 1;
                              end;
                           end rid;

                        begin
                           rid (the_Command.Sprite);
                        end;


                     when apply_Force =>
                        the_Command.Sprite.apply_Force (the_Command.Force);


                     when destroy_Sprite =>
                        declare
--                             the_free_Set : free_Set renames the_World.free_Sets (the_World.current_free_Set);
                        begin
                           raise Program_Error with "destroy_Sprite ~ TODO";
--                             the_free_Set.Count                        := the_free_Set.Count + 1;
--                             the_free_Set.Sprites (the_free_Set.Count) := the_Command.Sprite;
                        end;


                     when add_Joint =>
                        Self.Space.add                 (the_Command.Joint.all'Access);
                        the_Command.Joint.user_Data_is (the_Command.Joint);


                     when rid_Joint =>
                        Self.Space.rid (the_Command.Joint.all'Access);


                     when set_Joint_local_Anchor =>
                        Self.Space.set_Joint_local_Anchor (the_Command.anchor_Joint.all'Access,
                                                           the_Command.is_Anchor_A,
                                                           the_Command.local_Anchor);

                     when free_Joint =>
--                          Joint.free (the_Command.Joint);
                        null;


                     when cast_Ray =>
                        null;
--                          declare
--                             function cast_Ray (Self : in Item'Class;   From, To : in math.Vector_3) return ray_Collision
--                             is
--                                use type std_physics.Object.view;
--
--                                physics_Collision : constant standard.physics.Space.ray_Collision := Self.physics.cast_Ray (From, To);
--                             begin
--                                if physics_Collision.near_Object = null
--                                then
--                                   return ray_Collision' (near_sprite => null,
--                                                          others      => <>);
--                                else
--                                   return ray_Collision' (to_MMI (physics_Collision.near_Object),
--                                                          physics_Collision.hit_Fraction,
--                                                          physics_Collision.Normal_world,
--                                                          physics_Collision.Site_world);
--                                end if;
--                             end cast_Ray;
--
--                             the_Collision : constant ray_Collision := cast_Ray (the_World.all,
--                                                                                 the_Command.From,
--                                                                                 the_Command.To);
--                          begin
--                             if        the_Collision.near_Sprite = null
--                               or else the_Collision.near_Sprite.is_Destroyed
--                             then
--                                free (the_Command.Context);
--
--                             else
--                                declare
--                                   no_Params : aliased no_Parameters;
--                                   the_Event :         raycast_collision_Event'Class
--                                     := raycast_collision_Event_dispatching_Constructor (the_Command.event_Kind,
--                                                                                         no_Params'Access);
--                                begin
--                                   the_Event.near_Sprite := the_Collision.near_Sprite;
--                                   the_Event.Context     := the_Command.Context;
--                                   the_Event.Site_world  := the_Collision.Site_world;
--
--                                   the_Command.Observer.receive (the_Event, from_subject => the_World.Name);
--                                end;
--                             end if;
--                          end;


                     when set_Gravity =>
                        Self.Space.Gravity_is (the_Command.Gravity);
                  end case;
               end;
            end loop;
         end do_engine_Commands;

         Self.Space.evolve (by => 1.0 / 60.0);     -- Evolve the world.
--           free_Sprites;
      end evolve;

      use ada.Calendar;
      use type Space.view;

   begin
--        accept start (space_Kind : in physics.space_Kind)
      accept start (the_Space : in Space.view)
      do
         Stopped    := False;

--           Self.Space := physics.Forge.new_Space (space_Kind);
         Self.Space := the_Space;
      end start;


      next_render_Time := ada.Calendar.Clock;

      loop
         select
            accept stop
            do
               Stopped := True;


               -- Add 'destroy' commands for all sprites.
               --
--                 declare
--                    the_Sprites : Sprite.views renames the_World.Sprites;
--                 begin
--                    for i in 1 .. the_World.sprite_Count
--                    loop
--                       the_Sprites (i).destroy (and_Children => False);
--                    end loop;
--                 end;

               -- Evolve the world til there are no commands left.
               --
               while not Self.Commands.is_Empty
               loop
                  evolve;
               end loop;

               -- Free both sets of freeable sprites.
               --
               free_Sprites;
               free_Sprites;
            end stop;

            exit when Stopped;

         or
            accept reset_Age
            do
               Self.Age := 0.0;
            end reset_Age;

         else
            null;
         end select;


         evolve;


--           the_World.new_sprite_transforms_Available.signal;
--           the_World.evolver_Done                   .signal;


         -- Check for joint breakage.
         --
--           if the_World.broken_joints_Allowed
--           then
--              declare
--                 use mmi.Joint,
--                     standard.physics.Space;
--
--                 the_Joint       : mmi.Joint.view;
--                 reaction_Force,
--                 reaction_Torque : math.Real;
--
--                 Cursor          : standard.physics.Space.joint_Cursor'Class := the_World.Physics.first_Joint;
--              begin
--                 while has_Element (Cursor)
--                 loop
--                    the_Joint := to_MMI (Element (Cursor));
--
--                    if the_Joint /= null
--                    then
--                       reaction_Force  := abs (the_Joint.reaction_Force);
--                       reaction_Torque := abs (the_Joint.reaction_Torque);
--
--                       if   reaction_Force  >  50.0 / 8.0
--                         or reaction_Torque > 100.0 / 8.0
--                       then
--                          begin
--                             the_World.Physics      .rid (the_Joint.Physics.all'Access);
--                             the_World.broken_Joints.add (the_Joint);
--
--                          exception
--                             when no_such_Child =>
--                                put_Line ("Error when breaking joint due to reaction Force:  no_such_Child !");
--                          end;
--                       end if;
--
--                       if reaction_Force > max_joint_Force
--                       then
--                          max_joint_Force := reaction_Force;
--                       end if;
--
--                       if reaction_Torque > max_joint_Torque
--                       then
--                          max_joint_Torque := reaction_Torque;
--                       end if;
--                    end if;
--
--                    next (Cursor);
--                 end loop;
--              end;
--           end if;

         next_render_Time := next_render_Time + Duration (1.0 / 60.0);
      end loop;

   exception
      when E : others =>
         new_Line (2);
         put_Line ("Error in physics.Engine.evolver task !");
         new_Line;
         put_Line (Ada.Exceptions.Exception_Information (E));
         put_Line ("Evolver has terminated !");
         new_Line (2);
   end Evolver;



--     procedure start (Self : access Item;   space_Kind : in physics.space_Kind)
   procedure start (Self : access Item;   the_Space : in Space.view)
   is
   begin
      Self.Evolver.start (the_Space);
   end start;


   procedure stop (Self : access Item)
   is
      procedure free is new ada.Unchecked_Deallocation (safe_command_Set, safe_command_Set_view);
   begin
      Self.Evolver.stop;
      free (Self.Commands);
   end stop;



   procedure add (Self : access Item;   the_Sprite   : in Object.view)
   is
   begin
      Self.Commands.add ((kind         => add_Sprite,
                          sprite       => the_Sprite,
                          add_children => False));
   end add;


   procedure rid (Self : in out Item;   the_Sprite   : in Object.view)
   is
   begin
      Self.Commands.add ((kind         => rid_Sprite,
                          sprite       => the_Sprite,
                          rid_children => False));
   end rid;



   procedure add (Self : in out Item;   the_Sprite   : in Joint.view)
   is
   begin
      Self.Commands.add ((kind   => add_Joint,
                          sprite => null,
                          joint  => the_Sprite));
   end add;


   procedure rid (Self : in out Item;   the_Sprite   : in Joint.view)
   is
   begin
      Self.Commands.add ((kind   => rid_Joint,
                          sprite => null,
                          joint  => the_Sprite));
   end rid;




   procedure update_Scale (Self : in out Item;   of_Sprite : in Object.view;
                                                 To        : in math.Vector_3)
   is
   begin
      Self.Commands.add ((kind   => scale_Object,
                          sprite => of_Sprite,
                          scale  => To));
   end update_Scale;



   procedure apply_Force (Self : in out Item;   to_Sprite : in Object.view;
                                                Force     : in math.Vector_3)
   is
   begin
      Self.Commands.add ((kind   => apply_Force,
                          sprite => to_Sprite,
                          force  => Force));
   end apply_Force;




   procedure update_Site (Self : in out Item;   of_Sprite : in Object.view;
                                                To        : in math.Vector_3)
   is
   begin
      Self.Commands.add ((kind   => update_Site,
                          sprite => of_Sprite,
                          site   => To));
   end update_Site;



   procedure set_Speed (Self : in out Item;   of_Sprite : in Object.view;
                                              To        : in math.Vector_3)
   is
   begin
      Self.Commands.add ((kind   => set_Speed,
                          sprite => of_Sprite,
                          speed  => To));
   end set_Speed;



   procedure set_Gravity (Self : in out Item;   To        : in math.Vector_3)
   is
   begin
      Self.Commands.add ((kind    => set_Gravity,
                          sprite  => null,
                          gravity => To));
   end set_Gravity;





   procedure set_xy_Spin (Self : in out Item;   of_Sprite : in Object.view;
                                                To        : in math.Radians)
   is
   begin
      Self.Commands.add ((kind    => set_xy_Spin,
                          sprite  => of_Sprite,
                          xy_Spin => To));
   end set_xy_Spin;




   procedure update_Bounds (Self : in out Item;   of_Sprite : in Object.view)
   is
   begin
      Self.Commands.add ((kind   => update_Bounds,
                          sprite => of_Sprite));
   end update_Bounds;



   procedure set_local_Anchor (Self : in out Item;   for_Joint : in Joint.view;
                                                     To        : in math.Vector_3;
                                                     is_Anchor_A : in Boolean)
   is
   begin
      Self.Commands.add ((kind         => set_Joint_local_Anchor,
                          sprite       => null,
                          anchor_Joint => for_Joint,
                          local_Anchor => To,
                          is_Anchor_A  => is_Anchor_A));
   end set_local_Anchor;

end physics.Engine;
