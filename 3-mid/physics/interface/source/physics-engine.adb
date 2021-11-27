with
     ada.unchecked_Deallocation,
     ada.Containers,
     ada.Calendar,
     ada.Text_IO,
     ada.Exceptions;

package body physics.Engine
is
   use ada.Text_IO;


   protected body safe_command_Set
   is
      function is_Empty return Boolean
      is
      begin
         return the_Count = 0;
      end is_Empty;


      procedure add (the_Command : in Command)
      is
      begin
         the_Count       := the_Count + 1;
         Set (the_Count) := the_Command;
      end add;


      procedure Fetch (To    : out Commands;
                       Count : out Natural)
      is
      begin
         To (1 .. the_Count) := Set (1 .. the_Count);
         Count               := the_Count;
         the_Count           := 0;
      end Fetch;
   end safe_command_Set;



   task body Evolver
   is
      use type physics.Joint.view,
               ada.Containers.Count_type;

      Stopped          : Boolean                   := True;
      Cycle            : ada.Containers.Count_type := 0;
      next_render_Time : ada.Calendar.Time;

      max_joint_Force,
      max_joint_Torque : Real := 0.0;


      procedure free_Objects
      is
--           the_free_Objects : gel.Object.views := the_World.free_Object_Set;
      begin
--           for Each in the_free_Objects'Range
--           loop
--              log ("Engine is Freeing Object id: " & Object_Id'Image (the_free_Objects (Each).Id));
--
--              if the_free_Objects (Each).owns_Graphics
--              then
--                 the_World.Renderer.free (the_free_Objects (Each).Visual.Model);
--              end if;
--
--              gel.Object.free (the_free_Objects (Each));
--           end loop;
         null;

      end free_Objects;



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
                        the_Command.Object.activate;
                        the_Command.Object.Shape.Scale_is (the_Command.Scale);
                        the_Command.Object      .Scale_is (the_Command.Scale);

                        Self.Space.update_Bounds (the_Command.Object);


                     when update_Bounds =>
                        Self.Space.update_Bounds (the_Command.Object);


                     when update_Site =>
                        the_Command.Object.Site_is (the_Command.Site);


                     when set_Speed =>
                        the_Command.Object.Speed_is (the_Command.Speed);


                     when set_xy_Spin =>
                        the_Command.Object.xy_Spin_is (the_Command.xy_Spin);


                     when add_Object =>
                        declare
--                             procedure rebuild_Shape (the_Object : in Object.view)
--                             is
--                                use type physics.Model.shape_Kind,
--                                         physics.Model.View;
--
--                                the_Scale : aliased Vector_3;
--
--                             begin
--                                if the_Object.physics_Model = null then
--                                   return;
--                                end if;
--
--                                the_Scale := Self.physics_Model.Scale;
--
--                                case Self.physics_Model.shape_Info.Kind
--                                is
--                                when physics.Model.Cube =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.        new_box_Shape (Self.physics_Model.shape_Info.half_Extents));
--
--                                when physics.Model.a_Sphere =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.     new_sphere_Shape (Self.physics_Model.shape_Info.sphere_Radius));
--
--                                when physics.Model.multi_Sphere =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.new_multisphere_Shape (Self.physics_Model.shape_Info.Sites.all,
--                                                                     Self.physics_Model.shape_Info.Radii.all));
--                                when physics.Model.Cone =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.       new_cone_Shape (radius => Real (Self.physics_Model.Scale (1) / 2.0),
--                                                                                                               height => Real (Self.physics_Model.Scale (2))));
--                                when physics.Model.a_Capsule =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.    new_capsule_Shape (Self.physics_Model.shape_Info.lower_Radius,
--                                                                     Self.physics_Model.shape_Info.Height));
--                                when physics.Model.Cylinder =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.   new_cylinder_Shape (Self.physics_Model.shape_Info.half_Extents));
--
--                                when physics.Model.Hull =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.new_convex_hull_Shape (Self.physics_Model.shape_Info.Points.all));
--
--                                when physics.Model.Mesh =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics       .new_mesh_Shape (Self.physics_Model.shape_Info.Model));
--
--                                when physics.Model.Plane =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.      new_plane_Shape (Self.physics_Model.Shape_Info.plane_Normal,
--                                                                     Self.physics_Model.Shape_Info.plane_Offset));
--                                when physics.Model.Heightfield =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.new_heightfield_Shape (Self.physics_Model.shape_Info.Heights.all,
--                                                                     Self.physics_Model.Scale));
--                                when physics.Model.Circle =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.     new_circle_Shape (Self.physics_Model.shape_Info.circle_Radius));
--
--                                when physics.Model.Polygon =>
--                                   Self.Shape := physics_Shape_view (Self.World.Physics.    new_polygon_Shape (physics.space.polygon_Vertices (Self.physics_Model.shape_Info.Vertices (1 .. Self.physics_Model.shape_Info.vertex_Count))));
--                                end case;
--
--                             end rebuild_Shape;


                           procedure add (the_Object : in Object.view)
                           is
                           begin
--                                the_World.add (the_Object. physics_Model.all'Access);

--                                if the_Object.physics_Model.is_Tangible
--                                then

                              --                                rebuild_Shape  (the_Object);
                              the_Object.Shape.define;
                              --  the_Object.define (Shape       => the_Object.Shape,
                              --                     Mass        => the_Object.Model.Mass,
                              --                     Friction    => the_Object.Model.Friction,
                              --                     Restitution => the_Object.Model.Restitution,
                              --                     at_Site     => the_Object.Model.Site);

                              Self.Space.add (the_Object);
--                                end if;

--                                begin
--                                   the_Object_Transforms.insert  (the_Object, Identity_4x4);
--                                   the_Object.Solid.user_Data_is (the_Object);
--                                end;

--                                the_World.Object_Count                     := the_World.Object_Count + 1;
--                                the_World.Objects (the_World.Object_Count) := the_Object;
                           end add;

                        begin
                           add (the_Command.Object);
                        end;


                     when rid_Object =>
                        declare
                           function find (the_Object : in Object.view) return Index
                           is
                           begin
--                                for Each in 1 .. the_World.Object_Count
--                                loop
--                                   if the_World.Objects (Each) = the_Object
--                                   then
--                                      return Each;
--                                   end if;
--                                end loop;

                              raise constraint_Error with "no such Object in world";
                              return 0;
                           end find;


                           procedure rid (the_Object : in Object.view)
                           is
                              use type Object.view;
                           begin
                              if the_Object /= null
                              then
--                                   if the_Object.physics_Model.is_Tangible
--                                   then
                                    Self.Space.rid (the_Object);
--                                   end if;

--                                   if the_Object_Transforms.contains (the_Object) then
--                                      the_Object_Transforms.delete   (the_Object);
--                                   end if;

                              else
                                 raise program_Error;
                              end if;


                              declare
                                 Id : Index;
                                 pragma Unreferenced (Id);
                              begin
                                 Id := find (the_Object);

--                                   if Id <= the_World.Object_Count
--                                   then
--                                      the_World.Objects (1 .. the_World.Object_Count - 1)
--                                        :=   the_World.Objects (     1 .. Id - 1)
--                                           & the_World.Objects (Id + 1 .. the_World.Object_Count);
--                                   end if;

--                                   the_World.Object_Count := the_World.Object_Count - 1;
                              end;
                           end rid;

                        begin
                           rid (the_Command.Object);
                        end;


                     when apply_Force =>
                        the_Command.Object.apply_Force (the_Command.Force);


                     when destroy_Object =>
                        declare
--                             the_free_Set : free_Set renames the_World.free_Sets (the_World.current_free_Set);
                        begin
                           raise Program_Error with "destroy_Object ~ TODO";
--                             the_free_Set.Count                        := the_free_Set.Count + 1;
--                             the_free_Set.Objects (the_free_Set.Count) := the_Command.Object;
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
--                                   return ray_Collision' (near_Object => null,
--                                                          others      => <>);
--                                else
--                                   return ray_Collision' (to_GEL (physics_Collision.near_Object),
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
--                             if        the_Collision.near_Object = null
--                               or else the_Collision.near_Object.is_Destroyed
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
--                                   the_Event.near_Object := the_Collision.near_Object;
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
--           free_Objects;

      end evolve;

      use ada.Calendar;

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


               -- Add 'destroy' commands for all Objects.
               --
--                 declare
--                    the_Objects : Object.views renames the_World.Objects;
--                 begin
--                    for i in 1 .. the_World.Object_Count
--                    loop
--                       the_Objects (i).destroy (and_Children => False);
--                    end loop;
--                 end;

               -- Evolve the world til there are no commands left.
               --
               while not Self.Commands.is_Empty
               loop
                  evolve;
               end loop;

               -- Free both sets of freeable Objects.
               --
               free_Objects;
               free_Objects;
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


--           the_World.new_Object_transforms_Available.signal;
--           the_World.evolver_Done                   .signal;


         -- Check for joint breakage.
         --
--           if the_World.broken_joints_Allowed
--           then
--              declare
--                 use gel.Joint,
--                     standard.physics.Space;
--
--                 the_Joint       : gel.Joint.view;
--                 reaction_Force,
--                 reaction_Torque : math.Real;
--
--                 Cursor          : standard.physics.Space.joint_Cursor'Class := the_World.Physics.first_Joint;
--              begin
--                 while has_Element (Cursor)
--                 loop
--                    the_Joint := to_GEL (Element (Cursor));
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
      procedure free is new ada.unchecked_Deallocation (safe_command_Set, safe_command_Set_view);
   begin
      Self.Evolver.stop;
      free (Self.Commands);
   end stop;



   procedure add (Self : access Item;   the_Object : in Object.view)
   is
   begin
      put_Line ("physics engine: add Object");
      Self.Commands.add ((Kind         => add_Object,
                          Object       => the_Object,
                          add_Children => False));
   end add;


   procedure rid (Self : in out Item;   the_Object : in Object.view)
   is
   begin
      Self.Commands.add ((Kind         => rid_Object,
                          Object       => the_Object,
                          rid_Children => False));
   end rid;



   procedure add (Self : in out Item;   the_Joint : in Joint.view)
   is
   begin
      Self.Commands.add ((Kind   => add_Joint,
                          Object => null,
                          Joint  => the_Joint));
   end add;


   procedure rid (Self : in out Item;   the_Joint : in Joint.view)
   is
   begin
      Self.Commands.add ((Kind   => rid_Joint,
                          Object => null,
                          Joint  => the_Joint));
   end rid;



   procedure update_Scale (Self : in out Item;   of_Object : in Object.view;
                                                 To        : in math.Vector_3)
   is
   begin
      Self.Commands.add ((Kind   => scale_Object,
                          Object => of_Object,
                          Scale  => To));
   end update_Scale;



   procedure apply_Force (Self : in out Item;   to_Object : in Object.view;
                                                Force     : in math.Vector_3)
   is
   begin
      Self.Commands.add ((Kind   => apply_Force,
                          Object => to_Object,
                          Force  => Force));
   end apply_Force;



   procedure update_Site (Self : in out Item;   of_Object : in Object.view;
                                                To        : in math.Vector_3)
   is
   begin
      put_Line ("physics engine: update_Site");
      Self.Commands.add ((Kind   => update_Site,
                          Object => of_Object,
                          Site   => To));
   end update_Site;



   procedure set_Speed (Self : in out Item;   of_Object : in Object.view;
                                              To        : in math.Vector_3)
   is
   begin
      Self.Commands.add ((Kind   => set_Speed,
                          Object => of_Object,
                          Speed  => To));
   end set_Speed;



   procedure set_Gravity (Self : in out Item;   To : in math.Vector_3)
   is
   begin
      Self.Commands.add ((Kind    => set_Gravity,
                          Object  => null,
                          Gravity => To));
   end set_Gravity;



   procedure set_xy_Spin (Self : in out Item;   of_Object : in Object.view;
                                                To        : in math.Radians)
   is
   begin
      Self.Commands.add ((Kind    => set_xy_Spin,
                          Object  => of_Object,
                          xy_Spin => To));
   end set_xy_Spin;



   procedure update_Bounds (Self : in out Item;   of_Object : in Object.view)
   is
   begin
      Self.Commands.add ((Kind   => update_Bounds,
                          Object => of_Object));
   end update_Bounds;



   procedure set_local_Anchor (Self : in out Item;   for_Joint : in Joint.view;
                                                     To        : in math.Vector_3;
                                                     is_Anchor_A : in Boolean)
   is
   begin
      Self.Commands.add ((Kind         => set_Joint_local_Anchor,
                          Object       => null,
                          anchor_Joint => for_Joint,
                          local_Anchor => To,
                          is_Anchor_A  => is_Anchor_A));
   end set_local_Anchor;

end physics.Engine;
