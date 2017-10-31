with
     openGL.Camera,
     openGL.Impostor.simple,
     openGL.Impostor.terrain,
     openGL.Texture,

     ada.Containers.Generic_Array_Sort,
     ada.Unchecked_Deallocation;


package body openGL.Impostorer
is

   ---------
   --- Forge
   --

   procedure define (Self : in out Item)
   is
   begin
      Self.impostor_size_Min.Value_is (0.0625);
   end define;



   procedure destruct (Self : in out Item)
   is
      procedure free is new ada.Unchecked_Deallocation (impostor_load_Balancer.Slots,
                                                        impostor_load_Balancer.Slots_view);
   begin
      free (Self.impostor_load_Slots);

      declare
         use Impostor;

         the_Impostor : Impostor.view;
         Cursor       : visual_Maps_of_impostor.Cursor := Self.visual_Map_of_imposter.First;
      begin
         while has_Element (Cursor)
         loop
            the_Impostor := Element (Cursor);
            Self.Renderer.free (the_Impostor);

            next (Cursor);
         end loop;
      end;
   end destruct;



   --------------
   --- Attributes
   --

   function  impostor_Count (Self : in     Item) return Natural
   is
   begin
      return Natural (Self.visual_Map_of_imposter.Length);
   end impostor_Count;


   function impostor_size_Min (Self : in     Item'Class) return Real
   is
   begin
      return Self.impostor_size_Min.Value;
   end impostor_size_Min;


   procedure impostor_size_Min_is (Self : in out Item'Class;   Now : in Real)
   is
   begin
      Self.impostor_size_Min.Value_is (Now);
   end impostor_size_Min_is;



   function  Camera (Self : in     Item'Class)     return access openGL.Camera.item'Class
   is
   begin
      return Self.Camera;
   end Camera;


   procedure Camera_is (Self : in out Item'Class;   Now : access openGL.Camera.item'Class)
   is
   begin
      Self.Camera := Now;
   end Camera_is;



   function  Renderer (Self : in     Item'Class)     return openGL.Renderer.lean.view
   is
   begin
      return Self.Renderer;
   end Renderer;


   procedure Renderer_is (Self : in out Item'Class;   Now : in openGL.Renderer.lean.view)
   is
   begin
      Self.Renderer := Now;
   end Renderer_is;




   --------------
   --  Operations
   --

   procedure substitute (Self : in out Item;   the_Visuals : in out openGL.Visual.views;
                                               Camera      : access openGL.Camera.item'Class)
   is
   begin
      -- Find whether visual or imposter is used, for each object.
      --
      declare
         transposed_camera_Attitude : constant Matrix_3x3 := Transpose (Camera.Spin);

         Impostor_updates           :          openGL.renderer.lean.impostor_Updates (1 .. 20_000);
         impostor_updates_Last      :          Natural    := 0;

         procedure add (the_Impostor : in Impostor.view)
         is
            use openGL.Texture;
         begin
            impostor_updates_Last                    := impostor_updates_Last + 1;
            Impostor_updates (impostor_updates_Last) := (impostor              => the_Impostor,
--                                                           Width_size            => to_Size (Natural (the_Impostor.current_Width_pixels)),
--                                                           Height_size           => to_Size (Natural (the_Impostor.current_Height_pixels)),
--                                                           Width_size            => Natural (the_Impostor.current_Width_pixels),
--                                                           Height_size           => to_Size (Natural (the_Impostor.current_Height_pixels)),
                                                         current_Width_pixels  => the_Impostor.current_Width_pixels,
                                                         current_Height_pixels => the_Impostor.current_Height_pixels,

                                                         current_copy_x_Offset => the_Impostor.current_copy_X_Offset,
                                                         current_copy_y_Offset => the_Impostor.current_copy_Y_Offset,
                                                         current_copy_X        => the_Impostor.current_copy_X,
                                                         current_copy_Y        => the_Impostor.current_copy_Y,
                                                         current_copy_Width    => the_Impostor.current_copy_Width,
                                                         current_copy_Height   => the_Impostor.current_copy_Height,

                                                         current_Camera_look_at_Rotation => the_Impostor.current_Camera_look_at_Rotation);
            the_Impostor.freshen_Count := 0;
            the_Impostor.never_Updated := False;
         end add;

         the_impostor_size_Min : constant Real := Self.impostor_size_Min.Value;

      begin
         for Each in Self.impostor_load_Slots'Range
         loop
            Self.impostor_load_Slots (Each).impostors_Count := 0;        -- Empty each slot's contents.
         end loop;

         for Each in the_Visuals'Range
         loop
            declare
               the_Visual   : openGL.Visual.view renames the_Visuals (Each);
               the_Impostor : Impostor.view;
            begin
               -- Replace the visual with the impostors visual, if the visuals apparent size is small enough.
               --
               if the_Visual.apparent_Size < the_impostor_size_Min
               then   -- Use impostor.
                  -- Find or create the impostor for the visual.
                  --
                  begin
                     the_Impostor := Element (Self.visual_Map_of_imposter, the_Visual);
                  exception
                     when Constraint_Error =>   -- No impostor exists for this visual yet, so create one.
                        if the_Visual.is_Terrain
                        then
                           the_Impostor := new Impostor.terrain.item;
                        else
                           the_Impostor := new Impostor.simple.item;

                           the_Impostor.set_size_update_trigger_Delta        (to =>  10);
                           the_Impostor.set_freshen_count_update_trigger_Mod (to => 250);
                        end if;

                        the_Impostor.set_Target (the_Visual);
                        Self.visual_Map_of_imposter.insert (the_Visual, the_Impostor);
                  end;

                  declare
                     use linear_Algebra_3d,
                         openGL.Renderer.lean, openGL.Visual, openGL.Texture;

                     impostor_Target          : Visual.view renames the_Visual;

                     Impostor_update_required : constant Boolean := the_Impostor.update_Required (Camera);
                     Impostor_is_valid        : constant Boolean := the_Impostor.is_Valid;
                     Impostor_never_updated   : constant Boolean := the_Impostor.never_Updated;

                  begin
                     if Impostor_is_valid
                     then
                        if Impostor_update_required
                        then
                           the_Impostor.target_camera_Distance_less_frame_Count :=         the_Impostor.target_camera_Distance
                                                                                   - Real (the_Impostor.frame_Count_since_last_update);
                           if Impostor_never_updated
                           then
                              add (the_Impostor);
                           else
                              declare  -- Add impostor to appropriate load balancing slot.
                                 target_face_Count : constant Positive := impostor_Target.face_Count;


                                 function Slot_Id return Positive
                                 is
                                 begin
                                    for Each in Self.impostor_load_Slots.all'Range
                                    loop
                                       if target_face_Count <= Self.impostor_load_Slots (Each).max_Faces
                                       then
                                          return Each;
                                       end if;
                                    end loop;

                                    raise Program_Error;
                                 end Slot_Id;


                                 the_Slot : impostor_load_Balancer.Slot renames Self.impostor_load_Slots (Slot_Id);
                              begin
                                 the_Slot.impostors_Count                      := the_Slot.impostors_Count + 1;
                                 the_Slot.Impostors (the_Slot.impostors_Count) := the_Impostor;
                              end;
                           end if;
                        end if;

                        the_Impostor.Visual.Site_is (Site_of (the_Visual.all));
                        the_Impostor.Visual.Spin_is (transposed_camera_Attitude);

                        the_Visuals (Each) := the_Impostor.Visual;   -- Replace the visual with the impostor.

                     end if;
                  end;

               else   -- Don't use impostor.
                  null;
               end if;
            end;
         end loop;


         -- Add the load balanced impostor updates.
         --
         for Each in Self.impostor_load_Slots'Range
         loop
            declare
               the_Slot    :          impostor_load_Balancer.Slot renames Self.impostor_load_Slots (Each);
               num_Updates : constant Natural                     :=      Natural'Min (the_Slot.max_Updates,
                                                                                       the_Slot.impostors_Count);

               function "<" (L, R : in Impostor.view) return Boolean
               is
               begin
                  return   L.target_camera_Distance_less_frame_Count  -- Subtracting 'frame count' allows distant
                         < R.target_camera_Distance_less_frame_Count; -- targets a chance of update (tbd: need some sort of user-settable scale param to allow for very large scales (space/etc)).
               end "<";


               procedure sort is new Ada.Containers.Generic_Array_Sort (Positive,
                                                                        Impostor.view,
                                                                        Impostor.views);
            begin
               sort (the_Slot.Impostors (1 .. the_Slot.impostors_Count));

               for Each in 1 .. num_Updates
               loop
                  add (the_Slot.Impostors (Each));
               end loop;
            end;
         end loop;


         Self.Renderer.queue_Impostor_updates (Impostor_updates (1 .. impostor_updates_Last),
                                               Camera);
      end;

   end substitute;


end openGL.Impostorer;
