with
     mmi.Camera.forge,
     mmi.Events,

     physics.Forge,

     lace.event.Utility;


package body mmi.Applet.gui_and_sim_world
is

   use      Math;
   use type math.Real, math.Index;



   procedure define (Self : access Item;   Name       : in String;
                                           use_Window : in mmi.Window.view)
   is
      use lace.Event.utility;
   begin
      declare
         the_world_Info : constant world_Info_view  := new world_Info;
         the_Camera     : constant mmi.Camera.View  := mmi.Camera.forge.new_Camera;
      begin
         the_world_Info.World := mmi.World.forge.new_World (Name,
                                                            gui_world_Id,
                                                            space_kind => physics.Forge.Bullet,
                                                            Renderer   => Self.Renderer);

         the_world_Info.World.register (Self.all'Unchecked_Access,
                                        to_Kind (mmi.events.new_sprite_added_to_world_Event'Tag));

         the_Camera.set_viewport_Size (Self.Window.Width,  Self.Window.Height);
         the_Camera.Renderer_is       (Self.Renderer);
         the_Camera.Site_is           ((0.0, 5.0, 5.0));

         the_world_Info.Cameras.append (the_Camera);
         Self.Worlds           .append (the_world_Info);

         Self.local_Subject_and_Observer.add (the_add_new_sprite_Response'Access,
                                              to_Kind (mmi.events.new_sprite_added_to_world_Event'Tag),
                                              the_world_Info.World.Name);
         the_world_Info.World.start;
      end;


      declare
         the_world_Info : constant world_Info_view  := new world_Info;
         the_Camera     : constant mmi.Camera.View  := mmi.Camera.forge.new_Camera;
      begin
         the_world_Info.World := mmi.World.forge.new_World (name       => Name,
                                                            id         => sim_world_Id,
                                                            space_kind => physics.Forge.Bullet,
                                                            Renderer   => Self.Renderer);

         the_world_Info.World.register (the_observer => Self.all'Unchecked_Access,
                                        of_kind      => to_Kind (mmi.events.new_sprite_added_to_world_Event'Tag));

         the_Camera.set_viewport_Size (Self.Window.Width,  Self.Window.Height);
         the_Camera.Renderer_is       (Self.Renderer);
         the_Camera.Site_is           ((0.0, 5.0, 5.0));

         the_world_Info.Cameras.append (the_Camera);
         Self.Worlds           .append (the_world_Info);

         Self.local_Subject_and_Observer.add (the_add_new_sprite_Response'Access,
                                              to_Kind (mmi.events.new_sprite_added_to_world_Event'Tag),
                                              the_world_Info.World.Name);
         the_world_Info.World.start;
      end;
   end define;




   package body Forge
   is
      function  to_Applet (Name       : in String;
                           use_Window : in mmi.Window.view) return Item
      is
      begin
         return Self : Item := (mmi.Applet.Forge.to_Applet (Name, use_Window)
                                with others => <>)
         do
            define (Self'Unchecked_Access, Name, use_Window);
         end return;
      end to_Applet;



      function new_Applet (Name       : in String;
                           use_Window : in mmi.Window.view) return View
      is
         Self : constant View := new Item' (to_Applet (Name, use_Window));
      begin
         return Self;
      end new_Applet;

   end Forge;





   function sim_World  (Self : in Item) return mmi.World.view
   is
   begin
      return Self.World (sim_world_Id);
   end sim_World;


   function sim_Camera (Self : in Item) return mmi.Camera.view
   is
   begin
      return Self.Camera (sim_world_Id, sim_camera_Id);
   end sim_Camera;



   function gui_World  (Self : in Item) return mmi.World.view
   is
   begin
      return Self.World (gui_world_Id);
   end gui_World;


   function gui_Camera (Self : in Item) return mmi.Camera.view
   is
   begin
      return Self.Camera (gui_world_Id, gui_camera_Id);
   end gui_Camera;


end mmi.Applet.gui_and_sim_world;
