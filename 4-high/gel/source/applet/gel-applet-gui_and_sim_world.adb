with
     gel.World.simple,
     gel.Camera.forge,
     gel.Events,

     lace.Event.utility;


package body gel.Applet.gui_and_sim_world
is

   procedure define (Self : access Item;   Name       : in String;
                                           use_Window : in gel.Window.view)
   is
      pragma Unreferenced (use_Window);
      use lace.Event.utility;
   begin
      declare
         the_world_Info : constant world_Info_view  := new world_Info;
         the_Camera     : constant gel.Camera.view  := gel.Camera.forge.new_Camera;
      begin
         the_world_Info.World := gel.World.simple.forge.new_World (Name,
                                                                   gui_world_Id,
                                                                   space_Kind => physics.Bullet,
                                                                   Renderer   => Self.Renderer).all'Access;

         the_world_Info.World.register (Self.all'unchecked_Access,
                                        to_Kind (gel.events.new_sprite_added_to_world_Event'Tag));

         the_Camera.Viewport_is (Self.Window.Width, Self.Window.Height);
         the_Camera.Renderer_is (Self.Renderer);
         the_Camera.Site_is     ((0.0, 5.0, 5.0));

         the_world_Info.Cameras.append (the_Camera);
         Self.Worlds           .append (the_world_Info);

         Self.local_Subject_and_Observer.add (the_add_new_sprite_Response'Access,
                                              to_Kind (gel.events.new_sprite_added_to_world_Event'Tag),
                                              the_world_Info.World.Name);
         the_world_Info.World.start;
      end;

      declare
         the_world_Info : constant world_Info_view  := new world_Info;
         the_Camera     : constant gel.Camera.View  := gel.Camera.forge.new_Camera;
      begin
         the_world_Info.World := gel.World.simple.forge.new_World (Name       => Name,
                                                                   Id         => sim_world_Id,
                                                                   space_Kind => physics.Bullet,
                                                                   Renderer   => Self.Renderer).all'Access;

         the_world_Info.World.register (the_Observer => Self.all'unchecked_Access,
                                        of_Kind      => to_Kind (gel.events.new_sprite_added_to_world_Event'Tag));

         the_Camera.Viewport_is (Self.Window.Width, Self.Window.Height);
         the_Camera.Renderer_is (Self.Renderer);
         the_Camera.Site_is     ((0.0, 5.0, 5.0));

         the_world_Info.Cameras.append (the_Camera);
         Self.Worlds           .append (the_world_Info);

         Self.local_Subject_and_Observer.add (the_add_new_sprite_Response'Access,
                                              to_Kind (gel.events.new_sprite_added_to_world_Event'Tag),
                                              the_world_Info.World.Name);
         the_world_Info.World.start;
      end;
   end define;



   package body Forge
   is
      function to_Applet (Name       : in String;
                          use_Window : in gel.Window.view) return Item
      is
      begin
         return Self : Item := (gel.Applet.Forge.to_Applet (Name, use_Window)
                                with null record)
         do
            define (Self'unchecked_Access, Name, use_Window);
         end return;
      end to_Applet;



      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view) return View
      is
         Self : constant View := new Item' (to_Applet (Name, use_Window));
      begin
         return Self;
      end new_Applet;

   end Forge;



   function sim_World (Self : in Item) return gel.World.view
   is
   begin
      return Self.World (sim_world_Id);
   end sim_World;



   function sim_Camera (Self : in Item) return gel.Camera.view
   is
   begin
      return Self.Camera (sim_world_Id,
                          sim_camera_Id);
   end sim_Camera;



   function gui_World (Self : in Item) return gel.World.view
   is
   begin
      return Self.World (gui_world_Id);
   end gui_World;



   function gui_Camera (Self : in Item) return gel.Camera.view
   is
   begin
      return Self.Camera (gui_world_Id,
                          gui_camera_Id);
   end gui_Camera;


end gel.Applet.gui_and_sim_world;
