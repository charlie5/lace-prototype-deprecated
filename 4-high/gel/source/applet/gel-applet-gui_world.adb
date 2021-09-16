with
     mmi.Events,
     mmi.Camera.forge,

     lace.Event.utility,

     ada.Unchecked_Deallocation;


package body mmi.Applet.gui_world
is

   procedure define (Self : in mmi.Applet.gui_world.view;   Name       : in String;
                                                            space_Kind : in physics.space_Kind)
   is

   begin
      declare
         use lace.event.Utility;

         the_world_Info : constant world_Info_view  := new world_Info;
         the_Camera     : constant mmi.Camera.View  := mmi.Camera.forge.new_Camera;
      begin
         the_world_Info.World := mmi.World.forge.new_World (Name,
                                                            gui_world_Id,
                                                            space_Kind,
                                                            Self.Renderer);

         the_Camera.set_viewport_Size (Self.Window.Width,  Self.Window.Height);
         the_Camera.Renderer_is       (Self.Renderer);
         the_Camera.Site_is           ((0.0, 5.0, 50.0));

         the_world_Info.Cameras.append (the_Camera);

         Self.Worlds.append (the_world_Info);

         Self.local_Subject_and_Observer.add (the_add_new_sprite_Response'Access,
                                              to_Kind (mmi.events.new_sprite_added_to_world_Event'Tag),
                                              the_world_Info.World.Name);
         the_world_Info.World.start;
      end;

   end define;



   package body Forge
   is

      function new_Applet (Name       : in String;
                           use_Window : in mmi.Window.view;
                           space_Kind : in physics.space_Kind) return mmi.Applet.gui_world.view
      is
         Self : constant View := new Item' (mmi.Applet.Forge.to_Applet (Name, use_Window)
                                            with others => <>);
      begin
         define (Self, Name, space_Kind);
         return Self;
      end new_Applet;

   end Forge;



   procedure free    (Self : in out View)
   is
      procedure deallocate is new ada.Unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



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


end mmi.Applet.gui_world;
