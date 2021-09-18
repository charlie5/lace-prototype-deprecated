with
     gel.Events,
     gel.Camera.forge,
     lace.Event.utility,
     ada.unchecked_Deallocation;

package body gel.Applet.gui_world
is

   procedure define (Self : in gel.Applet.gui_world.view;   Name       : in String;
                                                            space_Kind : in physics.space_Kind)
   is
      use lace.Event.utility;

      the_world_Info : constant world_Info_view  := new world_Info;
      the_Camera     : constant gel.Camera.View  := gel.Camera.forge.new_Camera;
   begin
      the_world_Info.World := gel.World.forge.new_World (Name,
                                                         gui_world_Id,
                                                         space_Kind,
                                                         Self.Renderer);

      the_Camera.set_viewport_Size (Self.Window.Width, Self.Window.Height);
      the_Camera.Renderer_is       (Self.Renderer);
      the_Camera.Site_is           ((0.0, 5.0, 50.0));

      the_world_Info.Cameras.append (the_Camera);

      Self.Worlds.append (the_world_Info);

      Self.local_Subject_and_Observer.add (the_add_new_sprite_Response'Access,
                                           to_Kind (gel.events.new_sprite_added_to_world_Event'Tag),
                                           the_world_Info.World.Name);
      the_world_Info.World.start;
   end define;



   package body Forge
   is

      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view;
                           space_Kind : in physics.space_Kind) return gel.Applet.gui_world.view
      is
         Self : constant View := new Item' (gel.Applet.Forge.to_Applet (Name, use_Window)
                                            with null record);
      begin
         define (Self, Name, space_Kind);
         return Self;
      end new_Applet;

   end Forge;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



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


end gel.Applet.gui_world;
