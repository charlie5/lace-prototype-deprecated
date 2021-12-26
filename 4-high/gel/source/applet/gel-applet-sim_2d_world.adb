with
     gel.Camera.forge,
     gel.World.simple;


package body gel.Applet.sim_2D_world
is

   sim_world_Id  : constant gel.world_Id  := 1;
   sim_camera_Id : constant gel.camera_Id := 1;



   procedure define (Self : in View;   Name : in String)
   is
      the_world_Info : constant world_Info_view  := new world_Info;
      the_Camera     : constant gel.Camera.View  := gel.Camera.forge.new_Camera;
   begin
      the_world_Info.World := gel.World.simple.forge.new_World (Name,
                                                                sim_world_Id,
                                                                physics.Box2d,
                                                                Self.Renderer).all'Access;

      the_Camera.Viewport_is (Self.Window.Width, Self.Window.Height);
      the_Camera.Renderer_is (Self.Renderer);
      the_Camera.Site_is     ((0.0, 5.0, 50.0));

      the_world_Info.Cameras.append (the_Camera);

      Self.Worlds.append (the_world_Info);
   end define;



   package body Forge
   is

      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view) return View
      is
         Self : constant View := new Item' (gel.Applet.Forge.to_Applet (Name, use_Window)
                                            with null record);
      begin
         define (Self, Name);
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


end gel.Applet.sim_2D_world;
