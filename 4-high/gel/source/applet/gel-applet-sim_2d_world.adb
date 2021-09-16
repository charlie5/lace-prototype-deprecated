with
     mmi.Camera.forge,
     physics.Forge;


package body mmi.Applet.sim_2d_world
is
   use      Math;
   use type math.Real,
            math.Index;


   sim_world_Id  : constant mmi.World_Id  := 1;
   sim_camera_Id : constant mmi.camera_Id := 1;



   procedure define (Self : in View;   Name : in String)
   is
   begin
      declare
         the_world_Info : constant world_Info_view  := new world_Info;
         the_Camera     : constant mmi.Camera.View  := mmi.Camera.forge.new_Camera;
      begin
         the_world_Info.World := mmi.World.forge.new_World (Name,
                                                            sim_world_Id,
                                                            physics.Box2d,
                                                            Self.Renderer);

         the_Camera.set_viewport_Size (Self.Window.Width,  Self.Window.Height);
         the_Camera.Renderer_is       (Self.Renderer);
         the_Camera.Site_is           ((0.0, 5.0, 50.0));

         the_world_Info.Cameras.append (the_Camera);

         Self.Worlds.append (the_world_Info);
      end;

   end define;



   package body Forge
   is

      function new_Applet (Name       : in String;
                           use_Window : in mmi.Window.view) return View
      is
         Self : constant View := new Item' (mmi.Applet.Forge.to_Applet (Name, use_Window)
                                            with others => <>);
      begin
         define (Self, Name);
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


end mmi.Applet.sim_2d_world;
