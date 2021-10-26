with
     launch_simple_chat_Client,
     launch_simple_chat_Registrar,
     launch_simple_instant_events_demo,
     launch_simple_deferred_events_demo,
     lace_demo_Events,
     lace_demo_Keyboard,

     lace.Observer.instant,
     lace.Subject .local,

     lace.Response,
     lace.Event.utility,

     launch_strings_Demo,

     launch_basic_math_Demo,
     launch_basic_geometry_Demo,
     launch_math_Testsuite,

     launch_Outline,
     launch_Tree,
     launch_Write,

     launch_parse_Box,
     launch_learn_Linear,

     launch_impact_hello_2d_Demo,
     launch_orbs_hello_Demo,
     launch_impact_hello_3d_Demo,
     launch_box_box_collision_Test,
     launch_rigid_body_spin_Test,
     launch_sphere_sphere_collision_Test,

     launch_camera_Demo,
     launch_core_Test,
     launch_large_terrain_Demo,
     launch_many_boxes_Demo,
     launch_render_Lighting,
     launch_Model_scaling,
     launch_render_Arrows,
     launch_render_billboards,
     launch_render_Boxes,
     launch_render_Capsules,
     launch_render_Models,
     launch_render_Screenshot,
     launch_render_Text,
     launch_two_cameras_Demo,
     launch_freetype_linkage_Test,
     --  launch_egl_linkage_Test,
     --  launch_gl_linkage_Test,

     launch_Test_2d,

     --  launch_hello_physics_interface_Demo,
     launch_test_Engine,

     launch_Client,
     launch_Server,
     launch_Pong,
     launch_Hello_gel,
     launch_opengl_Model,
     launch_Mouse_motion,
     launch_Mouse_selection,
     launch_Rig_Demo,
     launch_Chains_2d,
     launch_drop_Ball_on_Box,
     launch_drop_Box_on_Box,
     launch_hinged_Box,
     launch_mixed_Joints,
     launch_mixed_Joints_2d,
     launch_mixed_Shapes,
     launch_text_Sprite_Demo,
     launch_add_rid_Sprite_Test,
     launch_pong_Tute,

     ada.Text_IO;

procedure build_all_Lace
--
-- Pulls in all applets and libraries for global checking, reference finding and refactoring.
--
is
begin
   launch_simple_chat_Client;
   launch_simple_chat_Registrar;
   launch_simple_deferred_events_Demo;
   launch_simple_instant_events_Demo;

   launch_strings_Demo;

   launch_basic_math_Demo;
   launch_basic_geometry_Demo;
   launch_math_Testsuite;

   launch_Outline;
   launch_Tree;
   launch_Write;

   launch_parse_Box;
   launch_learn_Linear;

   launch_impact_hello_2d_Demo;
   launch_orbs_hello_Demo;
   launch_impact_hello_3d_Demo;
   launch_box_box_collision_Test;
   launch_rigid_body_spin_Test;
   launch_sphere_sphere_collision_Test;

   launch_camera_Demo;
   launch_core_Test;
   launch_large_terrain_Demo;
   launch_many_boxes_Demo;
   launch_render_Lighting;
   launch_Model_scaling;
   launch_render_Arrows;
   launch_render_billboards;
   launch_render_Boxes;
   launch_render_Capsules;
   launch_render_Models;
   launch_render_Screenshot;
   launch_render_Text;
   launch_two_cameras_Demo;
   --  launch_egl_linkage_Test;
   launch_freetype_linkage_Test;
   --  launch_gl_linkage_Test;

   launch_Test_2D;

   --  launch_hello_physics_interface_Demo;
   launch_Test_Engine;

   launch_Client;
   launch_Server;
   launch_Pong;
   launch_Hello_gel;
   launch_opengl_Model;
   launch_Mouse_motion;
   launch_Mouse_selection;
   launch_Rig_Demo;
   launch_Chains_2d;
   launch_drop_Ball_on_Box;
   launch_drop_Box_on_Box;
   launch_hinged_Box;
   launch_mixed_Joints;
   launch_mixed_Joints_2d;
   launch_mixed_Shapes;
   launch_text_Sprite_Demo;
   launch_add_rid_Sprite_Test;
   launch_pong_Tute;

end build_all_Lace;
