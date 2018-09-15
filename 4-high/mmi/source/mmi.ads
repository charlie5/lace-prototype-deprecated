with
     openGL,
     Physics,
     float_Math.Geometry.d2,
     float_Math.Geometry.d3,
     float_Math.Algebra.linear.d3;


package MMI
--
--  Provides a set of 'Man/Machine Interface' classes.
--
is
   pragma Pure;

   Error : exception;


   package Math           renames float_Math;
   package Geometry       renames math.Geometry;
   package Geometry_2d    renames math.Geometry.d2;
   package Geometry_3d    renames math.Geometry.d3;
   package Algebra        renames math.Algebra;
   package linear_Algebra renames math.Algebra.linear;
   package Algebra_3d     renames math.Algebra.linear.d3;


   --  Constraints
   --
   max_Worlds          : constant := 1000;
   max_Cameras         : constant := 1000;
   max_graphics_Models : constant := 2**32 - 1;
--     max_physics_Models  : constant := 2**32 - 1;
   max_Sprites         : constant := 2**32 - 1;


   --  Ids
   --

   type          world_Id is range 0 .. max_Worlds;
   type         camera_Id is range 0 .. max_Cameras;

   subtype graphics_model_Id is openGL.model_Id;
--     type  physics_model_Id is range 0 .. max_physics_Models;
   type         sprite_Id is range 0 .. max_Sprites;

   null_graphics_model_Id : constant graphics_model_Id;
--     null_physics_model_Id  : constant physics.model_Id;
   null_sprite_Id         : constant sprite_Id;

   type graphics_model_Ids is array (Positive range <>) of graphics_model_Id;
   type  physics_model_Ids is array (Positive range <>) of  physics.model_Id;
   type         sprite_Ids is array (Positive range <>) of         sprite_Id;


   --  Assets
   --

   type asset_Name is new String (1 .. 128);
   --
   --  Name of a file containing textures, images, fonts, sounds, media or other resources.

   null_Asset : constant asset_Name;

   function to_Asset  (Self : in String)     return asset_Name;
   function to_String (Self : in asset_Name) return String;



private

   null_graphics_model_Id : constant graphics_model_Id := 0;
--     null_physics_model_Id  : constant physics.model_Id  := 0;
   null_sprite_Id         : constant sprite_Id         := 0;

   null_Asset             : constant asset_Name        := (others => ' ');
end MMI;
