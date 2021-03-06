with
     float_Math.Geometry.d3.Modeller.Forge,
     ada.text_IO;

procedure launch_basic_geometry_Demo
--
-- A simple demonstration of the geometry packages.
--
is
   package Math renames float_Math;

   use Math,
       math.Geometry,
       math.Geometry.d3.Modeller;

   procedure log (Message : in String)
                  renames ada.text_IO.put_Line;

   the_Modeller : d3.Modeller.item;

begin
   declare
      use float_math.Geometry.d3,
          float_math.Geometry.d3.Modeller.Forge;

      the_Model : float_math.Geometry.d3.a_Model := to_box_Model;
   begin
      log ("Box Model:     " & Image (the_Model));
   end;

   declare
      use float_math.Geometry.d3,
          float_math.Geometry.d3.Modeller.Forge;

      the_Model : float_math.Geometry.d3.a_Model := to_capsule_Model;
   begin
      log ("Capsule Model: " & Image (the_Model));
   end;
end launch_basic_geometry_Demo;
