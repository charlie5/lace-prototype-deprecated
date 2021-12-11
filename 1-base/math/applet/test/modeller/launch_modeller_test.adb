with
     ada.Text_IO,
     float_Math.Geometry.d3.Modeller.Forge;


procedure launch_modeller_Test
is
   use ada.Text_IO,
       float_Math.Geometry.d3.Modeller.Forge;

   the_Modeller : polar_Model := polar_Model_from ("gaspra.tab");

begin
   put_Line ("Done.");
end launch_modeller_Test;
