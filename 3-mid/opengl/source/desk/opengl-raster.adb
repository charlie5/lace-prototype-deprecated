package body openGL.Raster
is

   procedure set_window_Pos (x, y : in Real;
                             z    : in Real := 0.0;
                             w    : in Real := 1.0)
   is
      pragma Unreferenced (z, w);
   begin
      raise Program_Error with "unimplemented";
   end set_window_Pos;

end openGL.Raster;
