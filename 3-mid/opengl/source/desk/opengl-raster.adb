package body openGL.Raster
is

   procedure set_Window_Position (X, Y : in Real;
                                  Z    : in Real := 0.0;
                                  W    : in Real := 1.0)
   is
      pragma Unreferenced (Z, W);
   begin
      raise Program_Error with "TODO: unimplemented";
   end set_Window_Position;

end openGL.Raster;
