package openGL.Raster
--
-- Provides raster operations.
--
is

   procedure set_window_Pos (x, y : in Real;
                             z    : in Real := 0.0;    -- Default of '0.0' places text in front of all other renders.
                             w    : in Real := 1.0);

end openGL.Raster;
