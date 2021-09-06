package openGL.Raster
--
-- Provides raster operations.
--
is

   procedure set_Window_Position (X, Y : in Real;
                                  Z    : in Real := 0.0;    -- Default of '0.0' places text in front of all other renders.
                                  W    : in Real := 1.0);

end openGL.Raster;
