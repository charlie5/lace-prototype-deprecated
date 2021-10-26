package body openGL.Model.billboard
is

   ---------
   --- Forge
   --

   procedure define (Self : out Item;   Size : Size_t := default_Size)
   is
   begin
      Self.Size := Size;
   end define;



   --------------
   --- Attributes
   --

   function Size (Self : in Item) return Size_t
   is
   begin
      return Self.Size;
   end Size;



   function Width (Self : in Item) return Real
   is
   begin
      return Self.Size.Width;
   end Width;



   function Height (Self : in Item) return Real
   is
   begin
      return Self.Size.Height;
   end Height;



   function vertex_Sites (for_Plane     : in Plane;
                          Width, Height : in Real) return Sites
   is
      half_Width  : constant Real := Width  / 2.0;
      half_Height : constant Real := Height / 2.0;

      the_Sites   : constant array (Plane) of Sites := (xy => ((-half_Width, -half_Height,         0.0),
                                                               ( half_Width, -half_Height,         0.0),
                                                               ( half_Width,  half_Height,         0.0),
                                                               (-half_Width,  half_Height,         0.0)),
                                                        xz => ((-half_Width,          0.0,         1.0),
                                                               ( half_Width,          0.0,         1.0),
                                                               ( half_Width,          0.0,        -1.0),
                                                               (-half_Width,          0.0,        -1.0)),
                                                        yz => ((        0.0, -half_Height,  half_Width),
                                                               (        0.0, -half_Height, -half_Width),
                                                               (        0.0,  half_Height, -half_Width),
                                                               (        0.0,  half_Height,  half_Width)));
   begin
      return the_Sites (for_Plane);
   end vertex_Sites;


end openGL.Model.billboard;
