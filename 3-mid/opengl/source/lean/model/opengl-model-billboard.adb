package body openGL.Model.billboard
is

   --------------
   --- Attributes
   --

--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds
--     is
--        the_Sites  : constant Sites         := vertex_Sites (Self.Plane,  Self.Width, Self.Height);
--        the_Bounds : constant openGL.Bounds := bounding_Box_of (the_Sites);
--     begin
--        return the_Bounds;
--     end Bounds;



   function Width  (Self : in Item) return math.Real
   is
   begin
      case Self.Plane
      is
         when xy =>
            return Self.Scale (1);

         when xz =>
            return Self.Scale (1);

         when yz =>
            return Self.Scale (3);
      end case;
   end Width;



   function Height  (Self : in Item) return math.Real
   is
   begin
      case Self.Plane
      is
         when xy =>
            return Self.Scale (2);

         when xz =>
            return Self.Scale (3);

         when yz =>
            return Self.Scale (2);
      end case;
   end height;



   function vertex_Sites (for_Plane     : in Plane;
                          Width, Height : in math.Real) return Sites
   is
      use type openGL.Real;

      half_Width  : constant openGL.Real            := openGL.Real (Width  / 2.0);
      half_Height : constant openGL.Real            := openGL.Real (Height / 2.0);

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
