package openGL.Model.billboard
--
--  Models a rectangle capable of displaying an image.
--
is
   type Item  is abstract new Model.item with private;
   type Plane is (xy, xz, yz);

   --------------
   --- Attributes
   --

   function Width  (Self : in Item) return Real;
   function Height (Self : in Item) return Real;



private

   type Item is abstract new Model.item with
      record
         Plane  : billboard.Plane := xy;
         Width  : Real;
         Height : Real;
      end record;


   subtype site_Id is Index_t range 1 .. 4;
   subtype Sites   is Vector_3_array (site_Id'Range);

   function vertex_Sites (for_Plane     : in Plane;
                          Width, Height : in Real) return Sites;

   Normal : constant Vector_3 := (0.0,  0.0,  1.0);

end openGL.Model.billboard;
