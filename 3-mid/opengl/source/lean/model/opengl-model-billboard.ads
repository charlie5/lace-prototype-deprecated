package openGL.Model.billboard
--
--  A rectangle capable of displaying an image.
--
is

   type Item is abstract new openGL.Model.item with private;

   type Plane is (xy, xz, yz);

   --------------
   --- Attributes
   --

   function  Width  (Self : in Item) return Real;
   function  Height (Self : in Item) return Real;

--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds;



private

   type Item is abstract new openGL.Model.item with
      record
         Plane : billboard.Plane := xy;
      end record;


   subtype site_Id is openGL.Index_t range 1 .. 4;
   subtype Sites   is openGL.Vector_3_array (site_Id'Range);

   function vertex_Sites (for_Plane     : in Plane;
                          Width, Height : in Real) return Sites;

   Normal : constant openGL.Vector_3 := (0.0,  0.0,  1.0);

end openGL.Model.billboard;
