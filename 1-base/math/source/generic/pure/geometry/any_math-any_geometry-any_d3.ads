generic
package any_Math.any_Geometry.any_d3
--
--  Provides a namespace and core types for 3D geometry.
--
is
   pragma Pure;


   --------------
   --  Core Types
   --

   subtype Site  is Vector_3;
   type    Sites is array (Positive range <>) of Site;


   type a_Model (Site_Count : Positive;
                  Tri_Count : Positive) is
      record
         Sites     : any_d3      .Sites     (1 .. Site_Count);
         Triangles : any_Geometry.Triangles (1 ..  Tri_Count);
      end record;

   function Image (the_Model : in a_Model) return String;



   ---------
   -- Planes
   --

   type Plane is new Vector_4;   -- A general plane equation.

   procedure normalise (the_Plane : in out Plane);



   ----------
   --  Bounds
   --

   type bounding_Box is
      record
         Lower,
         Upper : Site;
      end record;

   null_Bounds : constant bounding_Box;


   function to_bounding_Box (Self : Sites) return bounding_Box;


   function "or"   (Left : in bounding_Box;   Right : in Site)         return bounding_Box;
   --
   --  Returns the bounds expanded to include the vector.

   function "or"   (Left : in bounding_Box;   Right : in bounding_Box) return bounding_Box;
   --
   --  Returns the bounds expanded to include both Left and Right.


   function "+"    (Left : in bounding_Box;   Right : in Vector_3)     return bounding_Box;
   --
   --  Returns the bounds translated by the vector.


   function Extent (Self : in bounding_Box;   Dimension : in Index)    return Real;
   function Image  (Self : in bounding_Box)                            return String;



private

   null_Bounds : constant bounding_Box := (Lower => (Real'Last,  Real'Last,  Real'Last),
                                           Upper => (Real'First, Real'First, Real'First));
end any_Math.any_Geometry.any_d3;
