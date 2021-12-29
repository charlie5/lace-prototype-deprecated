with
     GL,

     float_Math.Algebra.linear.d2,
     float_Math.Algebra.linear.d3,
     float_Math.Geometry.d2,
     float_Math.Geometry.d3,

     ada.Containers;


package openGL
--
--  Provides a namespace and set of core types.
--
is
   pragma Pure;

   Error : exception;


   ------------
   --  Profiles
   --

   type profile_Kind is (Safe, Lean, Desk);

   function Profile return profile_Kind;


   ----------
   --  Models
   --

   Model_too_complex : exception;
   max_Models        : constant := 2**32 - 1;

   type model_Id is range 0 .. max_Models;

   null_model_Id : constant model_Id;


   -----------
   --  Indices
   --

   type short_Index_t is range 0 .. 2**8  - 1;
   type       Index_t is range 0 .. 2**16 - 1;
   type  long_Index_t is range 0 .. 2**32 - 1;

   type short_Indices is array (long_Index_t range <>) of short_Index_t;
   type       Indices is array (long_Index_t range <>) of       Index_t;
   type  long_Indices is array (long_Index_t range <>) of  long_Index_t;


   --------
   --  Math
   --

   package Math renames float_Math;
   use     Math;

   package linear_Algebra    renames float_Math.Algebra.linear;
   package linear_Algebra_2d renames float_Math.Algebra.linear.d2;
   package linear_Algebra_3d renames float_Math.Algebra.linear.d3;
   package Geometry_2d       renames float_Math.Geometry.d2;
   package Geometry_3d       renames float_Math.Geometry.d3;


   --------
   --  Real
   --
   subtype Real is math.Real;

   package real_Functions renames math.Functions;


   -------------
   --  Safe Real
   --
   protected
   type safe_Real
   is
      procedure Value_is (Now : in Real);
      function  Value       return Real;
   private
      the_Value : Real;
   end safe_Real;


   -----------
   --  Extents
   --
   type Extent_2D is
      record
         Width  : Natural;
         Height : Natural;
      end record;


   -----------
   --  Vectors
   --
   subtype Vector   is math.Vector;

   subtype Vector_2 is math.Vector_2;
   subtype Vector_3 is math.Vector_3;
   subtype Vector_4 is math.Vector_4;

   type    Vector_2_array       is array (Positive     range <>) of         Vector_2;
   type    Vector_3_array       is array (     Index_t range <>) of aliased Vector_3;
   type    Vector_3_large_array is array (long_Index_t range <>) of aliased Vector_3;

   function  Scaled (Self : in Vector_3;         By : in Vector_3) return Vector_3;
   function  Scaled (Self : in Vector_3_array;   By : in Vector_3) return Vector_3_array;

   function  to_Vector_3_array (Self : in Vector_2_array) return Vector_3_array;


   ------------
   --  Matrices
   --
   subtype Matrix     is math.Matrix;

   subtype Matrix_2x2 is math.Matrix_2x2;
   subtype Matrix_3x3 is math.Matrix_3x3;
   subtype Matrix_4x4 is math.Matrix_4x4;


   ---------------
   --  Height Maps
   --
   type height_Map is array (Index_t range <>,
                             Index_t range <>) of aliased Real;

   function  Scaled (Self : in     height_Map;   By : in Real) return height_Map;
   procedure scale  (Self : in out height_Map;   By : in Real);

   function  height_Extent (Self : in height_Map) return Vector_2;
   --
   --  Returns the min and max height.


   type index_Pair is array (1 .. 2) of Index_t;

   function  Region (Self : in height_Map;   Rows, Cols : in index_Pair) return height_Map;
   --
   --  Returns the submatrix indicated via Rows & Cols.


   ------------
   --  Geometry
   --
   subtype      Site    is Vector_3;                   -- A position in 3d space.
   subtype      Sites   is Vector_3_array;
   subtype many_Sites   is Vector_3_large_array;

   subtype      Normal  is Vector_3;                   -- A normal in 3d space.
   subtype      Normals is Vector_3_array;
   subtype many_Normals is Vector_3_large_array;

   type Bounds is
      record
         Ball : Real;                                  -- Sphere radius.
         Box  : Geometry_3d.bounding_Box;
      end record;

   null_Bounds : constant Bounds;

   function  bounding_Box_of   (Self : Sites) return Bounds;
   procedure set_Ball_from_Box (Self : in out Bounds);


   ---------
   --  Color
   --

   -- RGB
   --
   subtype  grey_Value is gl.GLubyte;
   subtype color_Value is gl.GLubyte;

   type rgb_Color is
      record
         Red   : aliased color_Value;
         Green :         color_Value;
         Blue  :         color_Value;
      end record;


   type rgba_Color is
      record
         Primary : rgb_Color;
         Alpha   : color_Value;
      end record;


   -- Primary
   --
   null_Primary : constant := Real'Adjacent (0.0, -1.0);

   type Primary is new Real range null_Primary .. 1.0;

   type Color is
      record
         Red     : Primary;
         Green   : Primary;
         Blue    : Primary;
      end record;

   type Colors is array (Index_t range <>) of Color;


   type Opaqueness is new Real range 0.0 .. 1.0;

   Opaque : constant Opaqueness;
   Lucid  : constant Opaqueness;


   type lucid_Color is
      record
         Primary : Color;
         Opacity : Opaqueness;
      end record;

   type lucid_Colors is array (Index_t range <>) of lucid_Color;


   no_Color       : constant Color;
   no_lucid_Color : constant lucid_Color;


   subtype Shine is Real range 1.0 .. Real'Last;

   default_Shine : constant := 0.05;


   ----------
   --  Images
   --
   type  grey_Image is array (Index_t range <>, Index_t range <>) of aliased grey_Value;
   type       Image is array (Index_t range <>, Index_t range <>) of aliased  rgb_Color;
   type lucid_Image is array (Index_t range <>, Index_t range <>) of aliased rgba_Color;

   function to_Image (From : in lucid_Image) return Image;


   -----------
   --  Texture
   --

   --  Coordinates
   --

   type Coordinate_1D is
      record
         S : aliased Real;
      end record;

   type Coordinate_2D is
      record
         S, T : aliased Real;
      end record;

   type Coordinate_3D is
      record
         S, T, R : aliased Real;
      end record;

   type Coordinate_4D is
      record
         S, T, R, Q : aliased Real;
      end record;

   type Coordinates_1D is array (Index_t range <>) of aliased Coordinate_1D;
   type Coordinates_2D is array (Index_t range <>) of aliased Coordinate_2D;
   type Coordinates_3D is array (Index_t range <>) of aliased Coordinate_3D;
   type Coordinates_4D is array (Index_t range <>) of aliased Coordinate_4D;

   type many_Coordinates_1D is array (long_Index_t range <>) of aliased Coordinate_1D;
   type many_Coordinates_2D is array (long_Index_t range <>) of aliased Coordinate_2D;
   type many_Coordinates_3D is array (long_Index_t range <>) of aliased Coordinate_3D;
   type many_Coordinates_4D is array (long_Index_t range <>) of aliased Coordinate_4D;


   --  Transforms
   --

   type texture_Transform is
     record
         Offset : Real;
         Scale  : Real;
     end record;

   type texture_Transform_1D is
     record
       S : texture_Transform;
     end record;

   type texture_Transform_2D is
     record
       S : texture_Transform;
       T : texture_Transform;
     end record;

   type texture_Transform_3D is
     record
       S : texture_Transform;
       T : texture_Transform;
       R : texture_Transform;
     end record;

   type texture_Transform_4D is
     record
       S : texture_Transform;
       T : texture_Transform;
       R : texture_Transform;
       Q : texture_Transform;
     end record;


   ----------
   --  Assets
   --

   type asset_Name is new String (1 .. 128);
   --
   --  Name of a file containing textures, images, fonts or other resources.

   null_Asset : constant asset_Name;

   function to_Asset  (Self : in String)     return asset_Name;
   function to_String (Self : in asset_Name) return String;
   function Hash      (Self : in asset_Name) return ada.Containers.Hash_type;


   -----------------------------
   --  Shader Program Parameters
   --
   type Parameters is tagged limited private;



   ---------------
   --  Task Safety
   --
   type safe_Boolean is new Boolean;
   pragma Atomic (safe_Boolean);



private

   -- NB: Packing these arrays forces compiler to use the correct size for the element type, rather than the most efficient size.
   --
   pragma Pack (short_Indices);
   pragma Pack (      Indices);
   pragma Pack ( long_Indices);

   pragma Assert (GL.GLfloat'Size = Real'Size);


   null_Asset    : constant asset_Name := (others => ' ');
   null_model_Id : constant model_Id   := 0;
   null_Bounds   : constant Bounds     := (ball => 0.0,
                                           box  => (lower => (Real'Last,  Real'Last,  Real'Last),
                                                    upper => (Real'First, Real'First, Real'First)));
   -----------
   --  Opacity
   --
   Opaque   : constant Opaqueness := 1.0;
   Lucid    : constant Opaqueness := 0.0;

   opaque_Value : constant color_Value := color_Value'Last;
   lucid_Value  : constant color_Value := color_Value'First;


   ---------
   --  Color
   --
   no_Color : constant Color := (Red   => null_Primary,
                                 Green => null_Primary,
                                 Blue  => null_Primary);

   no_lucid_Color : constant lucid_Color := (Primary => (Red   => null_Primary,
                                                         Green => null_Primary,
                                                         Blue  => null_Primary),
                                             Opacity => Opaqueness'First);
   -- RGB
   --
   type  rgb_Colors is array (Index_t range <>) of  rgb_Color;
   type rgba_Colors is array (Index_t range <>) of rgba_Color;


   -- Conversions
   --
   function to_Color (Red, Green, Blue : in Primary) return rgb_Color;

   function to_color_Value (Self : in Primary)     return color_Value;
   function to_Primary     (Self : in color_Value) return Primary;

   function       to_Color (From : in   rgb_Color) return       Color;
   function to_lucid_Color (From : in  rgba_Color) return lucid_Color;
   function  to_rgba_Color (From : in lucid_Color) return  rgba_Color;
   function   to_rgb_Color (From : in       Color) return   rgb_Color;

   function "+"            (From : in   rgb_Color) return       Color renames      to_Color;
   function "+"            (From : in lucid_Color) return  rgba_Color renames to_rgba_Color;
   function "+"            (From : in       Color) return   rgb_Color renames  to_rgb_Color;


   ----------------------------
   -- Shader Program Parameters
   --
   type Parameters is tagged limited null record;

end openGL;
