package collada.Library.geometries
--
-- Models a collada 'geometries' library, which is a collection of geometries.
--
is

   ------------
   --- Vertices
   --

   type Vertices is
      record
         Id     :        Text;
         Inputs : access library.Inputs;
      end record;


   --------------
   --- Primitives
   --

   type int_array_List is array (Positive range <>) of access int_array;

   type primitive_Kind is (Unknown,
                           Lines,     line_Strips,
                           Polygons,  polyList,
                           Triangles, triFans,     triStrips);

   type Primitive (Kind : primitive_Kind := Unknown) is
      record
         Count    : Natural;
         Material : Text;

         Inputs   : access library.Inputs;
         P_List   : access int_array_List;

         case Kind is
            when polyList =>
               vCount : access int_Array;

            when others =>
               null;
         end case;
      end record;

   type Primitives is array (Positive range <>) of Primitive;

   function vertex_Offset_of (Self : in Primitive) return math.Index;
   function normal_Offset_of (Self : in Primitive) return math.Index;
   function  coord_Offset_of (Self : in Primitive) return math.Index;

   no_coord_Offset : exception;


   --------
   --- Mesh
   --

   type Mesh is
      record
         Sources    : access library.Sources;
         Vertices   :        geometries.Vertices;
         Primitives : access geometries.Primitives;
      end record;

   function Source_of    (Self          : in Mesh;
                          source_Name   : in String)    return Source;

   function Positions_of (Self          : in Mesh)      return access float_Array;

   function Normals_of   (Self          : in Mesh;
                          for_Primitive : in Primitive) return access float_Array;

   function Coords_of    (Self          : in Mesh;
                          for_Primitive : in Primitive) return access float_Array;

   ------------
   --- Geometry
   --

   type Geometry is
      record
         Name : Text;
         Id   : Text;
         Mesh : geometries.Mesh;
      end record;

   type Geometry_array is array (Positive range <>) of Geometry;


   ----------------
   --- Library Item
   --

   type Item is
      record
         Contents : access Geometry_array;
      end record;


end collada.Library.geometries;
