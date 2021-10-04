package collada.Library
--
-- Provides a namespace and core types for the specific collada library child packages.
--
is
   ----------
   -- Sources
   --

   type Source is
      record
         Id       : Text;
         array_Id : Text;

         Floats   : access float_Array;
         Texts    : access Text_array;
      end record;

   type Sources is array (Positive range <>) of Source;


   ----------
   --- Inputs
   --

   type Semantic is (Unknown,
                     BINORMAL,        -- Geometric binormal (bitangent) vector.
                     COLOR,           -- Color coordinate vector. Color inputs are RGB (float3_type).
                     CONTINUITY,      -- Continuity constraint at the control vertex (CV).
                     IMAGE,           -- Raster or MIP-level input.
                     INPUT,           -- Sampler input.
                     IN_TANGENT,      -- Tangent vector for preceding control point.
                     INTERPOLATION,   -- Sampler interpolation type.
                     INV_BIND_MATRIX, -- Inverse of local-to-world matrix.
                     JOINT,           -- Skin influence identifier.
                     LINEAR_STEPS,    -- Number of piece-wise linear approximation steps to use for the spline segment that follows this CV.
                     MORPH_TARGET,    -- Morph targets for mesh morphing.
                     MORPH_WEIGHT,    -- Weights for mesh morphing.
                     NORMAL,          -- Normal vector.
                     OUTPUT,          -- Sampler output.
                     OUT_TANGENT,     -- Tangent vector for succeeding control point.
                     POSITION,        -- Geometric coordinate vector.
                     TANGENT,         -- Geometric tangent vector.
                     TEXBINORMAL,     -- Texture binormal (bitangent) vector.
                     TEXCOORD,        -- Texture coordinate vector.
                     TEXTANGENT,      -- Texture tangent vector.
                     UV,              -- Generic parameter vector.
                     VERTEX,          -- Mesh vertex.
                     WEIGHT);         -- Skin influence weighting value.

   type Input_t is
      record
         Semantic : library.Semantic := Unknown;
         Source   : Text;
         Offset   : Natural          := 0;
      end record;

   type Inputs is array (Positive range <>) of Input_t;

   null_Input : constant Input_t;


   function find_in (Self : Inputs;   the_Semantic : in library.Semantic) return Input_t;



private

   null_Input : constant Input_t := (others => <>);

end collada.Library;
