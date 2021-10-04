package body collada.Library.geometries
is

   -----------
   --- Utility
   --

   function "+" (From : in ada.Strings.unbounded.unbounded_String) return String
     renames ada.Strings.unbounded.to_String;


   -------------
   --- Primitive
   --

   function vertex_Offset_of (Self : in Primitive) return math.Index
   is
      the_Input : constant Input_t := find_in (Self.Inputs.all, Vertex);
   begin
      return math.Index (the_Input.Offset);
   end vertex_Offset_of;



   function normal_Offset_of (Self : in Primitive) return math.Index
   is
      the_Input : constant Input_t := find_in (Self.Inputs.all, Normal);
   begin
      return math.Index (the_Input.Offset);
   end normal_Offset_of;



   function coord_Offset_of (Self : in Primitive) return math.Index
   is
      the_Input : constant Input_t := find_in (Self.Inputs.all, TexCoord);
   begin
      if the_Input = null_Input
      then
         raise no_coord_Offset;
      end if;

      return math.Index (the_Input.Offset);
   end coord_Offset_of;


   --------
   --- Mesh
   --

   function Source_of  (Self        : in Mesh;
                        source_Name : in String) return Source
   is
      use ada.Strings.unbounded;
   begin
      for i in Self.Sources'Range
      loop
         if Self.Sources (i).Id = source_Name (source_Name'First+1 .. source_Name'Last)
         then
            return Self.Sources (i);
         end if;
      end loop;

      declare
         null_Source : Source;
      begin
         return null_Source;
      end;
   end Source_of;



   function Positions_of (Self : in Mesh) return access float_Array
   is
      the_Input : constant Input_t := find_in (Self.Vertices.Inputs.all, Position);
   begin
      if the_Input = null_Input
      then
         return null;
      end if;

      declare
         the_Source : constant Source := Source_of (Self, +the_Input.Source);
      begin
         return the_Source.Floats;
      end;
   end Positions_of;



   function Normals_of (Self          : in Mesh;
                        for_Primitive : in Primitive) return access float_Array
   is
      the_Primitive : Primitive   renames for_Primitive;
      the_Input     : constant Input_t := find_in (the_Primitive.Inputs.all, Normal);

   begin
      if the_Input = null_Input then
         return null;
      end if;

      declare
         the_Source : constant Source := Source_of (Self, +the_Input.Source);
      begin
         return the_Source.Floats;
      end;
   end Normals_of;



   function Coords_of (Self          : in Mesh;
                       for_Primitive : in Primitive) return access float_Array
   is
      the_Primitive : Primitive   renames for_Primitive;
      the_Input     : constant Input_t := find_in (the_Primitive.Inputs.all, TexCoord);

   begin
      if the_Input = null_Input
      then
         return null;
      end if;

      declare
         the_Source : constant Source := Source_of (Self, +the_Input.Source);
      begin
         return the_Source.Floats;
      end;
   end Coords_of;


end collada.Library.geometries;
