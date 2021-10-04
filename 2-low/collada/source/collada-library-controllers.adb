package body collada.Library.controllers
is

   -----------
   --- Utility
   --

   function "+" (From : in ada.Strings.unbounded.unbounded_String) return String
     renames ada.Strings.unbounded.to_String;


   ------------------
   --- vertex weights
   --

   function joint_Offset_of (Self : in vertex_Weights) return math.Index
   is
      the_Input : constant Input_t := find_in (Self.Inputs.all, Joint);
   begin
      return math.Index (the_Input.Offset);
   end joint_Offset_of;



   function weight_Offset_of (Self : in vertex_Weights) return math.Index
   is
      the_Input : constant Input_t := find_in (Self.Inputs.all, Weight);
   begin
      return math.Index (the_Input.Offset);
   end weight_Offset_of;


   --------
   --- Skin
   --

   function Source_of (Self : in Skin;   source_Name : in String) return Source
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



   function Weights_of (Self : in Skin) return access float_Array
   is
      the_Input : constant Input_t := find_in (Self.vertex_weights.Inputs.all, Weight);
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
   end Weights_of;



   function raw_bind_Poses_of (Self : in Skin) return access float_Array
   is
      the_Input : constant Input_t := find_in (Self.joints.Inputs.all, inv_bind_Matrix);
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
   end raw_bind_Poses_of;



   function bind_shape_Matrix_of (Self : in Skin) return Matrix_4x4
   is
      Raw        : constant float_Array := Self.bind_shape_Matrix;
      First      : constant math.Index  := 1;
      the_Matrix :          Matrix_4x4;

   begin
      the_Matrix := (1 => (Raw (First),    Raw (First+1),  Raw (First+2),  Raw (First+3)),   -- These are column vectors.
                     2 => (Raw (First+4),  Raw (First+5),  Raw (First+6),  Raw (First+7)),
                     3 => (Raw (First+8),  Raw (First+9),  Raw (First+10), Raw (First+11)),
                     4 => (Raw (First+12), Raw (First+13), Raw (First+14), Raw (First+15)));

      return the_Matrix;
   end bind_shape_Matrix_of;



   function bind_Poses_of (Self : in Skin) return Matrix_4x4_array
   is
      Raw       : constant access float_Array := raw_bind_Poses_of (Self);
      the_Poses : Matrix_4x4_array (1 .. Raw'Length / 16);
      First     : math.Index := 1;

   begin
      for i in the_Poses'Range
      loop
         the_Poses (i) := (1 => (Raw (First),    Raw (First+1),  Raw (First+2),  Raw (First+3)),   -- These are column vectors.
                           2 => (Raw (First+4),  Raw (First+5),  Raw (First+6),  Raw (First+7)),
                           3 => (Raw (First+8),  Raw (First+9),  Raw (First+10), Raw (First+11)),
                           4 => (Raw (First+12), Raw (First+13), Raw (First+14), Raw (First+15)));
         First := First + 16;
      end loop;

      return the_Poses;
   end bind_Poses_of;



   function joint_Names_of (Self : in Skin) return Text_array
   is
      the_Input : constant Input_t := find_in (Self.Joints.Inputs.all, Joint);
   begin
      declare
         the_Source : constant Source := Source_of (Self, +the_Input.Source);
      begin
         return the_Source.Texts.all;
      end;
   end joint_Names_of;


end collada.Library.controllers;
