package body collada.Library.animations
is
   -----------
   --- Utility
   --

   function "+" (From : in ada.Strings.unbounded.unbounded_String) return String
     renames ada.Strings.unbounded.to_String;


   -------------
   --- Animation
   --

   function Source_of  (Self        : in Animation;
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



   function find_Inputs_of (Self : in Animation;   for_Semantic : in Semantic) return access float_Array
   is
      the_Input : constant Input_t := find_in (Self.Sampler.Inputs.all, for_Semantic);
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
   end find_Inputs_of;



   function Inputs_of (Self : in Animation) return access float_Array
   is
   begin
      return find_Inputs_of (Self, for_Semantic => Input);
   end Inputs_of;



   function Outputs_of (Self : in Animation) return access float_Array
   is
   begin
      return find_Inputs_of (Self, for_Semantic => Output);
   end Outputs_of;



   function Interpolations_of (Self : in Animation) return access float_Array
   is
   begin
      return find_Inputs_of (Self, for_Semantic => Interpolation);
   end Interpolations_of;


end collada.Library.animations;
