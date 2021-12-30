with
     System;


package body lace.Strings.Search
is

   use ada.Strings.Maps,
       System;


   -----------------------
   -- Local Subprograms --
   -----------------------

   function Belongs
     (Element : Character;
      Set     : Maps.Character_Set;
      Test    : Membership) return Boolean;
   pragma Inline (Belongs);
   --  Determines if the given element is in (Test = Inside) or not in
   --  (Test = Outside) the given character set.

   -------------
   -- Belongs --
   -------------

   function Belongs
     (Element : Character;
      Set     : Maps.Character_Set;
      Test    : Membership) return Boolean
   is
   begin
      if Test = Inside then
         return Is_In (Element, Set);
      else
         return not Is_In (Element, Set);
      end if;
   end Belongs;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Num : Natural;
      Ind : Natural;
      Cur : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      Num := 0;
      Ind := Source'First;

      --  Unmapped case

      if Mapping'Address = Maps.Identity'Address then
         while Ind <= Source'Last - PL1 loop
            if Pattern = Source (Ind .. Ind + PL1) then
               Num := Num + 1;
               Ind := Ind + Pattern'Length;
            else
               Ind := Ind + 1;
            end if;
         end loop;

      --  Mapped case

      else
         while Ind <= Source'Last - PL1 loop
            Cur := Ind;
            for K in Pattern'Range loop
               if Pattern (K) /= Value (Mapping, Source (Cur)) then
                  Ind := Ind + 1;
                  goto Cont;
               else
                  Cur := Cur + 1;
               end if;
            end loop;

            Num := Num + 1;
            Ind := Ind + Pattern'Length;

         <<Cont>>
            null;
         end loop;
      end if;

      --  Return result

      return Num;
   end Count;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Num : Natural;
      Ind : Natural;
      Cur : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Check for null pointer in case checks are off

      if Mapping = null then
         raise Constraint_Error;
      end if;

      Num := 0;
      Ind := Source'First;
      while Ind <= Source'Last - PL1 loop
         Cur := Ind;
         for K in Pattern'Range loop
            if Pattern (K) /= Mapping (Source (Cur)) then
               Ind := Ind + 1;
               goto Cont;
            else
               Cur := Cur + 1;
            end if;
         end loop;

         Num := Num + 1;
         Ind := Ind + Pattern'Length;

      <<Cont>>
         null;
      end loop;

      return Num;
   end Count;

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural
   is
      N : Natural := 0;

   begin
      for J in Source'Range loop
         if Is_In (Source (J), Set) then
            N := N + 1;
         end if;
      end loop;

      return N;
   end Count;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      for J in From .. Source'Last loop
         if Belongs (Source (J), Set, Test) then
            First := J;

            for K in J + 1 .. Source'Last loop
               if not Belongs (Source (K), Set, Test) then
                  Last := K - 1;
                  return;
               end if;
            end loop;

            --  Here if J indexes first char of token, and all chars after J
            --  are in the token.

            Last := Source'Last;
            return;
         end if;
      end loop;

      --  Here if no token found

      First := From;
      Last  := 0;
   end Find_Token;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      for J in Source'Range loop
         if Belongs (Source (J), Set, Test) then
            First := J;

            for K in J + 1 .. Source'Last loop
               if not Belongs (Source (K), Set, Test) then
                  Last := K - 1;
                  return;
               end if;
            end loop;

            --  Here if J indexes first char of token, and all chars after J
            --  are in the token.

            Last := Source'Last;
            return;
         end if;
      end loop;

      --  Here if no token found

      First := Source'First;
      Last  := 0;
   end Find_Token;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Cur : Natural;

      Ind : Integer;
      --  Index for start of match check. This can be negative if the pattern
      --  length is greater than the string length, which is why this variable
      --  is Integer instead of Natural. In this case, the search loops do not
      --  execute at all, so this Ind value is never used.

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Forwards case

      if Going = Forward then
         Ind := Source'First;

         --  Unmapped forward case

         if Mapping'Address = Maps.Identity'Address then
            for J in 1 .. Source'Length - PL1 loop
               if Pattern = Source (Ind .. Ind + PL1) then
                  return Ind;
               else
                  Ind := Ind + 1;
               end if;
            end loop;

         --  Mapped forward case

         else
            for J in 1 .. Source'Length - PL1 loop
               Cur := Ind;

               for K in Pattern'Range loop
                  if Pattern (K) /= Value (Mapping, Source (Cur)) then
                     goto Cont1;
                  else
                     Cur := Cur + 1;
                  end if;
               end loop;

               return Ind;

            <<Cont1>>
               Ind := Ind + 1;
            end loop;
         end if;

      --  Backwards case

      else
         --  Unmapped backward case

         Ind := Source'Last - PL1;

         if Mapping'Address = Maps.Identity'Address then
            for J in reverse 1 .. Source'Length - PL1 loop
               if Pattern = Source (Ind .. Ind + PL1) then
                  return Ind;
               else
                  Ind := Ind - 1;
               end if;
            end loop;

         --  Mapped backward case

         else
            for J in reverse 1 .. Source'Length - PL1 loop
               Cur := Ind;

               for K in Pattern'Range loop
                  if Pattern (K) /= Value (Mapping, Source (Cur)) then
                     goto Cont2;
                  else
                     Cur := Cur + 1;
                  end if;
               end loop;

               return Ind;

            <<Cont2>>
               Ind := Ind - 1;
            end loop;
         end if;
      end if;

      --  Fall through if no match found. Note that the loops are skipped
      --  completely in the case of the pattern being longer than the source.

      return 0;
   end Index;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
      PL1 : constant Integer := Pattern'Length - 1;
      Ind : Natural;
      Cur : Natural;

   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Check for null pointer in case checks are off

      if Mapping = null then
         raise Constraint_Error;
      end if;

      --  If Pattern longer than Source it can't be found

      if Pattern'Length > Source'Length then
         return 0;
      end if;

      --  Forwards case

      if Going = Forward then
         Ind := Source'First;
         for J in 1 .. Source'Length - PL1 loop
            Cur := Ind;

            for K in Pattern'Range loop
               if Pattern (K) /= Mapping.all (Source (Cur)) then
                  goto Cont1;
               else
                  Cur := Cur + 1;
               end if;
            end loop;

            return Ind;

         <<Cont1>>
            Ind := Ind + 1;
         end loop;

      --  Backwards case

      else
         Ind := Source'Last - PL1;
         for J in reverse 1 .. Source'Length - PL1 loop
            Cur := Ind;

            for K in Pattern'Range loop
               if Pattern (K) /= Mapping.all (Source (Cur)) then
                  goto Cont2;
               else
                  Cur := Cur + 1;
               end if;
            end loop;

            return Ind;

         <<Cont2>>
            Ind := Ind - 1;
         end loop;
      end if;

      --  Fall through if no match found. Note that the loops are skipped
      --  completely in the case of the pattern being longer than the source.

      return 0;
   end Index;

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   is
   begin
      --  Forwards case

      if Going = Forward then
         for J in Source'Range loop
            if Belongs (Source (J), Set, Test) then
               return J;
            end if;
         end loop;

      --  Backwards case

      else
         for J in reverse Source'Range loop
            if Belongs (Source (J), Set, Test) then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
   begin
      if Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return
           Index (Source (From .. Source'Last), Pattern, Forward, Mapping);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return
           Index (Source (Source'First .. From), Pattern, Backward, Mapping);
      end if;
   end Index;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      if Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return Index
           (Source (From .. Source'Last), Pattern, Forward, Mapping);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return Index
           (Source (Source'First .. From), Pattern, Backward, Mapping);
      end if;
   end Index;

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return
           Index (Source (From .. Source'Last), Set, Test, Forward);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return
           Index (Source (Source'First .. From), Set, Test, Backward);
      end if;
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if Source (J) /= ' ' then
               return J;
            end if;
         end loop;

      else -- Going = Backward
         for J in reverse Source'Range loop
            if Source (J) /= ' ' then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index_Non_Blank;

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return
           Index_Non_Blank (Source (From .. Source'Last), Forward);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return
           Index_Non_Blank (Source (Source'First .. From), Backward);
      end if;
   end Index_Non_Blank;

end lace.Strings.Search;
