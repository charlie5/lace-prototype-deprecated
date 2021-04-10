with
     lace.Text.utility,
     ada.Text_IO;

procedure test_Text_replace
is
   use lace.Text,
       lace.Text.utility,
       ada.Text_IO;

   test_Error : exception;
begin
   put_Line ("Begin Test");
   new_Line;

   -- Test 'replace' function.
   --

   declare
      Initial : aliased constant lace.Text.item :=  to_Text ("<TOKEN>");
      Final   :         constant String         := +replace (Initial, "<TOKEN>", "");
   begin
      if Final /= ""
      then
         raise test_Error with "replace fails:   Initial => '" & (+Initial) & "' "
                                              & "Final => '"   &   Final   & "'";
      end if;
   end;

   declare
      Initial : aliased constant lace.Text.item :=  to_Text ("<TOKEN>");
      Final   :         constant String         := +replace (Initial, "<TOKEN>", "Linux");
   begin
      if Final /= "Linux"
      then
         raise test_Error with "replace fails:   Initial => '" & (+Initial) & "' "
                                              & "Final => '"   &   Final   & "'";
      end if;
   end;

   declare
      Initial : aliased constant lace.Text.item :=  to_Text ("123<TOKEN>456");
      Final   :         constant String         := +replace (Initial, "<TOKEN>", "Linux");
   begin
      if Final /= "123Linux456"
      then
         raise test_Error with "replace fails:   Initial => '" & (+Initial) & "' "
                                              & "Final => '"   &   Final   & "'";
      end if;
   end;

   declare
      Initial : aliased constant lace.Text.item :=  to_Text ("123<TOKEN>");
      Final   :         constant String         := +replace (Initial, "<TOKEN>", "Linux");
   begin
      if Final /= "123Linux"
      then
         raise test_Error with "replace fails:   Initial => '" & (+Initial) & "' "
                                              & "Final => '"   &   Final   & "'";
      end if;
   end;

   declare
      Initial : aliased constant lace.Text.item :=  to_Text ("<TOKEN>456");
      Final   :         constant String         := +replace (Initial, "<TOKEN>", "Linux");
   begin
      if Final /= "Linux456"
      then
         raise test_Error with "replace fails:   Initial => '" & (+Initial) & "' "
                                              & "Final => '"   &   Final   & "'";
      end if;
   end;

   declare
      Initial : aliased constant lace.Text.item :=  to_Text ("<TOKEN>123<TOKEN>");
      Final   :         constant String         := +replace (Initial, "<TOKEN>", "Linux");
   begin
      if Final /= "Linux123Linux"
      then
         raise test_Error with "replace fails:   Initial => '" & (+Initial) & "' "
                                              & "Final => '"   &   Final   & "'";
      end if;
   end;

   declare
      Initial : aliased constant lace.Text.item :=  to_Text ("<TOKEN><TOKEN>");
      Final   :         constant String         := +replace (Initial, "<TOKEN>", "Linux");
   begin
      if Final /= "LinuxLinux"
      then
         raise test_Error with "replace fails:   Initial => '" & (+Initial) & "' "
                                              & "Final => '"   &   Final   & "'";
      end if;
   end;

   declare
      Initial : aliased constant lace.Text.item :=  to_Text ("<TOKEN>", capacity => 64);
      Final   :         constant String         := +replace (Initial, "<TOKEN>", "Longish String") with Unreferenced;
   begin
      put_Line ("No capacity error raised, as expected.");
   end;


   -- Test 'replace' procedure.
   --

   declare
      Initial : constant String := "<TOKEN>";
      Text    : lace.Text.item  := to_Text (Initial);
   begin
      replace (Text, "<TOKEN>", "");

      if +Text /= ""
      then
         raise test_Error with "replace fails:   Initial => '" & Initial & "' "
                                                & "Final => '" & (+Text) & "'";
      end if;
   end;

   declare
      Initial : constant String := "<TOKEN>";
      Text    : lace.Text.item  := to_Text (Initial);
   begin
      replace (Text, "<TOKEN>", "Linux");

      if +Text /= "Linux"
      then
         raise test_Error with "replace fails:   Initial => '" & Initial & "' "
                                                & "Final => '" & (+Text) & "'";
      end if;
   end;

   declare
      Initial : constant String := "123<TOKEN>456";
      Text    : lace.Text.item  := to_Text (Initial);
   begin
      replace (Text, "<TOKEN>", "Linux");

      if +Text /= "123Linux456"
      then
         raise test_Error with "replace fails:   Initial => '" & Initial & "' "
                                                & "Final => '" & (+Text) & "'";
      end if;
   end;

   declare
      Initial : constant String := "123<TOKEN>";
      Text    : lace.Text.item  := to_Text (Initial);
   begin
      replace (Text, "<TOKEN>", "Linux");

      if +Text /= "123Linux"
      then
         raise test_Error with "replace fails:   Initial => '" & Initial & "' "
                                                & "Final => '" & (+Text) & "'";
      end if;
   end;

   declare
      Initial : constant String := "<TOKEN>456";
      Text    : lace.Text.item  := to_Text (Initial);
   begin
      replace (Text, "<TOKEN>", "Linux");

      if +Text /= "Linux456"
      then
         raise test_Error with "replace fails:   Initial => '" & Initial & "' "
                                                & "Final => '" & (+Text) & "'";
      end if;
   end;

   declare
      Initial : constant String := "<TOKEN>123<TOKEN>";
      Text    : lace.Text.item  := to_Text (Initial);
   begin
      replace (Text, "<TOKEN>", "Linux");

      if +Text /= "Linux123Linux"
      then
         raise test_Error with "replace fails:   Initial => '" & Initial & "' "
                                                & "Final => '" & (+Text) & "'";
      end if;
   end;

   declare
      Initial : constant String := "<TOKEN><TOKEN>";
      Text    : lace.Text.item  := to_Text (Initial);
   begin
      replace (Text, "<TOKEN>", "Linux");

      if +Text /= "LinuxLinux"
      then
         raise test_Error with "replace fails:   Initial => '" & Initial & "' "
                                                & "Final => '"   & (+Text) & "'";
      end if;
   end;

   declare
      Initial : constant String := "<TOKEN>";
      Text    : lace.Text.item  := to_Text (Initial);
   begin
      replace (Text, "<TOKEN>", "Longish String");

   exception
      when lace.Text.Error =>
         put_Line ("Capacity error raised, as expected.");
   end;

   declare
      Initial : constant String := "<TOKEN>";
      Text    : lace.Text.item  := to_Text (Initial, capacity => 64);
   begin
      replace (Text, "<TOKEN>", "Longish String");
      put_Line ("No capacity error raised, as expected.");
   end;


   new_Line;
   put_Line ("Success");
   put_Line ("End Test");
end test_Text_replace;
