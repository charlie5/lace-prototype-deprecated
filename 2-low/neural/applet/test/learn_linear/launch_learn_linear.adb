with linear_Net;
with Neural.Net;

with lace.Environ.Paths;

with ada.Calendar;
with ada.Text_IO;             use ada.Text_IO;
with ada.Exceptions;          use ada.Exceptions;


procedure launch_learn_Linear
--
--
--
is
   package Math renames Neural.Math;
begin

   declare
      use Math,
          lace.Environ.Paths;

      the_Net             : aliased  linear_Net.item;

      pattern_Count       : constant := 10;
      the_Patterns        :          neural.Patterns (1 .. pattern_Count);

--        the_Environ         :          lace.Environ.item;

      Training            : constant Boolean          := True;
      --        Training            :          Boolean          := False;
      Signal              : math.Real := 0.0;
   begin
      --- Setup.
      --
      if Training
      then
         lace.Environ.Paths.rid_Folder (+"./velocity.net");
      end if;

      the_Net.define ("velocity");

      if Training
      then
         --- Collect net trainng data.
         --
         for each_Pattern in 1 .. pattern_Count
         loop
            Signal := Signal + 1.0;
            the_Patterns (Each_Pattern) := the_Net.Pattern_For (Signal,  False,  -Signal);
         end loop;


         --- Train the net.
         --
         the_Net.Patterns_are (the_Patterns);
         the_Net.train  (Max_Epochs => 150_000,
                         Client_Actions        => (1 => (Act  => Neural.Net.report_training_Status'Access,
                                                         Rate => 1000),
                                                   2 => (Act  => Neural.Net.store_best_Epoch'Access,
                                                         Rate => 1000)),
--                           Client_Continue       => Net_is_training'Access,
                         acceptable_error      => 0.000_01);


         the_Net.report_training_Status;


         --              if   the_Net.test_RMS_Error_For
         --                <= the_Net.min_test_RMS_Error_for
         --              then
         --                 the_Net.Store;
         --              end if;

      end if;

      --- Test the net.
      --
      put_Line ("Net's output estimate: " & Real'Image (the_Net.Output_for (5.55)));

   exception
      when E : others =>
         put_Line ("Launch *unhandled* exception ...");
         put_Line (Exception_Information (E));
         put_Line ("Launch has terminated !");
   end;


end launch_learn_Linear;
