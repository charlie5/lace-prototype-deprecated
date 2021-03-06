with ada.unchecked_Deallocation;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Strings.Fixed;



package body Neural.Forge
--
--
--
is

   type Signals_view is access all Signals;
   procedure free is new ada.unchecked_Deallocation (Signals, Signals_view);



   -- Pattern
   --

   procedure Destroy (Self : in out Pattern)
   is
   begin
      Free (Signals_view (Self.Inputs));
      Free (Signals_view (Self.Outputs));
   end Destroy;




   function Creation (From : in Pattern) return Pattern
   is
      Self : Pattern := (Inputs  => new Signals'(From.Inputs.all),
                         Outputs => new Signals'(From.Outputs.all));
   begin
      return Self;
   end Creation;




   -- Patterns
   --

   package Signal_IO is new Ada.Sequential_IO (Signal);

   procedure Free is new Ada.Unchecked_Deallocation (Patterns, neural.Set.Patterns_view);




   function Creation (From_File_Named : in String) return neural.Set.Patterns_view
   is
      use Signal_IO, ada.Text_IO;

      the_File : ada.text_io.File_Type;
   begin
      open (the_File, in_File, From_File_Named & ".bounds");

      declare
         Num_Patterns : Natural               := Integer'Value (get_Line (the_File));
         Num_Inputs   : Positive              := Integer'Value (get_Line (the_File));
         Num_Outputs  : Positive              := Integer'Value (get_Line (the_File));

         Signals_File : signal_io.File_Type;

         The_Creation : neural.Set.Patterns_view := new Patterns (1 .. Num_Patterns);
      begin
         open (Signals_File, In_File, From_File_Named & ".signals");

         for Each_Pattern in The_Creation'Range
         loop
            The_Creation (Each_Pattern).Inputs  := new Signals (1 .. Num_Inputs);
            The_Creation (Each_Pattern).Outputs := new Signals (1 .. Num_Outputs);

            for Each_Input in The_Creation (Each_Pattern).Inputs'Range
            loop
               Read (Signals_File, The_Creation (Each_Pattern).Inputs (Each_Input));
            end loop;

            for Each_Output in The_Creation (Each_Pattern).Outputs'Range
            loop
               Read (Signals_File, The_Creation (Each_Pattern).Outputs (Each_Output));
            end loop;
         end loop;

         Close (Signals_File);
         close (the_File);

         return The_Creation;
      end;

   end Creation;





   procedure Destroy (Self : in out Patterns)
   is
   begin
      for Each in Self'Range loop
         Destroy (Self (Each));
      end loop;
   end Destroy;




   procedure Destroy (Self : in out neural.Set.Patterns_view)
   is
      use type Neural.Set.Patterns_view;
   begin
      if Self /= null then
         Destroy (Self.all);
         Free (Self);
      end if;
   end Destroy;





   procedure Store (Self          : in Patterns;
                    In_File_Named : in String)
   is
      use Signal_IO;
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      use Ada.Strings;

      Bounds_File  : Ada.Text_IO.File_Type;
      Signals_File : Signal_IO.File_Type;

   begin
      create (Bounds_File,   Out_File, In_File_Named & ".bounds");

      put_Line (Bounds_File,   Integer'Image (Self'Length));
      put_Line (Bounds_File,   Integer'Image (Self (1).Inputs'Length));
      put_Line (Bounds_File,   Integer'Image (Self (1).Outputs'Length));

      close (Bounds_File);



      Create (Signals_File, Out_File, In_File_Named & ".signals");

      for Each_Pattern in Self'Range loop

         for Each_Input in Self (Each_Pattern).Inputs'Range loop
            Write (Signals_File, Self (Each_Pattern).Inputs (Each_Input));
         end loop;

         for Each_Output in Self (Each_Pattern).Outputs'Range loop
            Write (Signals_File, Self (Each_Pattern).Outputs (Each_Output));
         end loop;

      end loop;

      Close (Signals_File);
   end Store;




end Neural.Forge;
