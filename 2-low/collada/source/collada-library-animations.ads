package collada.Library.animations
--
-- Models a collada 'animations' library, which is a collection of animations.
--
is

   type Inputs_view    is access all Library.Inputs;
   type int_Array_view is access all int_Array;


   -----------
   --- Sampler
   --

   type Sampler is
      record
         Id     : Text;
         Inputs : Inputs_view;
      end record;


   -----------
   --- Channel
   --

   type Channel is
      record
         Source : Text;
         Target : Text;
      end record;


   --------------
   --- Animation
   --

   type Animation is
      record
         Id      :        Text;
         Name    :        Text;

         Sources : access library.Sources;
         Sampler :        animations.Sampler;
         Channel :        animations.Channel;
      end record;

   type Animation_array is array (Positive range <>) of Animation;

   function Inputs_of         (Self : in Animation) return access float_Array;
   function Outputs_of        (Self : in Animation) return access float_Array;
   function Interpolations_of (Self : in Animation) return access float_Array;


   ----------------
   --- Library Item
   --

   type Item is
      record
         Contents : access Animation_array;
      end record;


end collada.Library.animations;
