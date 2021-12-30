with
     ada.Containers;


package lace.Event
--
--  The base class for all derived event types.
--
is
   pragma Pure;

   type Item is tagged null record;

   null_Event : constant Event.item;


   subtype  subject_Name is String;
   subtype observer_Name is String;


   procedure destruct (Self : in out Item) is null;


   type Kind is new String;
   --
   --  Uniquely identifies each derived event class.
   --
   --  Each derived event class will have its own Kind.
   --
   --  Maps to the extended name of 'ada.Tags.Tag_type' value of each derived
   --  event class (see 'Conversions' section in 'lace.Event.utility').

   function Hash (the_Kind : in Kind) return ada.Containers.Hash_type;



private
   null_Event : constant Event.item := (others => <>);
end lace.Event;
