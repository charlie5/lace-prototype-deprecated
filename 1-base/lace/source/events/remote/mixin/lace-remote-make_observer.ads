with
     lace.Event,
     lace.remote.Response,
     lace.remote.Observer;

private
with
     ada.containers.indefinite_hashed_Maps,
     ada.strings.Hash;

generic
   type T is abstract tagged limited private;

package lace.remote.make_Observer
--
--  Makes a user class T into an event Observer.
--
is
   pragma remote_Types;

   type Item is abstract limited new T
                                 and Observer.item with private;

   type View is access all Item'Class;
   --   pragma Asynchronous (View);        -- tbd: Needed for lossy events.

   procedure destroy (Self : in out Item);


   --- Responses
   --

   overriding
   procedure add (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in event.Kind;
                                        from_Subject : in String);
   overriding
   procedure rid (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in event.Kind;
                                        from_Subject : in String);
   overriding
   procedure relay_responseless_Events (Self : in out Item;   To : in Observer.view);


   --- Operations
   --

   overriding
   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := event.null_Event;
                                            from_Subject : in String);

   overriding
   procedure respond (Self : access Item);



private

   --- event response Maps
   --

   use type event.Kind;
   use type Response.view;
   function to_Hash (Self : in event.Kind) return ada.containers.Hash_type;

   package event_response_Maps     is new ada.containers.indefinite_hashed_Maps (key_type        => event.Kind,
                                                                      element_type    => Response.view,
                                                                      hash            => to_Hash,
                                                                      equivalent_keys => "=");
   subtype event_response_Map      is event_response_maps.Map;
   type    event_response_Map_view is access all event_response_Map;


   --- subject Maps of event responses
   --

   package subject_Maps_of_event_responses
   is new ada.containers.indefinite_hashed_Maps (key_type        => String,
                                                 element_type    => event_response_Map_view,
                                                 hash            => ada.strings.Hash,
                                                 equivalent_keys => "=");
   subtype subject_Map_of_event_responses is subject_Maps_of_event_responses.Map;


   --- Observer Item
   --

   type Item is abstract limited new T
                                 and Observer.item
   with
      record
         subject_Responses : subject_Map_of_event_responses;
         relay_Target      : Observer.view;
      end record;

end lace.remote.make_Observer;
