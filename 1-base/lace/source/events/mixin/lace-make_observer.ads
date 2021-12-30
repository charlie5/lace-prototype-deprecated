with
     lace.Event,
     lace.Response,
     lace.Observer;

private
with
     ada.Containers.indefinite_hashed_Maps,
     ada.Strings.Hash;


generic
   type T is abstract tagged limited private;

package lace.make_Observer
--
--  Makes a user class T into an event Observer.
--
is
   pragma remote_Types;

   type Item is abstract limited new T
                                 and Observer.item with private;
   type View is access all Item'Class;


   procedure destroy (Self : in out Item);


   ------------
   -- Responses
   --

   overriding
   procedure add (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in Event.Kind;
                                        from_Subject : in Event.subject_Name);
   overriding
   procedure rid (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in Event.Kind;
                                        from_Subject : in Event.subject_Name);
   overriding
   procedure relay_responseless_Events (Self : in out Item;   To : in Observer.view);


   -------------
   -- Operations
   --

   overriding
   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := event.null_Event;
                                            from_Subject : in Event.subject_Name);
   overriding
   procedure respond (Self : access Item);



private
   ----------------------
   -- Event response maps
   --
   use type event.Kind;
   use type Response.view;

   package event_response_Maps     is new ada.Containers.indefinite_hashed_Maps (key_type        => Event.Kind,
                                                                                 element_type    => Response.view,
                                                                                 hash            => Event.Hash,
                                                                                 equivalent_keys => "=");
   subtype event_response_Map      is event_response_Maps.Map;
   type    event_response_Map_view is access all event_response_Map;


   ----------------------------------
   -- Subject maps of event responses
   --

   package subject_Maps_of_event_responses
   is new ada.Containers.indefinite_hashed_Maps (key_type        => Event.subject_Name,
                                                 element_type    => event_response_Map_view,
                                                 hash            => ada.Strings.Hash,
                                                 equivalent_keys => "=");
   subtype subject_Map_of_event_responses is subject_Maps_of_event_responses.Map;


   -----------------
   -- Safe Responses
   --
   protected
   type safe_Responses
   is
      procedure destroy;

      ------------
      -- Responses
      --

      procedure add (Self         : access Item'Class;
                     the_Response : in     Response.view;
                     to_Kind      : in     Event.Kind;
                     from_Subject : in     Event.subject_Name);

      procedure rid (Self         : access Item'Class;
                     the_Response : in     Response.view;
                     to_Kind      : in     Event.Kind;
                     from_Subject : in     Event.subject_Name);

      procedure relay_responseless_Events (To : in Observer.view);

      function  relay_Target return Observer.view;

      function  Contains (Subject : in Event.subject_Name) return Boolean;
      function  Element  (Subject : in Event.subject_Name) return event_response_Map;

      -------------
      -- Operations
      --

      procedure receive (Self         : access Item'Class;
                         the_Event    : in     Event.item'Class := Event.null_Event;
                         from_Subject : in     Event.subject_Name);

   private
      my_Responses    : subject_Map_of_event_responses;
      my_relay_Target : Observer.view;
   end safe_Responses;


   ----------------
   -- Observer Item
   --
   type Item is abstract limited new T
                                 and Observer.item
   with
      record
         Responses : safe_Responses;
      end record;

end lace.make_Observer;
