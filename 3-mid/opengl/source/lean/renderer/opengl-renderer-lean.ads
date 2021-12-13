with
     openGL.Context,
     openGL.Surface,
     openGL.Geometry,
     openGL.Model,
     openGL.Visual,
     openGL.Impostor,
     openGL.Texture,
     openGL.Font,
     openGL.Light;

limited
with
     openGL.Camera;

private
with
     ada.Containers.hashed_Maps,
     ada.unchecked_Conversion;


package openGL.Renderer.lean
--
-- Provides a rendering engine for the 'lean' GL profile.
--
is
   type Item is limited new Renderer.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure define  (Self : access Item);
   procedure destroy (Self : in out Item);

   procedure free    (Self : in out View);


   --------------
   --- Attributes
   --

   function  new_Light (Self : in out Item)           return Light.item;
   procedure set       (Self : in out Item;   the_Light : in Light.item);
   procedure rid       (Self : in out Item;   the_Light : in Light.item);
   function  Light     (Self : in out Item;   Id        : in light.Id_t) return openGL.Light.item;
   function  fetch     (Self : in out Item)                              return openGL.Light.items;

   type context_Setter is access procedure;
   type Swapper        is access procedure;

   procedure Context_is        (Self : in out Item;   Now : in Context.view);
   procedure Context_Setter_is (Self : in out Item;   Now : in context_Setter);
   procedure Swapper_is        (Self : in out Item;   Now : in Swapper);


   --------------
   --  Operations
   --

   type impostor_Update
   is
      record
         Impostor : openGL.Impostor.view;

         current_Width_pixels  : gl.GLsizei;
         current_Height_pixels : gl.GLsizei;

         current_copy_x_Offset : gl.GLsizei;
         current_copy_y_Offset : gl.GLsizei;
         current_copy_X        : gl.GLsizei;
         current_copy_Y        : gl.GLsizei;
         current_copy_Width    : gl.GLsizei;
         current_copy_Height   : gl.GLsizei;

         current_Camera_look_at_Rotation : Matrix_3x3;
      end record;

   type impostor_Updates is array (Positive range <>) of impostor_Update;



   procedure queue_Impostor_updates (Self : in out Item;    the_Updates   : in     impostor_Updates;
                                                            the_Camera    : access Camera.item'Class);

   procedure queue_Visuals          (Self : in out Item;    the_Visuals   : in     Visual.views;
                                                            the_Camera    : access Camera.item'Class);

   procedure start_Engine (Self : in out Item);
   procedure  stop_Engine (Self : in out Item);

   procedure render       (Self : in out Item;   to_Surface : in Surface.view := null);
   procedure add_Font     (Self : in out Item;   font_Id    : in Font.font_Id);
   procedure Screenshot   (Self : in out Item;   Filename   : in String;
                                                 with_Alpha : in Boolean := False);

   function  is_Busy      (Self : in Item) return Boolean;

   procedure draw         (Self : in out Item;   the_Visuals            : in Visual.views;
                                                 camera_world_Transform : in Matrix_4x4;
                                                 view_Transform         : in Matrix_4x4;
                                                 perspective_Transform  : in Matrix_4x4;
                                                 clear_Frame            : in Boolean;
                                                 to_Surface             : in Surface.view := null);
   --
   --  Raises buffer_Overflow if the renderer is unable to cope with the new 'draw'.


   procedure free (Self : in out Item;   the_Model    : in Model   .view);
   procedure free (Self : in out Item;   the_Impostor : in Impostor.view);

   buffer_Overflow   : exception;
   Texture_not_found : exception;



private

   type Camera_view is access all openGL.Camera.item'Class;

   max_Visuals : constant := 20_000;

   ----------
   -- Updates
   --

   type updates_for_Camera is
      record
         impostor_Updates      : lean.impostor_Updates (1 .. max_Visuals);
         impostor_updates_Last : Natural := 0;

         Visuals               : Visual.views (1 .. max_Visuals);
         visuals_Last          : Natural := 0;
      end record;

   type Updates_for_Camera_view is access Updates_for_Camera;

   function Hash                   is new ada.unchecked_Conversion   (Camera_view, ada.Containers.Hash_type);
   package  camera_Maps_of_updates is new ada.Containers.Hashed_Maps (Camera_view,
                                                                      updates_for_Camera_view,
                                                                      Hash,
                                                                      "=");
   type camera_updates_Couple is
      record
         Camera  : Camera_view;
         Updates : Updates_for_Camera_view;
      end record;

   type camera_updates_Couples is array (Positive range <>) of camera_updates_Couple;


   protected
   type safe_camera_Map_of_updates
   is
      procedure define;
      procedure destruct;

      procedure add (the_Updates : in impostor_Updates;
                     the_Camera  : in Camera_view);

      procedure add (the_Visuals : in Visual.views;
                     the_Camera  : in Camera_view);

      procedure fetch_all_Updates (the_Updates : out camera_updates_Couples;
                                   Length      : out Natural);

   private
      Map_1       : aliased camera_Maps_of_updates.Map;
      Map_2       : aliased camera_Maps_of_updates.Map;
      current_Map : access  camera_Maps_of_updates.Map;
   end safe_camera_Map_of_updates;


   -- visual_geometry_Couple
   --

   type visual_geometry_Couple is
      record
         Visual   : openGL.Visual  .view;
         Geometry : openGL.Geometry.view;
      end record;

   type visual_geometry_Couples      is array (math.Index range <>) of visual_geometry_Couple;
   type visual_geometry_Couples_view is access all visual_geometry_Couples;


   -- graphics_Models
   --

   type graphics_Models is array (1 .. max_Visuals) of Model.view;

   protected
   type safe_Models
   is
      procedure add   (the_Model  : in     Model.view);
      procedure fetch (the_Models :    out graphics_Models;
                       Count      :    out Natural);
   private
      my_Models : graphics_Models;
      my_Count  : Natural        := 0;
   end safe_Models;


   -- Impostors
   --

   type Impostor_Set is array (1 .. max_Visuals) of Impostor.view;

   protected
   type safe_Impostors
   is
      procedure add   (the_Impostor : in     Impostor.view);
      procedure fetch (Impostors    :    out Impostor_Set;
                       Count        :    out Natural);
   private
      the_Impostors : Impostor_Set;
      the_Count     : Natural := 0;
   end safe_Impostors;


   ----------
   --- Lights
   --

   function Hash (Id : in openGL.light.Id_t) return ada.Containers.Hash_type;
   use type openGL.Light.Id_t,
            openGL.Light.item;

   package  id_Maps_of_light is new ada.Containers.hashed_Maps (Key_type        => openGL.light.Id_t,
                                                                Element_type    => openGL.Light.item,
                                                                Hash            => Hash,
                                                                equivalent_Keys => "=");
   subtype  id_Map_of_light is id_Maps_of_light.Map;

   protected
   type safe_Lights
   is
      procedure add (Light : in openGL.Light.item);
      procedure set (Light : in openGL.Light.item);
      procedure rid (Light : in openGL.Light.item);

      function  get (Id    : in openGL.light.Id_t) return openGL.Light.item;
      function  fetch                              return openGL.Light.items;
   private
      the_Lights : id_Map_of_light;
   end safe_Lights;


   -- Engine
   --

   task type Engine (Self : access Item'Class)
   is
      entry start      (Context    : in openGL.Context.view);
      entry Stop;
      entry render;
      entry add_Font   (font_Id    : in Font.font_Id);
      entry Screenshot (Filename   : in String;
                        with_Alpha : in Boolean := False);

      pragma Storage_Size (100_000_000);
   end Engine;


   -- Renderer
   --

   type Item is limited new Renderer.item with
      record
         Lights             :         safe_Lights;
         prior_Light_Id     :         openGL.Light.Id_t := 0;

         Textures           : aliased Texture.name_Map_of_texture;
         Fonts              :         Font.font_id_Map_of_font;

         all_opaque_Couples :         visual_geometry_Couples_view := new visual_geometry_Couples (1 .. max_Visuals);
         all_lucid_Couples  :         visual_geometry_Couples_view := new visual_geometry_Couples (1 .. max_Visuals);

         obsolete_Models    :         safe_Models;
         obsolete_Impostors :         safe_Impostors;

         texture_Pool       : aliased Texture.Pool;

         safe_Camera_updates_Map
                            : aliased safe_camera_Map_of_updates;

         Engine             :         lean.Engine (Self => Item'Access);

         Context            :         openGL.Context.view;
         context_Setter     :         lean.context_Setter;
         Swapper            :         lean.Swapper;
         swap_Required      :         Boolean;
         is_Busy            :         Boolean := False;
      end record;


   procedure update_Impostors_and_draw_Visuals
                                (Self : in out Item;   all_Updates : in camera_updates_Couples);

   procedure update_Impostors   (Self : in out Item;   the_Updates            : in impostor_Updates;
                                                       camera_world_Transform : in Matrix_4x4;
                                                       view_Transform         : in Matrix_4x4;
                                                       perspective_Transform  : in Matrix_4x4);
   procedure free_old_Models    (Self : in out Item);
   procedure free_old_Impostors (Self : in out Item);


end openGL.Renderer.lean;
