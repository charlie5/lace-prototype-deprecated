with
     collada.Library.geometries,
     collada.Library.controllers,
     collada.Library.animations,
     collada.Library.visual_scenes,

     XML,

     ada.Calendar.formatting,
     ada.Strings.fixed,
     ada.Characters.latin_1,
     ada.Text_IO;


package body collada.Document
is
   use ada.Strings.unbounded;


   ------------
   -- Utilities
   --

   function "+" (From : in String) return unbounded_String
     renames to_unbounded_String;


   function to_Time (From : in String) return ada.Calendar.Time
   is
      Pad   :          String  := From;
      Index : constant Natural := ada.Strings.fixed.Index (Pad, "T");
   begin
      if Index /= 0 then
         Pad (Index) := ' ';
      end if;

      return ada.Calendar.formatting.Value (Pad);

   exception
      when constraint_Error =>
         return ada.Calendar.Clock;   -- TODO: Temporary debug measure to handle unknown date formats.
   end to_Time;



   function to_int_Array (From : in String) return int_Array
   is
      use ada.Strings.fixed;

      the_Array : int_Array (1 .. 500_000);
      Count     : math.Index := 0;

      Start     : Natural := 1;
      Cursor    : Natural := Index (From, " ");

   begin
      if Cursor = 0
      then
         return (1 => Integer'Value (From));
      end if;

      loop
         if         From (Start .. Cursor-1) /= ""
           and then From (Start .. Cursor-1) /= "" & ada.Characters.latin_1.LF
         then
            Count             := Count + 1;
            the_Array (Count) := Integer'Value (From (Start .. Cursor-1));
         end if;

         Start  := Cursor + 1;
         Cursor := Index (From, " ", Start);

         exit when Cursor = 0;
      end loop;

      if Start <= From'Last
      then
         Count             := Count + 1;
         the_Array (Count) := Integer'Value (From (Start .. From'Last));
      end if;

      return the_Array (1 .. Count);
   end to_int_Array;



   function to_float_Array (From : in String) return float_Array
   is
   begin
      if From = ""
      then
         return float_Array' (1 .. 0 => <>);
      end if;

      declare
         use ada.Strings.fixed;

         the_Array : float_Array (1 .. 500_000);
         Count     : math.Index := 0;

         Start     : Integer    := 1;
         Cursor    : Integer    := Index (From, " ");

      begin
         if Cursor = 0
         then
            return (1 => math.Real'Value (From));
         end if;

         loop
            if         From (Start .. Cursor-1) /= ""
              and then From (Start .. Cursor-1) /= "" & ada.Characters.latin_1.LF
            then
               Count             := Count + 1;
               the_Array (Count) := math.Real'Value (From (Start .. Cursor-1));
            end if;

            Start  := Cursor + 1;
            Cursor := Index (From, " ", Start);

            exit when Cursor = 0;
         end loop;

         if From (Start .. From'Last) /= ""
         then
            Count             := Count + 1;
            the_Array (Count) := math.Real'Value (From (Start .. From'Last));
         end if;

         return the_Array (1 .. Count);
      end;
   end to_float_Array;



   function to_Text_array (From : in String) return Text_array
   is
   begin
      if From = ""
      then
         return Text_array' (1 .. 0 => <>);
      end if;

      declare
         use ada.Strings.fixed;

         the_Array : Text_array (1 .. 40_000);
         Count     : math.Index := 0;

         Start     : Integer    := 1;
         Cursor    : Integer    := Index (From, " ");

      begin
         if Cursor = 0
         then
            return (1 => +From);
         end if;

         loop
            if         From (Start .. Cursor-1) /= ""
              and then From (Start .. Cursor-1) /= "" & ada.Characters.latin_1.LF
            then
               Count             := Count + 1;
               the_Array (Count) := +From (Start .. Cursor-1);
            end if;

            Start  := Cursor + 1;
            Cursor := Index (From, " ", Start);

            exit when Cursor = 0;
         end loop;

         if From (Start .. From'Last) /= ""
         then
            Count             := Count + 1;
            the_Array (Count) := +From (Start .. From'Last);
         end if;

         return the_Array (1 .. Count);
      end;
   end to_Text_array;



   function to_Matrix (From : in String) return Matrix_4x4
   is
      the_Floats : constant math.Vector_16 := math.Vector_16 (to_float_Array (From));
   begin
      return math.to_Matrix_4x4 (the_Floats);
   end to_Matrix;



   function to_Source (From : in xml.Element) return collada.Library.Source
   is
      the_xml_Id          : constant access xml.Attribute_t := From.Attribute ("id");
      the_xml_float_Array : constant access xml.Element     := From.Child ("float_array");
      the_xml_text_Array  : constant access xml.Element     := From.Child ("Name_array");

      the_array_Length    : Natural;
      pragma Unreferenced (the_array_Length);
      the_Source          : Library.source;

   begin
      the_Source.Id := +the_xml_Id.Value;

      if the_xml_float_Array /= null
      then
         the_Source.array_Id := +the_xml_float_Array.Attribute ("id").Value;

         the_array_Length    := Natural'Value (the_xml_float_Array.Attribute ("count").Value);
         the_Source.Floats   := new float_Array' (to_float_Array (the_xml_float_Array.Data));

      elsif the_xml_text_Array /= null
      then
         the_Source.array_Id := +the_xml_text_Array.Attribute ("id").Value;

         the_array_Length    := Natural'Value (the_xml_text_Array.Attribute ("count").Value);
         the_Source.Texts    := new Text_array' (to_Text_array (the_xml_text_Array.Data));
      end if;

      return the_Source;
   end to_Source;



   function to_Input (From : in xml.Element) return collada.Library.Input_t
   is
      use collada.Library;

      the_xml_Semantic : constant access xml.Attribute_t := From.Attribute ("semantic");
      the_xml_Source   : constant access xml.Attribute_t := From.Attribute ("source");
      the_xml_Offset   : constant access xml.Attribute_t := From.Attribute ("offset");

      the_Input : Input_t;

   begin
      the_Input.Semantic := Semantic'Value (the_xml_Semantic.Value);
      the_Input.Source   :=                +the_xml_Source  .Value;

      if the_xml_Offset /= null
      then
         the_Input.Offset := Natural'Value (the_xml_Offset.Value);
      end if;

      return the_Input;
   end to_Input;



   function to_Vertices (From : in xml.Element) return collada.Library.geometries.Vertices
   is
      use collada.Library,
          collada.Library.geometries;

      the_xml_Id     : constant access xml.Attribute_t := From.Attribute ("id");
      the_xml_Inputs : constant        xml.Elements    := From.Children  ("input");

      the_Vertices   : geometries.Vertices;

   begin
      the_Vertices.Id     := +the_xml_Id.Value;
      the_Vertices.Inputs := new Inputs (the_xml_Inputs'Range);

      for i in the_xml_Inputs'Range
      loop
         the_Vertices.Inputs (i) := to_Input (the_xml_Inputs (i).all);
      end loop;

      return the_Vertices;
   end to_Vertices;



   function to_Polylist (From : in xml.Element) return collada.Library.geometries.Primitive
   is
      use collada.Library,
          collada.Library.geometries;

      the_xml_Count    : constant access xml.Attribute_t := From.Attribute ("count");
      the_xml_Material : constant access xml.Attribute_t := From.Attribute ("material");

      the_xml_Inputs   : constant        xml.Elements    := From.Children ("input");
      the_xml_vCount   : constant access xml.Element     := From.Child    ("vcount");
      the_xml_P        : constant access xml.Element     := From.Child    ("p");

      the_Polylist     : geometries.Primitive (polyList);

   begin
      the_Polylist.Count := Natural'Value (the_xml_Count.Value);

      if the_xml_Material /= null
      then
         the_Polylist.Material := +the_xml_Material.Value;
      end if;

      the_Polylist.Inputs := new Inputs (the_xml_Inputs'Range);

      for i in the_xml_Inputs'Range
      loop
         the_Polylist.Inputs (i) := to_Input (the_xml_Inputs (i).all);
      end loop;

      the_Polylist.vCount := new int_Array' (to_int_Array (the_xml_vCount.Data));
      the_Polylist.P_List := new int_array_List' (1 => new int_Array' (to_int_Array (the_xml_P.Data)));

      return the_Polylist;
   end to_Polylist;



   function to_Polygon (From : in xml.Element) return collada.Library.geometries.Primitive
   is
      use collada.Library,
          collada.Library.geometries;

      the_xml_Count    : constant access xml.Attribute_t := From.Attribute ("count");
      the_xml_Material : constant access xml.Attribute_t := From.Attribute ("material");

      the_xml_Inputs   : constant        xml.Elements    := From.Children ("input");
      the_xml_Ps       : constant        xml.Elements    := From.Children ("p");

      the_Polygons     : geometries.Primitive (Polygons);

   begin
      the_Polygons.Count := Natural'Value (the_xml_Count.Value);

      if the_xml_Material /= null
      then
         the_Polygons.Material := +the_xml_Material.Value;
      end if;

      -- Do inputs.
      --
      the_Polygons.Inputs := new Inputs (the_xml_Inputs'Range);

      for i in the_xml_Inputs'Range
      loop
         the_Polygons.Inputs (i) := to_Input (the_xml_Inputs (i).all);
      end loop;

      -- Do P list.
      --
      the_Polygons.P_List := new int_array_List (1 .. the_xml_Ps'Length);

      for i in the_Polygons.P_List'Range
      loop
         the_Polygons.P_List (i) := new int_Array' (to_int_Array (the_xml_Ps (i).Data));
      end loop;

      return the_Polygons;
   end to_Polygon;



   function to_Triangles (From : in xml.Element) return collada.Library.geometries.Primitive
   is
      use collada.Library,
          collada.Library.geometries;

      the_xml_Count    : constant access xml.Attribute_t := From.Attribute ("count");
      the_xml_Material : constant access xml.Attribute_t := From.Attribute ("material");

      the_xml_Inputs   : constant        xml.Elements    := From.Children ("input");
      the_xml_Ps       : constant        xml.Elements    := From.Children ("p");

      the_Triangles    : geometries.Primitive (Triangles);

   begin
      the_Triangles.Count := Natural'Value (the_xml_Count.Value);

      if the_xml_Material /= null
      then
         the_Triangles.Material := +the_xml_Material.Value;
      end if;

      -- Do inputs.
      --
      the_Triangles.Inputs := new Inputs (the_xml_Inputs'Range);

      for i in the_xml_Inputs'Range
      loop
         the_Triangles.Inputs (i) := to_Input (the_xml_Inputs (i).all);
      end loop;

      -- Do P list.
      --
      the_Triangles.P_List := new int_array_List (1 .. the_xml_Ps'Length);

      for i in the_Triangles.P_List'Range
      loop
         the_Triangles.P_List (i) := new int_Array' (to_int_Array (the_xml_Ps (i).Data));
      end loop;

      return the_Triangles;
   end to_Triangles;



   function to_Joints (From : in xml.Element) return collada.Library.controllers.Joints
   is
      use collada.Library,
          collada.Library.controllers;

      the_xml_Inputs : constant xml.Elements := From.Children  ("input");
      the_Joints     : controllers.Joints;
   begin
      the_Joints.Inputs := new Inputs (the_xml_Inputs'Range);

      for i in the_xml_Inputs'Range
      loop
         the_Joints.Inputs (i) := to_Input (the_xml_Inputs (i).all);
      end loop;

      return the_Joints;
   end to_Joints;



   function to_vertex_Weights (From : in xml.Element) return collada.Library.controllers.vertex_Weights
   is
      use collada.Library,
          collada.Library.controllers;

      the_xml_Count    : constant access xml.Attribute_t := From.Attribute ("count");

      the_xml_Inputs   : constant        xml.Elements    := From.Children ("input");
      the_xml_vCount   : constant access xml.Element     := From.Child    ("vcount");
      the_xml_V        : constant access xml.Element     := From.Child    ("v");

      the_Weights      : controllers.vertex_Weights;

   begin
      the_Weights.Count  := Natural'Value (the_xml_Count.Value);
      the_Weights.Inputs := new Inputs (the_xml_Inputs'Range);

      for i in the_xml_Inputs'Range
      loop
         the_Weights.Inputs (i) := to_Input (the_xml_Inputs (i).all);
      end loop;

      the_Weights.v_Count := new int_Array' (to_int_Array (the_xml_vCount.Data));
      the_Weights.V       := new int_array' (to_int_Array (the_xml_V.Data));

      return the_Weights;
   end to_vertex_Weights;



   function to_Sampler (From : in xml.Element) return collada.Library.animations.Sampler
   is
      use collada.Library,
          collada.Library.animations;

      the_xml_Id     : constant access xml.Attribute_t := From.Attribute ("id");
      the_xml_Inputs : constant        xml.Elements    := From.Children  ("input");

      the_Sampler    : animations.Sampler;

   begin
      the_Sampler.Id     := +the_xml_Id.Value;
      the_Sampler.Inputs := new Inputs (the_xml_Inputs'Range);

      for i in the_xml_Inputs'Range
      loop
         the_Sampler.Inputs (i) := to_Input (the_xml_Inputs (i).all);
      end loop;

      return the_Sampler;
   end to_Sampler;



   function to_Channel (From : in xml.Element) return collada.Library.animations.Channel
   is
      use collada.Library,
          collada.Library.animations;

      the_xml_Source : constant access xml.Attribute_t := From.Attribute ("source");
      the_xml_Target : constant access xml.Attribute_t := From.Attribute ("target");

      the_Channel    : animations.Channel;
   begin
      the_Channel.Source := +the_xml_Source.Value;
      the_Channel.Target := +the_xml_Target.Value;

      return the_Channel;
   end to_Channel;


   ---------------
   -- Construction
   --

   function to_Document (Filename : in String) return Item
   is
      use XML;

      the_xml_Tree     : constant        xml.Element := xml.to_XML (Filename);
      the_collada_Tree : constant access xml.Element := the_xml_Tree.Child (named => "COLLADA");

      the_Document     : Document.item;

   begin
      parse_the_asset_Element:
      declare
         the_Asset             : constant access xml.Element := the_collada_Tree.Child (named => "asset");

         the_Contributor       : constant access xml.Element := the_Asset.Child (named => "contributor");
         the_creation_Date     : constant access xml.Element := the_Asset.Child (named => "created");
         the_modification_Date : constant access xml.Element := the_Asset.Child (named => "modified");

         the_Unit              : constant access xml.Element := the_Asset.Child (named => "unit");
         the_up_Axis           : constant access xml.Element := the_Asset.Child (named => "up_axis");

      begin
         -- Parse the 'contributor' element.
         --
         if the_Contributor /= null
         then
            declare
               the_Author         : constant access xml.Element := the_Contributor .Child (named => "author");
               the_authoring_Tool : constant access xml.Element := the_Contributor .Child (named => "authoring_tool");
            begin
               if the_Author /= null
               then
                  the_Document.Asset.Contributor.Author := +the_Author.Data;
               end if;

               if the_authoring_Tool /= null
               then
                  the_document.asset.contributor.authoring_Tool := +the_authoring_Tool.Data;
               end if;
            end;
         end if;

         -- Parse the creation and modification dates.
         --
         if the_creation_Date /= null
         then
            the_document.asset.Created := to_Time (the_creation_Date.Data);
         end if;

         if the_modification_Date /= null
         then
            the_document.asset.Modified := to_Time (the_modification_Date.Data);
         end if;

         -- Parse the 'unit' element.
         --
         if the_Unit /= null
         then
            the_document.asset.Unit.Name  :=             +the_Unit.Attribute (named => "name") .Value;
            the_document.asset.Unit.Meter := Float'Value (the_Unit.Attribute (named => "meter").Value);
         end if;

         -- Parse the 'up_axis' element.
         --
         if the_up_Axis /= null
         then
            the_document.asset.up_Axis := collada.asset.up_Direction'Value (the_up_Axis.Data);
         end if;
      end parse_the_asset_Element;

      ---------------------------------
      --- Parse the 'library' elements.
      --

      parse_the_geometries_Library:
      declare
         the_Library : constant access xml.Element := the_collada_Tree.Child (named => "library_geometries");
      begin
         if the_Library /= null
         then
            declare
               use collada.Library.geometries;
               the_Geometries : constant xml.Elements := the_Library.Children (named => "geometry");
            begin
               the_document.Libraries.Geometries.Contents := new Geometry_array (the_Geometries'Range);

               for Each in the_Geometries'Range
               loop
                  declare
                     the_xml_Geometry : access xml.Element renames the_Geometries (Each);
                     the_Geometry     :        Geometry    renames the_Document.Libraries.Geometries.Contents (Each);

                     the_xml_Id       : constant access xml.Attribute_t'Class := the_xml_Geometry.Attribute ("id");
                     the_xml_Name     : constant access xml.Attribute_t'Class := the_xml_Geometry.Attribute ("name");

                  begin
                     the_Geometry.Id := +the_xml_Id.Value;

                     if the_xml_Name /= null
                     then
                        the_Geometry.Name := +the_xml_Name.Value;
                     end if;

                     parse_Mesh:
                     declare
                        the_xml_Mesh       :          access xml.Element  renames the_xml_Geometry.Child ("mesh");
                        the_xml_Vertices   : constant access xml.Element  :=      the_xml_Mesh    .Child ("vertices");
                        the_xml_Sources    : constant        xml.Elements :=      the_xml_Mesh.Children  ("source");
                     begin
                        the_Geometry.Mesh.Sources := new library.Sources (the_xml_Sources'Range);

                        -- Parse sources.
                        --
                        for i in the_xml_Sources'Range
                        loop
                           the_Geometry.Mesh.Sources (i) := to_Source (the_xml_Sources (i).all);
                        end loop;

                        -- Parse vertices.
                        --
                        the_Geometry.Mesh.Vertices := to_Vertices (the_xml_Vertices.all);

                        -- Parse primitives.
                        --
                        declare
                           the_xml_Polylists : constant xml.Elements := the_xml_Mesh.Children (named => "polylist");
                           the_xml_Polygons  : constant xml.Elements := the_xml_Mesh.Children (named => "polygons");
                           the_xml_Triangles : constant xml.Elements := the_xml_Mesh.Children (named => "triangles");

                           primitive_Count   :          Natural := 0;
                           primitive_Total   : constant Natural :=   the_xml_Polylists'Length
                                                                   + the_xml_Polygons 'Length
                                                                   + the_xml_Triangles'Length;
                        begin
                           the_Geometry.Mesh.Primitives := new Primitives (1 .. primitive_Total);

                           -- polylists
                           --
                           for i in the_xml_Polylists'Range
                           loop
                              primitive_Count                                := primitive_Count + 1;
                              the_Geometry.Mesh.Primitives (primitive_Count) := to_Polylist (the_xml_Polylists (i).all);
                           end loop;

                           -- polygons
                           --
                           for i in the_xml_Polygons'Range
                           loop
                              primitive_Count                                := primitive_Count + 1;
                              the_Geometry.Mesh.Primitives (primitive_Count) := to_Polygon (the_xml_Polygons (i).all);
                           end loop;

                           -- Triangles
                           --
                           for i in the_xml_Triangles'Range
                           loop
                              primitive_Count                                := primitive_Count + 1;
                              the_Geometry.Mesh.Primitives (primitive_Count) := to_Triangles (the_xml_Triangles (i).all);
                           end loop;
                        end;
                     end parse_Mesh;

                  end;
               end loop;
            end;

         end if;
      end parse_the_geometries_Library;

      -- Parse the controllers library.
      --
      declare
         the_Library : constant access xml.Element := the_collada_Tree.Child (named => "library_controllers");
      begin
         if the_Library /= null
         then
            declare
               use collada.Library.controllers;
               the_Controllers : constant xml.Elements := the_Library.Children (named => "controller");
            begin
               the_Document.Libraries.controllers.Contents := new Controller_array (the_Controllers'Range);

               for Each in the_Controllers'Range
               loop
                  declare
                     the_xml_Controller : access xml.Element renames the_Controllers (Each);
                     the_Controller     :        Controller  renames the_Document.Libraries.controllers.Contents (Each);

                     the_xml_Id   : constant access xml.Attribute_t'Class := the_xml_Controller.Attribute ("id");
                     the_xml_Name : constant access xml.Attribute_t'Class := the_xml_Controller.Attribute ("name");

                  begin
                     the_Controller.Id := +the_xml_Id.Value;

                     if the_xml_Name /= null
                     then
                        the_Controller.Name := +the_xml_Name.Value;
                     end if;

                     parse_Skin:
                     declare
                        the_xml_Skin    : access xml.Element renames the_xml_Controller.Child ("skin");

                        the_xml_Sources : constant        xml.Elements := the_xml_Skin.Children ("source");
                        the_xml_Matrix  : constant access xml.Element  := the_xml_Skin.Child    ("bind_shape_matrix");
                        the_xml_Joints  : constant access xml.Element  := the_xml_Skin.Child    ("joints");
                        the_xml_Weights : constant access xml.Element  := the_xml_Skin.Child    ("vertex_weights");
                     begin
                        the_Controller.Skin.main_Source       := +the_xml_Skin.Attribute ("source").Value;
                        the_Controller.Skin.bind_shape_Matrix :=  to_float_Array (the_xml_Matrix.Data);

                        -- Parse sources.
                        --
                        the_Controller.Skin.Sources := new library.Sources (the_xml_Sources'Range);

                        for i in the_xml_Sources'Range
                        loop
                           the_Controller.Skin.Sources (i) := to_Source (the_xml_Sources (i).all);
                        end loop;

                        the_Controller.Skin.Joints         := to_Joints         (the_xml_Joints.all);
                        the_Controller.Skin.vertex_Weights := to_vertex_Weights (the_xml_Weights.all);
                     end parse_Skin;

                  end;
               end loop;
            end;
         end if;
      end;

      -- Parse the visual_Scenes library.
      --
      declare
         the_Library : constant access xml.Element := the_collada_Tree.Child (named => "library_visual_scenes");
      begin
         if the_Library /= null
         then
            declare
               use collada.Library.visual_scenes;
               the_visual_Scenes : constant xml.Elements := the_Library.Children (named => "visual_scene");
            begin
               the_Document.Libraries.visual_Scenes.Contents := new visual_Scene_array (the_visual_Scenes'Range);

               for Each in the_visual_Scenes'Range
               loop
                  declare
                     the_visual_Scene   :        visual_Scene renames the_document.Libraries.visual_Scenes.Contents (Each);
                     the_xml_Scene      : access xml.Element  renames the_visual_Scenes (Each);

                     the_xml_Id         : constant access xml.Attribute_t'Class := the_xml_Scene.Attribute ("id");
                     the_xml_Name       : constant access xml.Attribute_t'Class := the_xml_Scene.Attribute ("name");

                  begin
                     the_visual_Scene.Id := +the_xml_Id.Value;

                     if the_xml_Name /= null
                     then
                        the_visual_Scene.Name := +the_xml_Name.Value;
                     end if;

                     parse_Nodes:
                     declare
                        the_xml_root_Node : constant access xml.Element := the_xml_Scene.Child ("node");


                        function to_Node (the_XML : access xml.Element;
                                          Parent  : in     Library.visual_scenes.Node_view) return Library.visual_scenes.Node_view
                        is
                           the_xml_Sid       : constant access xml.Attribute_t'Class := the_xml.Attribute ("sid");
                           the_xml_Id        : constant access xml.Attribute_t'Class := the_xml.Attribute ("id");
                           the_xml_Name      : constant access xml.Attribute_t'Class := the_xml.Attribute ("name");
                           the_xml_Type      :          access xml.Attribute_t'Class := the_xml.Attribute ("type");

                           the_xml_Translate :          access xml.Element  := the_xml.Child    ("translate");
                           the_xml_Scale     :          access xml.Element  := the_xml.Child    ("scale");
                           the_xml_Rotates   :                 xml.Elements := the_xml.Children ("rotate");
                           the_xml_Children  :                 xml.Elements := the_xml.Children ("node");

                           the_Node : constant Library.visual_scenes.Node_view := new Library.visual_scenes.Node;

                        begin
                           if the_xml_Id /= null
                           then
                              the_Node.Id_is  (+the_xml_Id.Value);
                           end if;

                           if the_xml_Sid /= null
                           then
                              the_Node.Sid_is (+the_xml_Sid.Value);
                           end if;

                           if the_xml_Name /= null
                           then
                              the_Node.Name_is (+the_xml_Name.Value);
                           end if;

                           the_Node.Parent_is (Parent);

                           -- Parse children.
                           --
                           declare
                              the_xml_Children : constant xml.Elements := the_XML.Children;
                              the_Child        : access   xml.Element;
                           begin
                              for i in the_xml_Children'Range
                              loop
                                 the_Child := the_xml_Children (i);

                                 if    the_Child.Name = "translate"
                                 then
                                    the_Node.add (Transform' (Kind   => Translate,
                                                              Sid    => to_Text (the_Child.Attribute ("sid").Value),
                                                              Vector => Vector_3 (to_Float_array (the_Child.Data))));

                                 elsif the_Child.Name = "rotate"
                                 then
                                    declare
                                       use collada.Math;
                                       the_Data : constant Vector_4 := Vector_4 (to_Float_array (the_Child.Data));
                                    begin
                                       the_Node.add (Transform' (Kind  => Rotate,
                                                                 Sid   => to_Text (the_Child.Attribute ("sid").Value),
                                                                 Axis  => Vector_3 (the_Data (1 .. 3)),
                                                                 Angle => to_Radians (math.Degrees (the_Data (4)))));
                                    end;

                                 elsif the_Child.Name = "scale"
                                 then
                                    the_Node.add (Transform' (Kind  => Scale,
                                                              Sid   => to_Text (the_Child.Attribute ("sid").Value),
                                                              Scale => Vector_3 (to_Float_array (the_Child.Data))));

                                 elsif the_Child.Name = "matrix"
                                 then
                                    declare
                                       the_Data      : constant        Matrix_4x4            := to_Matrix (the_Child.Data);   -- Will be column vectors.
                                       the_child_Sid : constant access xml.Attribute_t'Class := the_Child.Attribute ("sid");
                                       the_sid_Text  : Text;
                                    begin
                                       if the_child_Sid = null
                                       then
                                          the_sid_Text := to_Text ("");
                                       else
                                          the_sid_Text := to_Text (the_child_Sid.Value);
                                       end if;

                                       the_Node.add (Transform' (Kind   => full_Transform,
                                                                 Sid    => the_sid_Text,
                                                                 Matrix => the_Data));
                                    end;

                                 elsif the_Child.Name = "node"
                                 then
                                    the_Node.add (the_Child => to_Node (the_Child, Parent => the_Node));   -- Recurse.

                                 elsif the_Child.Name = "instance_controller"
                                 then
                                    declare
                                       the_skeleton_Child : constant access xml.Element := the_Child.Child ("skeleton");
                                    begin
                                       the_Document.Libraries.visual_Scenes.skeletal_Root := +the_skeleton_Child.Data (2 .. the_skeleton_Child.Data'Last);
                                    end;

                                 elsif the_Child.Name = "instance_geometry"
                                 then
                                    ada.Text_IO.put_Line ("TODO: Handle instance_geometry.");

                                 else
                                    ada.Text_IO.put_Line ("TODO: Unhandled collada 'visual scene element' found: " & the_Child.Name & ".");
                                 end if;
                              end loop;
                           end;

                           return the_Node;
                        end to_Node;

                     begin
                        the_visual_Scene.root_Node := to_Node (the_xml_root_Node, Parent => null);
                     end parse_Nodes;
                  end;
               end loop;
            end;
         end if;
      end;

      -- Parse the animations library.
      --
      declare
         the_Library : constant access xml.Element := the_collada_Tree.Child (named => "library_animations");
      begin
         if the_Library /= null
         then
            declare
               use collada.Library.animations;
               the_Animations : constant xml.Elements := the_Library.Children (named => "animation");
            begin
               the_document.Libraries.animations.Contents := new Animation_array (the_Animations'Range);

               for Each in the_Animations'Range
               loop
                  declare
                     the_Animation     :        Animation   renames the_document.Libraries.animations.Contents (Each);

                     child_Animation   : constant access xml.Element := the_Animations (Each).Child ("animation");
                     the_xml_Animation : constant access xml.Element := (if child_Animation = null then the_Animations (Each) else child_Animation);
                     --  the_xml_Animation : access xml.Element renames the_Animations (Each); --.Child ("animation");

                     the_xml_Id   : constant access xml.Attribute_t'Class := the_xml_Animation.Attribute ("id");
                     the_xml_Name : constant access xml.Attribute_t'Class := the_xml_Animation.Attribute ("name");

                  begin
                     the_Animation.Id := +the_xml_Id.Value;

                     if the_xml_Name /= null
                     then
                        the_Animation.Name := +the_xml_Name.Value;
                     end if;

                     the_Animation.Sampler := to_Sampler (the_xml_Animation.Child ("sampler").all);
                     the_Animation.Channel := to_Channel (the_xml_Animation.Child ("channel").all);

                     parse_Sources:
                     declare
                        the_xml_Sources : constant xml.Elements := the_xml_Animation.Children ("source");
                     begin
                        the_Animation.Sources := new library.Sources (the_xml_Sources'Range);

                        for i in the_xml_Sources'Range
                        loop
                           the_Animation.Sources (i) := to_Source (the_xml_Sources (i).all);
                        end loop;
                     end parse_Sources;
                  end;
               end loop;
            end;
         end if;
      end;


      --- Parse the 'scene' element.
      --
      -- TODO

      return the_Document;
   end to_Document;



   function Asset (Self : in Item) return collada.Asset.item
   is
   begin
      return Self.Asset;
   end Asset;



   function Libraries (Self : in Item) return collada.Libraries.item
   is
   begin
      return Self.Libraries;
   end Libraries;


end collada.Document;
