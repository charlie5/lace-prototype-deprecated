with
     collada.Library.geometries,
     collada.Library.controllers,
     collada.Library.animations,
     collada.Library.visual_scenes;

package collada.Libraries
--
-- Provides a container for the specific collada library packages.
--
is

   type Item is
      record
         Geometries    : collada.Library.geometries   .item;
         Controllers   : collada.Library.controllers  .item;
         visual_Scenes : collada.Library.visual_scenes.item;
         Animations    : collada.Library.animations   .item;
      end record;

end collada.Libraries;
