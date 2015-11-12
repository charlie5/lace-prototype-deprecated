with
     openGL.Buffer.general;

package openGL.Buffer.texture_coords is new openGL.Buffer.general (base_object   => openGL.Buffer.array_Object,
                                                                   index         => openGL.Index_t,
                                                                   element       => openGL.Coordinate_2D,
                                                                   element_array => openGL.Coordinates_2D);
