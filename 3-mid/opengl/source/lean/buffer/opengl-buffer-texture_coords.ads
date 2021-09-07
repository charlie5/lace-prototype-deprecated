with
     openGL.Buffer.general;

package openGL.Buffer.texture_coords is new openGL.Buffer.general (base_Object   => Buffer.array_Object,
                                                                   Index         => Index_t,
                                                                   Element       => Coordinate_2D,
                                                                   Element_Array => Coordinates_2D);
