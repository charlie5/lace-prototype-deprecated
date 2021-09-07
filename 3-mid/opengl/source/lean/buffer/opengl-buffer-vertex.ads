with
     openGL.Buffer.general;

package openGL.Buffer.vertex is new openGL.Buffer.general (base_Object   => Buffer.array_Object,
                                                           Index         => Index_t,
                                                           Element       => Site,
                                                           Element_Array => Sites);
