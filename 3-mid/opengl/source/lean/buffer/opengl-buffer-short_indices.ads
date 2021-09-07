with
     openGL.Buffer.general;

package openGL.Buffer.short_indices is new openGL.Buffer.general (base_Object   => Buffer.element_array_Object,
                                                                  Index         => long_Index_t,
                                                                  Element       => short_Index_t,
                                                                  Element_Array => short_Indices);
