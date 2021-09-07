with
     openGL.Buffer.general;

package openGL.Buffer.long_indices is new openGL.Buffer.general (base_Object   => Buffer.element_array_Object,
                                                                 Index         => long_Index_t,
                                                                 Element       => long_Index_t,
                                                                 Element_Array => long_Indices);
