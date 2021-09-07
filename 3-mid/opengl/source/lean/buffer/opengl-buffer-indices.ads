with
     openGL.Buffer.general;

package openGL.Buffer.indices is new openGL.Buffer.general (base_Object   => Buffer.element_array_Object,
                                                            Index         => long_Index_t,
                                                            Element       => Index_t,
                                                            Element_Array => Indices);
