with
     openGL.Buffer.general;

package openGL.Buffer.indices is new openGL.Buffer.general (base_object   => openGL.Buffer.element_array_Object,
                                                            index         => openGL.long_Index_t,
                                                            element       => openGL.Index_t,
                                                            element_array => openGL.Indices);
