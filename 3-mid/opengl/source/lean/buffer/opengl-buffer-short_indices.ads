with
     openGL.Buffer.general;

package openGL.Buffer.short_indices is new openGL.Buffer.general (base_object   => openGL.Buffer.element_array_Object,
                                                                  index         => openGL.long_Index_t,
                                                                  element       => openGL.short_Index_t,
                                                                  element_array => openGL.short_Indices);
