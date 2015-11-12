with
     openGL.Buffer.general;

package openGL.Buffer.normals is new openGL.Buffer.general (base_object   => openGL.Buffer.array_Object,
                                                            index         => openGL.Index_t,
                                                            element       => openGL.Normal,
                                                            element_array => openGL.Normals);
