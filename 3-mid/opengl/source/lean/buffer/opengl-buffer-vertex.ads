with
     openGL.Buffer.general;

package openGL.Buffer.vertex is new openGL.Buffer.general (base_object   => openGL.Buffer.array_Object,
                                                           index         => openGL.Index_t,
                                                           element       => openGL.Site,
                                                           element_array => openGL.Sites);
