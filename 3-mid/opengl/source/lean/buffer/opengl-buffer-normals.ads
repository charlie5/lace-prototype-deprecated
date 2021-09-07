with
     openGL.Buffer.general;

package openGL.Buffer.normals is new openGL.Buffer.general (base_Object   => Buffer.array_Object,
                                                            Index         => Index_t,
                                                            Element       => Normal,
                                                            Element_Array => Normals);
