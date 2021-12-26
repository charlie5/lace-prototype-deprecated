with
     interfaces.C.Pointers,
     interfaces.C.Strings,

     system.Address_To_Access_Conversions;

package swig.Pointers
--
-- Contains pointers to Swig related C type definitions not found in the 'interfaces.C' family.
--
is
   --  void_ptr
   --
   package C_void_ptr_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                             Element            => swig.void_ptr,
                                                             element_Array      => void_ptr_Array,
                                                             default_Terminator => system.null_Address);
   subtype void_ptr_Pointer is C_void_ptr_Pointers.Pointer;


   --  opaque struct_ptr
   --
   type opaque_structure_ptr       is access swig.opaque_structure;
   type opaque_structure_ptr_array is array (interfaces.c.Size_t range <>) of aliased opaque_structure_ptr;

   package C_opaque_structure_ptr_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                         Element            => opaque_structure_ptr,
                                                                         element_Array      => opaque_structure_ptr_array,
                                                                         default_Terminator => null);
   subtype opaque_structure_ptr_Pointer is C_opaque_structure_ptr_Pointers.Pointer;


   --  incomplete class
   --
   type incomplete_class_ptr is access swig.incomplete_class;
   type incomplete_class_ptr_array is array (interfaces.c.Size_t range <>) of aliased incomplete_class_ptr;

   package C_incomplete_class_ptr_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                         Element            => incomplete_class_ptr,
                                                                         element_Array      => incomplete_class_ptr_array,
                                                                         default_Terminator => null);
   subtype incomplete_class_ptr_Pointer is C_incomplete_class_ptr_Pointers.Pointer;


   --  bool*
   --
   package c_bool_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                         Element            => swig.bool,
                                                         element_Array      => bool_Array,
                                                         default_Terminator => 0);
   subtype bool_Pointer       is c_bool_Pointers.Pointer;
   type    bool_Pointer_array is array (interfaces.c.Size_t range <>) of aliased bool_Pointer;


   --  bool**
   --
   package C_bool_pointer_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                 Element            => bool_Pointer,
                                                                 element_Array      => bool_Pointer_array,
                                                                 default_Terminator => null);
   subtype bool_pointer_Pointer is C_bool_pointer_Pointers.Pointer;



   --  char* []
   --
   type chars_ptr_array is array (interfaces.c.Size_t range <>) of aliased interfaces.c.strings.chars_Ptr;   -- standard Ada does not have 'aliased'

   package C_chars_ptr_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                              Element            => interfaces.c.strings.chars_ptr,
                                                              element_Array      => chars_ptr_array,
                                                              default_Terminator => interfaces.c.strings.Null_Ptr);
   subtype chars_ptr_Pointer is C_chars_ptr_Pointers.Pointer;


   --  char** []
   --
   type chars_ptr_Pointer_array is array (interfaces.c.Size_t range <>) of aliased chars_ptr_Pointer;

   package C_chars_ptr_pointer_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                      Element            => chars_ptr_Pointer,
                                                                      element_Array      => chars_ptr_Pointer_array,
                                                                      default_Terminator => null);
   subtype chars_ptr_pointer_Pointer is C_chars_ptr_pointer_Pointers.Pointer;


   -- wchar_t*
   --
   package c_wchar_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                            Element            => interfaces.c.wchar_t,
                                                            element_Array      => interfaces.c.wchar_array,
                                                            default_Terminator => interfaces.c.wchar_t'First);
   subtype wchar_t_Pointer is c_wchar_t_Pointers.Pointer;


   --  signed char*
   --
   package c_signed_char_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                Element            => interfaces.c.signed_Char,
                                                                element_Array      => swig.signed_char_Array,
                                                                default_Terminator => 0);
   subtype signed_char_Pointer is c_signed_char_Pointers.Pointer;


   --  unsigned char*
   --
   package c_unsigned_char_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                  Element            => interfaces.c.unsigned_Char,
                                                                  element_Array      => unsigned_char_Array,
                                                                  default_Terminator => 0);
   subtype unsigned_char_Pointer is c_unsigned_char_Pointers.Pointer;


   --  short*
   --
   package c_short_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                          Element            => interfaces.c.Short,
                                                          element_Array      => short_Array,
                                                          default_Terminator => 0);
   subtype short_Pointer is c_short_Pointers.Pointer;



   --  unsigned short*
   --
   package c_unsigned_short_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                   Element            => interfaces.c.unsigned_Short,
                                                                   element_Array      => unsigned_short_Array,
                                                                   default_Terminator => 0);
   subtype unsigned_short_Pointer is c_unsigned_short_Pointers.Pointer;


   --  int*
   --
   package c_int_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                        Element            => interfaces.c.Int,
                                                        element_Array      => int_Array,
                                                        default_Terminator => 0);
   subtype int_Pointer is c_int_Pointers.Pointer;


   --  int**
   --
   type int_pointer_Array is array (interfaces.c.size_t range <>) of aliased int_Pointer;

   package c_int_pointer_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                Element            => int_Pointer,
                                                                element_Array      => int_pointer_Array,
                                                                default_Terminator => null);
   subtype int_pointer_Pointer is c_int_pointer_Pointers.Pointer;


   --  size_t*
   --
   package c_size_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                           Element            => interfaces.c.Size_t,
                                                           element_Array      => size_t_Array,
                                                           default_Terminator => 0);
   subtype size_t_Pointer is c_size_t_Pointers.Pointer;



   --  unsigned*
   --
   package c_unsigned_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                             Element            => interfaces.c.Unsigned,
                                                             element_Array      => unsigned_Array,
                                                             default_Terminator => 0);
   subtype unsigned_Pointer is c_unsigned_Pointers.Pointer;


   --  long*
   --
   package c_long_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                         Element            => interfaces.c.Long,
                                                         element_Array      => long_Array,
                                                         default_Terminator => 0);
   subtype long_Pointer is c_long_Pointers.Pointer;


   --  unsigned long*
   --
   package c_unsigned_long_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                  Element            => interfaces.c.unsigned_Long,
                                                                  element_Array      => unsigned_long_Array,
                                                                  default_Terminator => 0);
   subtype unsigned_long_Pointer is c_unsigned_long_Pointers.Pointer;


   --  long long*
   --
   package c_long_long_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                              Element            => swig.long_Long,
                                                              element_Array      => long_long_Array,
                                                              default_Terminator => 0);
   subtype long_long_Pointer is c_long_long_Pointers.Pointer;


   --  unsigned long long*
   --
   package c_unsigned_long_long_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                       Element            => swig.unsigned_long_Long,
                                                                       element_Array      => unsigned_long_long_Array,
                                                                       default_Terminator => 0);
   subtype unsigned_long_long_Pointer is c_unsigned_long_long_Pointers.Pointer;



   --  int8_t*
   --
   package c_int8_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                           Element            => swig.int8_t,
                                                           element_Array      => swig.int8_t_Array,
                                                           default_Terminator => 0);
   subtype int8_t_Pointer is c_int8_t_Pointers.Pointer;


   --  int16_t*
   --
   package c_int16_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                            Element            => swig.int16_t,
                                                            element_Array      => swig.int16_t_Array,
                                                            default_Terminator => 0);
   subtype int16_t_Pointer is c_int16_t_Pointers.Pointer;


   --  int32_t*
   --
   package c_int32_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                            Element            => swig.int32_t,
                                                            element_Array      => swig.int32_t_Array,
                                                            default_Terminator => 0);
   subtype int32_t_Pointer is c_int32_t_Pointers.Pointer;


   --  int64_t*
   --
   package c_int64_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                            Element            => swig.int64_t,
                                                            element_Array      => swig.int64_t_Array,
                                                            default_Terminator => 0);
   subtype int64_t_Pointer is c_int64_t_Pointers.Pointer;



   --  uint8_t*'
   --
   package c_uint8_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                            Element            => swig.uint8_t,
                                                            element_Array      => swig.uint8_t_Array,
                                                            default_Terminator => 0);
   subtype uint8_t_Pointer is c_uint8_t_Pointers.Pointer;


   --  uint16_t*'
   --
   package c_uint16_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                             Element            => swig.uint16_t,
                                                             element_Array      => swig.uint16_t_Array,
                                                             default_Terminator => 0);
   subtype uint16_t_Pointer is c_uint16_t_Pointers.Pointer;


   --  uint32_t*'
   --
   package c_uint32_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                             Element            => swig.uint32_t,
                                                             element_Array      => swig.uint32_t_Array,
                                                             default_Terminator => 0);
   subtype uint32_t_Pointer is c_uint32_t_Pointers.Pointer;


   --  uint64_t*'
   --
   package c_uint64_t_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                             Element            => swig.uint64_t,
                                                             element_Array      => swig.uint64_t_Array,
                                                             default_Terminator => 0);
   subtype uint64_t_Pointer is c_uint64_t_Pointers.Pointer;



   --  float*'
   package c_float_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                          Element            => interfaces.c.c_Float,
                                                          element_Array      => float_Array,
                                                          default_Terminator => 0.0);
   subtype float_Pointer is c_float_Pointers.Pointer;



   --  double*'
   --
   package c_double_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                           Element            => interfaces.c.Double,
                                                           element_Array      => double_Array,
                                                           default_Terminator => 0.0);
   subtype double_Pointer is c_double_Pointers.Pointer;



   --  long double*'
   --
   package c_long_double_Pointers is new interfaces.c.Pointers (Index              => interfaces.c.size_t,
                                                                Element            => interfaces.c.long_Double,
                                                                element_Array      => long_double_Array,
                                                                default_Terminator => 0.0);
   subtype long_double_Pointer is c_long_double_Pointers.Pointer;



   -- std::string
   --
   type std_string         is private;
   type std_string_Pointer is access all std_String;
   type std_string_Array   is array (interfaces.c.size_t range <>) of aliased std_String;



   --  Utility
   --
   package void_Conversions is new system.Address_To_Access_Conversions (swig.Void);



private

   type std_String is
      record
         M_dataplus : swig.void_ptr;    -- which is a subtype of system.Address
      end record;

end Swig.Pointers;


-- tbd: use sensible default_Terminator's.
