module DWARF where

import Data.Maybe
import qualified Data.Map as Map

data DWARFAttributeSpec =
  DWARFAttributeSpec
  {
    specName :: Int,
    specForm :: Int
  }
  deriving (Show)

data DWARFAbbreviation =
  DWARFAbbreviation
  {
    abbreviationKey :: [Char],
    abbreviationTag :: Int,
    abbreviationHasChildren :: Bool,
    abbreviationSpecs :: [DWARFAttributeSpec],
    abbreviationCode :: Int    
  }
  deriving (Show)
    
dwarfTags =
  [(0x1,"DW_TAG_array_type"),
   (0x2,"DW_TAG_class_type"),
   (0x3,"DW_TAG_entry_point"),
   (0x4,"DW_TAG_enumeration_type"),
   (0x5,"DW_TAG_formal_parameter"),
   (0x8,"DW_TAG_imported_declaration"),
   (0xa,"DW_TAG_label"),
   (0xb,"DW_TAG_lexical_block"),
   (0xd,"DW_TAG_member"),
   (0xf,"DW_TAG_pointer_type"),
   (0x10,"DW_TAG_reference_type"),
   (0x11,"DW_TAG_compile_unit"),
   (0x12,"DW_TAG_string_type"),
   (0x13,"DW_TAG_structure_type"),
   (0x15,"DW_TAG_subroutine_type"),
   (0x16,"DW_TAG_typedef"),
   (0x17,"DW_TAG_union_type"),
   (0x18,"DW_TAG_unspecified_parameters"),
   (0x19,"DW_TAG_variant"),
   (0x1a,"DW_TAG_common_block"),
   (0x1b,"DW_TAG_common_inclusion"),
   (0x1c,"DW_TAG_inheritance"),
   (0x1d,"DW_TAG_inlined_subroutine"),
   (0x1e,"DW_TAG_module"),
   (0x1f,"DW_TAG_ptr_to_member_type"),
   (0x20,"DW_TAG_set_type"),
   (0x21,"DW_TAG_subrange_type"),
   (0x22,"DW_TAG_with_stmt"),
   (0x23,"DW_TAG_access_declaration"),
   (0x24,"DW_TAG_base_type"),
   (0x25,"DW_TAG_catch_block"),
   (0x26,"DW_TAG_const_type"),
   (0x27,"DW_TAG_constant"),
   (0x28,"DW_TAG_enumerator"),
   (0x29,"DW_TAG_file_type"),
   (0x2a,"DW_TAG_friend"),
   (0x2b,"DW_TAG_namelist"),
   (0x2c,"DW_TAG_namelist_item"),
   (0x2d,"DW_TAG_packed_type"),
   (0x2e,"DW_TAG_subprogram"),
   (0x2f,"DW_TAG_template_type_parameter"),
   (0x30,"DW_TAG_template_value_parameter"),
   (0x31,"DW_TAG_thrown_type"),
   (0x32,"DW_TAG_try_block"),
   (0x33,"DW_TAG_variant_part"),
   (0x34,"DW_TAG_variable"),
   (0x35,"DW_TAG_volatile_type"),
   (0x36,"DW_TAG_dwarf_procedure"),
   (0x37,"DW_TAG_restrict_type"),
   (0x38,"DW_TAG_interface_type"),
   (0x39,"DW_TAG_namespace"),
   (0x3a,"DW_TAG_imported_module"),
   (0x3b,"DW_TAG_unspecified_type"),
   (0x3c,"DW_TAG_partial_unit"),
   (0x3d,"DW_TAG_imported_unit"),
   (0x3f,"DW_TAG_condition"),
   (0x40,"DW_TAG_shared_type"),
   (0x41,"DW_TAG_type_unit"),
   (0x42,"DW_TAG_rvalue_reference_type"),
   (0x43,"DW_TAG_template_alias"),
   (0x4080,"DW_TAG_lo_user"),
   (0xffff,"DW_TAG_hi_user")]

dwarfTagEncodingToStringMap =
  Map.fromList (map (\t -> (fst t :: Int,snd t)) dwarfTags)

dwarfStringToTagEncodingMap =
  Map.fromList (map (\t -> (snd t,fst t :: Int)) dwarfTags) 

dwarfTagEncodingToString :: Int -> [Char]
dwarfTagEncodingToString encoding =
  fromJust (Map.lookup encoding dwarfTagEncodingToStringMap)
  
dwarfStringToTagEncoding :: [Char] -> Int
dwarfStringToTagEncoding string =
  fromJust(Map.lookup string dwarfStringToTagEncodingMap)

dwarfAttributeNames =
  [(0x1,"DW_AT_sibling"),
   (0x2,"DW_AT_location"),
   (0x3,"DW_AT_name"),
   (0x9,"DW_AT_ordering"),
   (0xb,"DW_AT_byte_size"),
   (0xc,"DW_AT_bit_offset"),
   (0xd,"DW_AT_bit_size"),
   (0x10,"DW_AT_stmt_list"),
   (0x11,"DW_AT_low_pc"),
   (0x12,"DW_AT_high_pc"),
   (0x13,"DW_AT_language"),
   (0x15,"DW_AT_discr"),
   (0x16,"DW_AT_discr_value"),
   (0x17,"DW_AT_visibility"),
   (0x18,"DW_AT_import"),
   (0x19,"DW_AT_string_length"),
   (0x1a,"DW_AT_common_reference"),
   (0x1b,"DW_AT_comp_dir"),
   (0x1c,"DW_AT_const_value"),
   (0x1d,"DW_AT_containing_type"),
   (0x1e,"DW_AT_default_value"),
   (0x20,"DW_AT_inline"),
   (0x21,"DW_AT_is_optional"),
   (0x22,"DW_AT_lower_bound"),
   (0x25,"DW_AT_producer"),
   (0x27,"DW_AT_prototyped"),
   (0x2a,"DW_AT_return_addr"),
   (0x2c,"DW_AT_start_scope"),
   (0x2e,"DW_AT_bit_stride"),
   (0x2f,"DW_AT_upper_bound"),
   (0x31,"DW_AT_abstract_origin"),
   (0x32,"DW_AT_accesibility"),
   (0x33,"DW_AT_address_class"),
   (0x34,"DW_AT_artificial"),
   (0x35,"DW_AT_base_types"),
   (0x36,"DW_AT_calling_convention"),
   (0x37,"DW_AT_count"),
   (0x38,"DW_AT_data_member_location"),
   (0x39,"DW_AT_decl_column"),
   (0x3a,"DW_AT_decl_file"),
   (0x3b,"DW_AT_decl_line"),
   (0x3c,"DW_AT_declaration"),
   (0x3d,"DW_AT_discr_list"),
   (0x3e,"DW_AT_encoding"),
   (0x3f,"DW_AT_external"),
   (0x40,"DW_AT_frame_base"),
   (0x41,"DW_AT_friend"),
   (0x42,"DW_AT_identifier_case"),
   (0x43,"DW_AT_macro_info"),
   (0x44,"DW_AT_namelist_item"),
   (0x45,"DW_AT_priority"),
   (0x46,"DW_AT_segment"),
   (0x47,"DW_AT_specification"),
   (0x48,"DW_AT_static_link"),
   (0x49,"DW_AT_type"),
   (0x4a,"DW_AT_use_location"),
   (0x4b,"DW_AT_variable_parameter"),
   (0x4c,"DW_AT_virtuality"),
   (0x4d,"DW_AT_vtable_elem_location"),
   (0x4e,"DW_AT_allocated"),
   (0x4f,"DW_AT_associated"),
   (0x50,"DW_AT_data_location"),
   (0x51,"DW_AT_byte_stride"),
   (0x52,"DW_AT_entry_pc"),
   (0x53,"DW_AT_use_UTF8"),
   (0x54,"DW_AT_extension"),
   (0x55,"DW_AT_ranges"),
   (0x56,"DW_AT_trampoline"),
   (0x57,"DW_AT_call_column"),
   (0x58,"DW_AT_call_file"),
   (0x59,"DW_AT_call_line"),
   (0x5a,"DW_AT_description"),
   (0x5b,"DW_AT_binary_scale"),
   (0x5c,"DW_AT_decimal_scale"),
   (0x5d,"DW_AT_small"),
   (0x5e,"DW_AT_decimal_sign"),
   (0x5f,"DW_AT_digit_count"),
   (0x60,"DW_AT_picture_string"),
   (0x61,"DW_AT_mutable"),
   (0x62,"DW_AT_threads_scaled"),
   (0x63,"DW_AT_explicit"),
   (0x64,"DW_AT_object_pointer"),
   (0x65,"DW_AT_endianity"),
   (0x66,"DW_AT_elemental"),
   (0x67,"DW_AT_pure"),
   (0x68,"DW_AT_recursive"),
   (0x69,"DW_AT_signature"),
   (0x6a,"DW_AT_main_subprogram"),
   (0x6b,"DW_AT_data_bit_offset"),
   (0x6c,"DW_AT_const_expr"),
   (0x6d,"DW_AT_enum_class"),
   (0x6e,"DW_AT_linkage_name"),
   (0x2000,"DW_AT_lo_user"),
   (0x3fff,"DW_AT_hi_user")]
    
dwarfAttributeNameEncodingToStringMap =
  Map.fromList (map (\t -> (fst t :: Int,snd t)) dwarfAttributeNames)

dwarfStringToAttributeNameEncodingMap =
  Map.fromList (map (\t -> (snd t,fst t :: Int)) dwarfAttributeNames) 

dwarfAttributeNameEncodingToString :: Int -> [Char]
dwarfAttributeNameEncodingToString encoding =
  fromJust (Map.lookup encoding dwarfAttributeNameEncodingToStringMap)
  
dwarfStringToAttributeNameEncoding :: [Char] -> Int
dwarfStringToAttributeNameEncoding string =
  fromJust(Map.lookup string dwarfStringToAttributeNameEncodingMap)

dwarfAttributeForms =
  [(0x1,"DW_FORM_addr"),
   (0x3,"DW_FORM_block2"),
   (0x4,"DW_FORM_block4"), 
   (0x5,"DW_FORM_data2"),
   (0x6,"DW_FORM_data4"),
   (0x7,"DW_FORM_data8"),
   (0x8,"DW_FORM_string"), 
   (0x9,"DW_FORM_block"),
   (0xa,"DW_FORM_block1"), 
   (0xb,"DW_FORM_data1"),
   (0xc,"DW_FORM_flag"),
   (0xd,"DW_FORM_sdata"),
   (0xe,"DW_FORM_strp"),
   (0xf,"DW_FORM_udata"), 
   (0x10,"DW_FORM_ref_addr"), 
   (0x11,"DW_FORM_ref1"),
   (0x12,"DW_FORM_ref2"), 
   (0x13,"DW_FORM_ref4"), 
   (0x14,"DW_FORM_ref8"), 
   (0x15,"DW_FORM_ref_udata"), 
   (0x16,"DW_FORM_indirect"), 
   (0x17,"DW_FORM_sec_offset"), 
   (0x18,"DW_FORM_exprloc"), 
   (0x19,"DW_FORM_flag_present"), 
   (0x20,"DW_FORM_ref_sig8")]
     
dwarfAttributeFormEncodingToStringMap =
  Map.fromList (map (\t -> (fst t :: Int,snd t)) dwarfAttributeForms)

dwarfStringToAttributeFormEncodingMap =
  Map.fromList (map (\t -> (snd t,fst t :: Int)) dwarfAttributeForms)

dwarfAttributeFormEncodingToString :: Int -> [Char]
dwarfAttributeFormEncodingToString encoding =
  fromJust (Map.lookup encoding dwarfAttributeFormEncodingToStringMap)
  
dwarfStringToAttributeFormEncoding :: [Char] -> Int
dwarfStringToAttributeFormEncoding string =
  fromJust(Map.lookup string dwarfStringToAttributeFormEncodingMap)


dwarfTag = dwarfStringToTagEncoding
dwarfName = dwarfStringToAttributeNameEncoding
dwarfForm = dwarfStringToAttributeFormEncoding
dwarfHasChildren = True
dwarfNoChildren = False


        
