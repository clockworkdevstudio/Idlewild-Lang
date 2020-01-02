module DWARF where

import Data.Int
import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.Map as Map

data DWARFAttributeSpec =
  DWARFAttributeSpec
  {
    specName :: Int,
    specForm :: Int
  }
  deriving (Show)


data DWARFDIE =
  DWARFDIE
  {
    dwarfDIEKey :: [Char],
    dwarfDIEAbbreviationCode :: Int,
    dwarfDIEAttributes :: [DWARFAttribute],
    dwarfDIEAttributesSize :: Int,
    dwarfDIEOffset :: Int
  }
  deriving (Show)

data DWARFAttribute =
  DWARFAttributeAddr
  {
    attributeAddr :: Int64
  } |
  DWARFAttributeBlock1
  {
    attributeBlock1Length :: Word8,
    attributeBlock1Data :: [Word8]
  } |
  DWARFAttributeBlock2
  {
    attributeBlock2Length :: Word16,
    attributeBlock2Data :: [Word8]
  } |
  DWARFAttributeBlock4
  {
    attributeBlock4Length :: Word32,
    attributeBlock4Data :: [Word8]
  } |
  DWARFAttributeBlock8
  {
    attributeBlock8Length :: Word64,
    attributeBlock8Data :: [Word8]
  } |
  DWARFAttributeBlock
  {
    attributeBlockLength :: DWARFUnsignedLEB128,
    attributeBlockData :: [Word8]
  } |
  DWARFAttributeData1UnsignedInt
  {
    attributeData1UInt :: Word8
  } |
  DWARFAttributeData1SignedInt
  {
    attributeData1Int :: Int8
  } |
  DWARFAttributeData2UnsignedInt
  {
    attributeData2UInt :: Word16
  } |
  DWARFAttributeData2SignedInt
  {
    attributeData2Int :: Int16
  } |
  DWARFAttributeData4UnsignedInt
  {
    attributeData4UInt :: Word32
  } |
  DWARFAttributeData4SignedInt
  {
    attributeData4Int :: Int32
  } |
  DWARFAttributeData4Float
  {
    attributeData4Float :: Float
  } |
  DWARFAttributeData8UnsignedInt
  {
    attributeData8UInt :: Word64
  } |
  DWARFAttributeData8SignedInt
  {
    attributeData8Int :: Int64
  } |
  DWARFAttributeData8Double
  {
    attributeData8Double :: Double
  } |
  DWARFAttributeDataUnsignedLEB128
  {
    attributeDataULEB128 :: DWARFUnsignedLEB128
  } |
  DWARFAttributeDataSignedLEB128
  {
    attributeDataSLEB128 :: DWARFSignedLEB128
  } |
  DWARFAttributeExprLoc
  {
  
  } |
  DWARFAtttributeFlag
  {
    attributeFlag :: Bool
  } |
  DWARFAttributeLinePtr
  {
    attributeLinePtr :: Int32
  } |
  DWARFAttributeLocListPtr
  {
    attributeLocListPtr :: Int32
  } |
  DWARFAttributeMacPtr
  {
    attributeMacPtr :: Int32
  } |
  DWARFAttributeRangeListPtr
  {
    attributeRangeListPtr :: Int32
  } |
  DWARFAttributeRef1
  {
    attributeRef1 :: Word8
  } |
  DWARFAttributeRef2
  {
    attributeRef2 :: Word16
  } |
  DWARFAttributeRef4
  {
    attributeRef4 :: Word32
  } |
  DWARFAttributeRef8
  {
    attributeRef8 :: Word64
  } |
  DWARFAttributeRefUData
  {
    attributeRefUData :: DWARFUnsignedLEB128
  } |
  DWARFAttributeRefAddr
  {
    attributeRefAddr :: Word32
  } |
  DWARFAttributeRefSig8
  {
    attributeRefSig8 :: Word64
  } |
  DWARFAttributeRefString
  {
    attributeRefString :: [Char]
  } |
  DWARFAttributeRefStrp
  {
    attributeRefStrp :: Word32
  } |
  DWARFDIECrossReference
  {
    crossReferenceOffset :: Int
  }
  deriving (Show)

dwarfAttributeSize :: DWARFAttribute -> Int
dwarfAttributeSize (DWARFAttributeRefStrp _) = 4 
dwarfAttributeSize (DWARFAttributeAddr _) = 8
dwarfAttributeSize (DWARFAttributeBlock1 s d) = 1 + (fromIntegral s) * length d
dwarfAttributeSize (DWARFAttributeBlock2 s d) = 2 + (fromIntegral s) * length d
dwarfAttributeSize (DWARFAttributeBlock4 s d) = 4 + (fromIntegral s) * length d
dwarfAttributeSize (DWARFAttributeBlock8 s d) = 8 + (fromIntegral s) * length d
dwarfAttributeSize (DWARFAttributeBlock s d) = length (uLEB128 s) + decodeUnsignedLEB128 s * length d
dwarfAttributeSize (DWARFAttributeData1UnsignedInt _) = 1
dwarfAttributeSize (DWARFAttributeData1SignedInt _) = 1
dwarfAttributeSize (DWARFAttributeData2UnsignedInt _) = 2
dwarfAttributeSize (DWARFAttributeData2SignedInt _) = 2
dwarfAttributeSize (DWARFAttributeData4UnsignedInt _) = 4
dwarfAttributeSize (DWARFAttributeData4SignedInt _) = 4
dwarfAttributeSize (DWARFAttributeData4Float _) = 4
dwarfAttributeSize (DWARFAttributeData8UnsignedInt _) = 8
dwarfAttributeSize (DWARFAttributeData8SignedInt _) = 8
dwarfAttributeSize (DWARFAttributeData8Double _) = 8
dwarfAttributeSize (DWARFAttributeDataUnsignedLEB128 d) = length (uLEB128 d)
--dwarfAttributeSize (DWARFAttributeDataSignedLEB128 _) = 
--dwarfAttributeSize (DWARFAttributeExprLoc) = 0 
dwarfAttributeSize (DWARFAtttributeFlag _) = 1
dwarfAttributeSize (DWARFAttributeLinePtr _) = 4
dwarfAttributeSize (DWARFAttributeLocListPtr _) = 4
dwarfAttributeSize (DWARFAttributeMacPtr _) = 4
dwarfAttributeSize (DWARFAttributeRangeListPtr _) = 4
dwarfAttributeSize (DWARFAttributeRef1 _) = 1
dwarfAttributeSize (DWARFAttributeRef2 _) = 2
dwarfAttributeSize (DWARFAttributeRef4 _) = 4
dwarfAttributeSize (DWARFAttributeRef8 _) = 8
dwarfAttributeSize (DWARFAttributeRefUData d) = length (uLEB128 d)
dwarfAttributeSize (DWARFAttributeRefAddr _) = 4
dwarfAttributeSize (DWARFAttributeRefSig8 _) = 8
dwarfAttributeSize (DWARFAttributeRefString s) = length s + 1
dwarfAttributeSize (DWARFAttributeRefStrp _) = 4
dwarfAttributeSize (DWARFDIECrossReference _) = 4
dwarfAttributeSize k = error (show k)

dwarfCreateDIE :: [Char] -> Int -> [DWARFAttribute] -> (Int -> DWARFDIE)
dwarfCreateDIE key abbrevCode attributes =
  DWARFDIE
    key abbrevCode attributes (foldr (\da a -> dwarfAttributeSize da + a) 0 attributes)

dwarfDIECalculateSize :: DWARFDIE -> Int
dwarfDIECalculateSize die =
  length (uLEB128 (encodeUnsignedLEB128 (dwarfDIEAbbreviationCode die))) + (foldr (\da a -> dwarfAttributeSize da + a) 0 (dwarfDIEAttributes die))
  
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

data DWARFUnsignedLEB128 =
  DWARFUnsignedLEB128
  {
    uLEB128 :: [Word8]
  }
  deriving (Show)

data DWARFSignedLEB128 =
  DWARFSignedLEB128
  {
    sLEB128 :: [Word8]
  }
  deriving (Show)

dwarfUnsignedLEB128Cat :: DWARFUnsignedLEB128 -> DWARFUnsignedLEB128 -> DWARFUnsignedLEB128
dwarfUnsignedLEB128Cat (DWARFUnsignedLEB128 l) (DWARFUnsignedLEB128 r) =
  DWARFUnsignedLEB128 (l ++ r)

dwarfSignedLEB128Cat :: DWARFSignedLEB128 -> DWARFSignedLEB128 -> DWARFSignedLEB128
dwarfSignedLEB128Cat  (DWARFSignedLEB128 l) (DWARFSignedLEB128 r)=
  DWARFSignedLEB128 (l ++ r)
  
encodeUnsignedLEB128 :: Int -> DWARFUnsignedLEB128
encodeUnsignedLEB128 integer =

  if recurse integer
  then (DWARFUnsignedLEB128 [(fromIntegral ((extractLowOrder7Bits integer) .|. 0x80))]) `dwarfUnsignedLEB128Cat`
       encodeUnsignedLEB128 (shiftRight7Bits integer)
  else (DWARFUnsignedLEB128 [(fromIntegral (extractLowOrder7Bits integer))])
  
  where recurse i = shiftRight7Bits i > 0
        extractLowOrder7Bits i = i .&. 0x7F
        shiftRight7Bits i = shift i (-7)

decodeUnsignedLEB128 :: DWARFUnsignedLEB128 -> Int
decodeUnsignedLEB128 e =
  decode e 0 0
  
  where byte = fromIntegral (head (uLEB128 e))
        update n s = n .|. (shift (byte .&. 0x7f) s) 
        decode i accum s =
          if uLEB128 i == []
          then accum
          else decode (DWARFUnsignedLEB128 (tail (uLEB128 i))) (update accum s) (s + 7)
             
encodeSignedLEB128 :: Int -> DWARFSignedLEB128
encodeSignedLEB128 integer =
 
  if recurse integer
  then (DWARFSignedLEB128 [(fromIntegral ((extractLowOrder7Bits integer) .|. 0x80))]) `dwarfSignedLEB128Cat`
       encodeSignedLEB128 (shiftRight7Bits integer)
  else (DWARFSignedLEB128 [(fromIntegral (extractLowOrder7Bits integer))])
  
  where recurse i = not ((shiftRight7Bits i == 0 && (encodedByte i .&. 0x40 == 0)) || (shiftRight7Bits i == (-1) && (encodedByte i .&. 0x40 == 0x40)))
        encodedByte i = extractLowOrder7Bits i
        extractLowOrder7Bits i = i .&. 0x7F
        shiftRight7Bits i = shift i (-7)

uShiftR :: Int -> Int -> Int
uShiftR n k = fromIntegral (fromIntegral n `shiftR` k :: Word)

    
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

dwarfAttributeEncodings =
  [(0x1,"DW_ATE_address"),
   (0x2,"DW_ATE_boolean"),
   (0x3,"DW_ATE_complex_float"),
   (0x4,"DW_ATE_float"),
   (0x5,"DW_ATE_signed"),
   (0x6,"DW_ATE_signed_char"),
   (0x7,"DW_ATE_unsigned"),
   (0x8,"DW_ATE_unsigned_char"),
   (0x9,"DW_ATE_imaginary_float"),
   (0xa,"DW_ATE_packed_decimal"),
   (0xb,"DW_ATE_numeric_string"),
   (0xc,"DW_ATE_edited"),
   (0xd,"DW_ATE_signed_fixed"),
   (0xe,"DW_ATE_unsigned_fixed"),
   (0xf,"DW_ATE_decimal_float"),
   (0x10,"DW_ATE_UTF"),
   (0x80,"DW_ATE_lo_user"),
   (0xff,"DW_ATE_hi_user")]
   
dwarfAttributeEncodingToStringMap =
  Map.fromList (map (\t -> (fst t :: Int,snd t)) dwarfAttributeEncodings)

dwarfStringToAttributeEncodingMap =
  Map.fromList (map (\t -> (snd t,fst t :: Int)) dwarfAttributeEncodings)

dwarfAttributeEncodingToString :: Int -> [Char]
dwarfAttributeEncodingToString encoding =
  fromJust (Map.lookup encoding dwarfAttributeEncodingToStringMap)
  
dwarfStringToAttributeEncoding :: [Char] -> Int
dwarfStringToAttributeEncoding string =
  fromJust(Map.lookup string dwarfStringToAttributeEncodingMap)

dwarfTag = dwarfStringToTagEncoding
dwarfName = dwarfStringToAttributeNameEncoding
dwarfForm = dwarfStringToAttributeFormEncoding
dwarfHasChildren = True
dwarfNoChildren = False


        
