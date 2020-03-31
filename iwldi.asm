%define DBG_LINE_BASE (-5)
%define DBG_LINE_RANGE 14
%define DBG_OPCODE_BASE 0xd
%define DBG_LNS_ADVANCE_LINE 0x3
%define DBG_LNS_ADVANCE_PC 0x2
%define DBG_LNS_COPY 0x1

%macro DBG_ADVANCE 2
  %assign OPCODE ((%1 - DBG_LINE_BASE) + (DBG_LINE_RANGE * %2) + DBG_OPCODE_BASE)
  %if OPCODE <= 255 && %1 <= DBG_LINE_BASE + DBG_LINE_RANGE - 1
    db OPCODE
  %else
    %if %1 != 0
      db DBG_LNS_ADVANCE_LINE
      encodeSignedLEB128 %1
      db DBG_LNS_ADVANCE_PC
      encodeUnsignedLEB128 %2
      db DBG_LNS_COPY
    %else
      db DBG_LNS_ADVANCE_PC
      encodeUnsignedLEB128 %2
      db DBG_LNS_COPY
    %endif
  %endif
  %undef OPCODE
%endmacro

%macro encodeUnsignedLEB128 1
  %assign INTEGER %1
  %rep 16
    %if (INTEGER >> 7) > 0
      db (INTEGER & 0x7f) | 0x80
      %assign INTEGER (INTEGER >> 7)
    %else
      db (INTEGER & 0x7f)
      %exitrep
    %endif
  %endrep
%endmacro

%macro encodeSignedLEB128 1
  %assign VALUE %1
  %if VALUE < 0
    %assign NEGATIVE 1
  %else
    %assign NEGATIVE 0
  %endif
  %assign MORE 1
  %assign SIZE 64
  %assign BYTE 0
  %rep 16
    %if MORE == 0
      %exitrep
    %endif
    %assign BYTE (VALUE & 0x7f)
    %assign VALUE (VALUE >> 7)
    %if NEGATIVE
      %assign VALUE (VALUE | -(1 << (SIZE - 7)))
    %endif
    %if (VALUE == 0 && ((BYTE & 0x40) == 0)) || (VALUE == (-1) && ((BYTE & 0x40) == 0x40))
      %assign MORE 0
    %else
      %assign BYTE (BYTE | 0x80)
    %endif
    db BYTE
  %endrep
%endmacro

%macro DBG_CFA_ADVANCE_LOC 1
  %if %1 < 64
    db (1 << 6) | %1 ; advance location (6-bit delta)
  %elif %1 < 256
    db 2
    db %1 ; advance location (8-bit delta)
  %elif %1 < 65536
    db 3
    dw %1 ; advance location (16-bit delta)
  %else
    db 4
    dd %1 ; advance location (32-bit delta)
  %endif
%endmacro
