use crate::acaot::{
  cinder::{CompilerMeta, Resolved, StencilMap, emit::Stencil, stencilify},
  pickle::{
    def::PickleInstruction,
    reader::{
      cast::{CAST, VFCAST, parse_cast, parse_vfcast},
      vbit::{VBIT, VROT, parse_vbit, parse_vrot},
      vcmp::{VCMP, parse_vcmp},
      vfop::{VDATAOP, VFOP, parse_vdataop, parse_vfop},
      vminimax::{VCNT, VMINIMAX, parse_vcnt, parse_vminimax},
      vsh::{VSH, parse_vsh},
    },
  },
};

pub fn emit_vcmp(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VCMP {
    datawdt,
    cmpop,
    count,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  } = parse_vcmp(pickle, ws);

  let dt_src1_src2_tgt_count = (datawdt as u64)
    | ((src1 as u64) << 8)
    | ((src2 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src1.cast_unsigned() as u64) | ((of_src2.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      (
        "INSTDEFINED",
        Resolved::Immediate {
          imm: cmpop.to_classical() as u64,
        },
      ),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_tgt.cast_unsigned() as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_vcnt(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VCNT {
    op,
    flags_src: src1,
    flags_target: tgt,
    count,
    of_src,
    of_target,
    typ,
    ..
  } = parse_vcnt(pickle, ws);

  let dt_src1_src2_tgt_count = (typ as u64)
    | ((src1 as u64) << 8)
    | ((src1 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src.cast_unsigned() as u64) | ((of_src.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      ("INSTDEFINED", Resolved::Immediate { imm: op as u64 }),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_target as i64 as u64,
        },
      ),
      ("__real@43e0000000000000", Resolved::WorkingSetId { idx: 1 }),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_vminimax(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VMINIMAX {
    op,
    flags_src1: src1,
    flags_src2: src2,
    flags_target: tgt,
    count,
    of_src1,
    of_src2,
    of_target,
    typ,
    ..
  } = parse_vminimax(pickle, ws);

  let dt_src1_src2_tgt_count = (typ as u64)
    | ((src1 as u64) << 8)
    | ((src2 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src1.cast_unsigned() as u64) | ((of_src2.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      ("INSTDEFINED", Resolved::Immediate { imm: op as u64 }),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_target as i64 as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_vbit(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VBIT {
    count,
    op,
    width,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt: of_target,
  } = parse_vbit(pickle, ws);

  let dt_src1_src2_tgt_count = (width as u64)
    | ((src1 as u64) << 8)
    | ((src2 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src1.cast_unsigned() as u64) | ((of_src2.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      ("INSTDEFINED", Resolved::Immediate { imm: op as u64 }),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_target as i64 as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_vdataop(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VDATAOP {
    datatype,
    count,
    src1,
    of_src1,
    tgt,
    of_tgt,
  } = parse_vdataop(pickle, ws);

  let dt_src1_src2_tgt_count = (datatype as u64)
    | ((src1 as u64) << 8)
    | ((src1 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src1 as u32 as u64) | ((of_src1 as u32 as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      ("INSTDEFINED", Resolved::Immediate { imm: 0 as u64 }),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_tgt as i64 as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_cast(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let CAST {
    offset_src: of_src,
    offset_target: of_tgt,
    src,
    target: tgt,
    type_initial,
    type_final,
  } = parse_cast(pickle, ws);

  let tags_src_tgt = (type_initial as u64)
    | ((type_final as u64) << 8)
    | ((src as u64) << 16)
    | ((tgt as u64) << 24);
  let of_src_tgt = (of_src.cast_unsigned() as u64) | ((of_tgt.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      ("TAGS_SRC_TGT", Resolved::Immediate { imm: tags_src_tgt }),
      ("OF_SRC_TGT", Resolved::Immediate { imm: of_src_tgt }),
      ("COUNT", Resolved::Immediate { imm: 1 }),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_vfcast(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VFCAST {
    offset_src: of_src,
    offset_target: of_tgt,
    count,
    src,
    target: tgt,
    type_initial,
    type_final,
  } = parse_vfcast(pickle, ws);

  let tags_src_tgt = (type_initial as u64)
    | ((type_final as u64) << 8)
    | ((src as u64) << 16)
    | ((tgt as u64) << 24);
  let of_src_tgt = (of_src.cast_unsigned() as u64) | ((of_tgt.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      ("TAGS_SRC_TGT", Resolved::Immediate { imm: tags_src_tgt }),
      ("OF_SRC_TGT", Resolved::Immediate { imm: of_src_tgt }),
      ("COUNT", Resolved::Immediate { imm: count as _ }),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_jif(
  op: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let intent = op.u1;
  let src = op.u2;
  let width = op.u3;

  let offset = i32::from_ne_bytes(unsafe { ws[0..4].try_into().unwrap_unchecked() });
  let marker = u64::from_ne_bytes(unsafe { ws[4..12].try_into().unwrap_unchecked() });

  let intent_src_width = (intent as u64) | ((src as u64) << 8) | ((width as u64) << 16);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "INTENT_SRC_WIDTH",
        Resolved::Immediate {
          imm: intent_src_width,
        },
      ),
      ("OFFSET", Resolved::Immediate { imm: offset as u64 }),
      ("TAKEN_JUMP", Resolved::ResolveLaterStencilID { marker }),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_vfop(
  op: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VFOP {
    src,
    target,
    subop,
    offset_src,
    offset_target,
    count,
    typetag,
  } = parse_vfop(op, ws);

  let dt_src1_src2_tgt_count = (typetag as u64)
    | ((src as u64) << 8)
    | ((src as u64) << 16)
    | ((target as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (offset_src as u32 as u64) | ((offset_src as u32 as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      ("INSTDEFINED", Resolved::Immediate { imm: subop as u64 }),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: offset_target as i64 as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_vrot(
  op: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VROT {
    count,
    op,
    typetag,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_vrot(op, ws);

  let dt_src1_src2_tgt_count = (typetag as u64)
    | ((src1 as u64) << 8)
    | ((src2 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src1 as u32 as u64) | ((of_src2 as u32 as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      ("INSTDEFINED", Resolved::Immediate { imm: op as u64 }),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_tgt as i64 as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

pub fn emit_vsh(
  op: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VSH {
    op,
    flags_src1: src1,
    flags_src2: src2,
    flags_target: tgt,
    count,
    of_src1,
    of_src2,
    of_target: of_tgt,
    typ: typetag,
  } = parse_vsh(op, ws);

  let dt_src1_src2_tgt_count = (typetag as u64)
    | ((src1 as u64) << 8)
    | ((src2 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src1 as u32 as u64) | ((of_src2 as u32 as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      ("INSTDEFINED", Resolved::Immediate { imm: op as u64 }),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_tgt as i64 as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}
