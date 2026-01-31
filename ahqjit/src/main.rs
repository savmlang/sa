use std::mem::transmute;

use cranelift::{
  codegen::{
    Context, FinalizedMachReloc, FinalizedRelocTarget,
    ir::{Function, UserExternalName, UserExternalNameRef, UserFuncName},
  },
  jit::{JITBuilder, JITModule},
  module::{FuncId, Linkage, Module, ModuleReloc, ModuleRelocTarget, default_libcall_names},
  prelude::{
    isa::lookup,
    settings::Flags,
    types::{I64, I128},
    *,
  },
};
use target_lexicon::Triple;

fn main() {
  let mut builder = settings::builder();

  builder.set("enable_llvm_abi_extensions", "true").unwrap();

  let isa = lookup(Triple::host())
    .expect("Unsupported target isa")
    .finish(Flags::new(builder))
    .expect("Could not build ISA");

  let mut sig = Signature::new(isa.default_call_conv());

  sig.params.push(AbiParam::new(I64));
  sig.params.push(AbiParam::new(I64));
  sig.returns.push(AbiParam::new(I128));

  // IR Building
  let mut func =
    Function::with_name_signature(UserFuncName::User(UserExternalName::new(0, 0)), sig.clone());
  let mut func_ctx = FunctionBuilderContext::new();

  {
    let mut builder = FunctionBuilder::new(&mut func, &mut func_ctx);

    let mut external_sig = Signature::new(isa.default_call_conv());
    external_sig.params.push(AbiParam::new(types::I128));
    // external_sig.returns.push(AbiParam::new(types::I64)); // Pointer as i64

    let si = builder.import_signature(external_sig);

    let user_name = UserExternalName::new(1, 0);
    let name_ref = builder.func.declare_imported_user_function(user_name);
    let fnref = builder.import_function(ExtFuncData {
      patchable: false,
      name: ExternalName::User(name_ref),
      signature: si,
      colocated: false,
    });

    let block = builder.create_block();
    builder.switch_to_block(block);
    builder.append_block_params_for_function_params(block);

    let arg0 = builder.block_params(block)[0];
    let arg1 = builder.block_params(block)[1];

    // let result = builder.ins().sadd(arg0, const_val);
    let out = builder.ins().iconcat(arg0, arg1);
    builder.ins().call(fnref, &[out]);

    builder.ins().return_(&[out]);
    builder.seal_all_blocks();
    builder.finalize();
  }

  println!("{func:?}");

  let mut ctx = Context::for_function(func);

  let code = ctx
    .compile(isa.as_ref(), &mut Default::default())
    .expect("Compilation Failed");

  let machinecode = code.code_buffer();
  let relocs = code.buffer.relocs();

  println!("{machinecode:?}");
  println!("\nRELOCS:\n{relocs:?}");

  jit(sig, machinecode, relocs);
}

fn jit(sig: Signature, code: &[u8], relocs: &[FinalizedMachReloc]) {}

// fn jit(sig: Signature, code: &[u8], relocs: &[FinalizedMachReloc]) {
//   let mut builder = JITBuilder::new(default_libcall_names()).unwrap();

//   // 1. TELL THE LINKER WHERE THE RUST FUNCTION IS
//   builder.symbol("my_external_function", my_external_function as *const u8);

//   let mut module = JITModule::new(builder);

//   let mut ctx = module.make_context();
//   let func_id = module
//     .declare_function("my_func", Linkage::Export, &sig)
//     .unwrap();

//   // 3. DECLARE THE EXTERNAL FUNCTION (IMPORT)
//   // We need a signature for the external function to declare it.
//   let mut ext_sig = Signature::new(module.target_config().default_call_conv);
//   ext_sig.params.push(AbiParam::new(I128)); // Matches my_external_function(val: u128)

//   let user_index = ctx
//     .func
//     .declare_imported_user_function(UserExternalName::new(1, 0));

//   let sig_ref = ctx.func.import_signature(ext_sig.clone());

//   ctx.func.import_function(ExtFuncData {
//     name: ExternalName::User(user_index),
//     signature: sig_ref,
//     colocated: false,
//     patchable: true,
//   });

//   let mut alloc = vec![];
//   for reloc in relocs {
//     println!("{:?}", reloc.target);
//     match reloc.target {
//       FinalizedRelocTarget::ExternalName(ExternalName::User(_)) => {
//         alloc.push(ModuleReloc {
//           offset: reloc.offset,
//           kind: reloc.kind,
//           addend: reloc.addend,
//           name: ModuleRelocTarget::FunctionOffset(ext_func_id),
//         });
//       }
//       _ => {
//         alloc.push(ModuleReloc::from_mach_reloc(reloc, &ctx.func, func_id));
//       }
//     }

//     let rel = reloc.clone();

//     let reloc_ = ModuleReloc::from_mach_reloc(reloc, &ctx.func, func_id);

//     alloc.push(reloc_);
//   }

//   module
//     .define_function_bytes(func_id, 16, code, &alloc)
//     .unwrap();

//   module.finalize_definitions().unwrap();

//   let id: extern "C" fn(a: i64, b: i64) -> i128 =
//     unsafe { transmute(module.get_finalized_function(func_id)) };

//   println!("{}", id(10, 20));
// }

extern "C" fn my_external_function(val: u128) {
  println!("Called with: {}", val);
}
