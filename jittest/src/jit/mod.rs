use cranelift::{
  codegen::{self, ir::Endianness},
  jit::JITModule,
  module::{Linkage, Module},
  prelude::{isa::CallConv, *},
};
use std::os::raw::c_void; // Added for clarity on the pointer type

pub struct JIT {
  pub fnctx: FunctionBuilderContext,
  pub ctx: codegen::Context,
  pub module: JITModule,
}

impl JIT {
  // Note: 'new_dummy' should probably take 'self' by mutable reference,
  // as it is modifying 'self.ctx' and 'self.fnctx' within a new context.
  // Also, the JIT struct would need proper initialization logic in a real-world scenario.
  pub fn new_dummy(&mut self) -> Result<*const c_void, String> {
    // Changed to return the compiled function pointer
    // 1. Determine the pointer type for the target architecture
    let pt = self.module.target_config().pointer_type();

    // 2. Clear the context and set up the function signature
    // The signature will be: fn(register_ctx: pointer) -> pointer
    self.ctx.clear(); // Clear context before setting up a new function

    // Input parameter: register ctx as a pointer
    let mut sig = self.module.make_signature();

    #[cfg(unix)]
    {
      sig.call_conv = CallConv::SystemV;
    }

    #[cfg(windows)]
    {
      sig.call_conv = CallConv::WindowsFastcall;
    }

    sig.params.push(AbiParam::new(pt));

    // Output parameter: register ctx out as a pointer
    sig.returns.push(AbiParam::new(pt));

    self.ctx.func.signature = sig;

    // 3. Build the function body
    let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fnctx);

    // Start a new basic block
    let code_block = builder.create_block();

    // Set the function's entry block
    builder.switch_to_block(code_block);

    // Get the parameters passed to the function in the entry block
    // The first (and only) parameter is the input pointer
    builder.append_block_params_for_function_params(code_block);

    let input_ptr_value = builder.block_params(code_block)[0];

    let vout = builder
      .ins()
      .load(pt, MemFlags::trusted(), input_ptr_value, 0);

    // let input_ptr_value = builder.ins().iconst(pt, 100);

    // 4. Insert the instruction to return the input pointer
    // In Cranelift, the 'return' instruction takes the values to return.
    builder.ins().return_(&[vout]);

    // 5. Seal the block and finalize the function
    builder.seal_block(code_block);
    builder.finalize();

    println!("{}", self.ctx.func);

    // 6. Compile the function and get the pointer

    // Declare the function in the module (using a dummy name)
    let func_name = self
      .module
      .declare_anonymous_function(&self.ctx.func.signature)
      .map_err(|e| format!("Failed to declare function: {}", e))?;

    // Define (compile) the function
    self
      .module
      .define_function(func_name, &mut self.ctx)
      .map_err(|e| format!("Failed to define function: {}", e))?;

    // Finalize the module (link and make callable)
    self
      .module
      .finalize_definitions()
      .map_err(|e| format!("Failed to finalize definitions: {}", e))?;

    // Get the compiled function's address
    let code_ptr = self.module.get_finalized_function(func_name);

    Ok(code_ptr as *const c_void) // Return the raw pointer to the compiled function
  }
}
