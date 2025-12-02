SECTIONS
{
/* The .vm_fast_instructions section must appear early in the program's code,
ideally just after the standard entry points, to ensure maximum locality.
The wildcard (*) ensures we capture all instruction handlers, regardless
of LLVM's internal naming conventions (e.g., .vm_fast_instructions.0, .vm_fast_instructions.1). /
.vm_instructions_fast :
{
(.vm_fast_instructions .vm_fast_instructions.)
. = ALIGN(0x10); / Optional: Ensure 16-byte alignment for cache lines */
}

/* Standard executable text section follows, ensuring other code is not interspersed */
.text :
{
(.text .text.)
}

/* ... other standard sections follow, e.g., .data, .rodata, etc. ... */
}