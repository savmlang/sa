# Security Policy

## Vulnerabilities

A vulnerability is strictly defined as a failure of the VM or Runtime when executing "defined" assembly. This includes:
1. **Specification Deviation:** Where any VM runtime deviates from the intended specification in any emulation tier.
2. **Execution Disparity:** Where VM showcases disparity in outcome between JIT compiler and Interpreter.
3. **VM Failure:** Where VM enters a state of Undefined Control-Flow or Instruction Sequences or aborts or crashes or panics during execution of defined assembly sequences.
4. **Nonreproducible Outputs:** If the VM or any executor of VM returns output that is non reproducible where assembly spec defines it to be reproducible.

## Exclusions

The following is a non-exhaustive list of exclusions:
1. **Well Known Floating Point Precision Issues:** Floating Point precision issues are expected, however, noncompliance to IEEE 754 is classified as vulnerability.
2. **Performance Differences:** Differences in performance are not considered vulnerabilities and instead may be an issue, at best.

## "defined" vs "undefined"

- "defined" behaviour is strictly defined as assembly that follows the specification to an output defined under the specification, that is defined assembly input that maps to defined assembly output as defined in the specification.
- "undefined" behaviour is loosely defined as anything that is not "defined"

## Version Support

We focus our development and security efforts on the latest active release tree to ensure the highest quality and velocity. 

1. **Latest Release Focus:** Security fixes are actively developed for and applied only to the current major/minor release tree. 
2. **Older Versions:** Legacy versions (including older patch, minor, or major releases) are generally not supported with backported security fixes.
3. **Exceptions:** The maintainers reserve the right to backport critical security fixes to older versions at their sole discretion in exceptional circumstances.

## Reporting a Vulnerability

> Note: Our project moves quickly. If you discover a security issue in an older version, please verify whether it can be reproduced in the latest stable release before submitting a report.

### For the LATEST Version:
**Please do not report security vulnerabilities through public GitHub issues**. If you find a vulnerability, head over to https://github.com/savmlang/sa/security/advisories/new for securely and privately reporting a vulnerability.

### For LEGACY versions
If a vulnerability only affects a *legacy* system. We request you to submit a private security advisory using the link below so we can evaluate if an exceptional backport is warranted.

Depending on Common Vulnerability Scoring System (CVSS) framework, we determine the appropriate disclosure and patching path
- Critical Severity (CVSS 8.0 – 10.0): An exceptional backport may be warranted. It may be assigned to a maintainer or to trusted volunteers.
- Low to Medium Severity (CVSS <8.0): A public advisory/issue may be created detailing the bug and appropriate remediation steps (eg, migration or backport) shall be made.

Kindly also note that for an extremely old legacy version, patches may be accepted from community contributions.

Also if you can, migrate to latest version since our VM is backwards compatible.

## Vulnerability Reporting
Kindly head over to **https://github.com/savmlang/sa/security/advisories/new** for private vulnerability reporting.
