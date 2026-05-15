# Security Policy

## Vulnerabilities

A vulnerability is strictly defined as a failure of the VM or Runtime when executing "defined" assembly. This includes:
1. **Specification Deviation:** Where any VM runtime deviates from the intended specification in any emulation tier.
2. **Execution Disparity:** Where VM showcases disparity in outcome between JIT compiler and Interpreter.
3. **VM Failure:** Where VM enters a state of Undefined Control-Flow or Instruction Sequences or aborts or crashes or panics during execution of defined assembly sequences.

## Exclusions

The following is a non-exhaustive list of exclusions:
1. **Well Known Floating Point Precision Issues:** Floating Point precision issues is expected, however, if you believe it is a major deviation from expected output. Consider it as vulnerability.
2. **Performance Differences:** Differences in performance is not considered vulnerabilities and instead may be an issue, at best.

## "defined" vs "undefined"

- "defined" behaviour is strictly defined as assemebly verified to have an reproducible outcome for the exact context.
- "undefined" behaviour is loosely defined as anything that it not "defined"

## Version Support

**"EVERY VERSION IS A STANDALONE RELEASE WITHOUT BACKPORTS"**

1. Each version (a.b.c) or each version group (a.b.x) or major version cluster (a.x.x) is not guaranteed to be supported with security backports.
2. Any security fixes shall go only to the latest version tree being worked on.
3. The above MAY be overridden on Maintainer's discretion only in exceptional cases.

## Reporting a Vulnerability

> Note: We move fast. If you discover a bug in an older version, please verify it still exists in the latest release before proceeding to report it.

### For the LATEST Version:
**Please do not report security vulnerabilities through public GitHub issues**. Report them privately and securely via the link below.

### For OLDER versions
Kindly calculate CVSS score.
1. If it is above 9.0, consider creating a private vulnerability.
2. If it is above 5.0, create a public issue and potentially wait for a volunteer to create a PR.
3. If it is under or equal to 5.0, consider creating an issue and mentally prepare to create a PR.

## Vulnerability Reporting
Kindly head over to **https://github.com/savmlang/sa/security/advisories/new**
