// @ts-check

import { intro, log, outro, select } from "@clack/prompts";
import handlellvm from "./llvm.js";

(async () => {
  intro(`SaCLI (NodeJS ${process.version})`, {
    withGuide: true,
  });

  const argv = await parse();

  switch (argv.command) {
    case "llvm":
      await handlellvm(argv);
      break;

    default:
      log.error(`Unknown command : ${argv.command}`);
      process.exit(1);
  }

  outro();
})();

async function parse() {
  /**
   * @type {string | undefined}
   */
  let command;
  /**
   * @type {Set<string>}
   */
  const flags = new Set();

  const args = process.argv.slice(2);

  args.forEach((arg) => {
    if (arg.startsWith("--")) {
      flags.add(arg);
      return;
    }

    if (command != undefined) {
      log.error("Multiple commands are not supported");

      process.exit(1);
      return;
    }

    command = arg;
  });

  if (command == undefined) {
    command = (
      await select({
        options: [
          {
            value: "llvm",
            label: "Download LLVM (sacli llvm)",
          },
        ],
        message: "Please select your option",
      })
    ).toString();
  }

  return {
    command,
    flags,
  };
}
