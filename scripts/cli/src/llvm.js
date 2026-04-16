// @ts-check

import * as tar from "tar";
import { log, progress, select, spinner } from "@clack/prompts";
import { mkdir, rm, rmdir } from "node:fs/promises";
import { cwd } from "node:process";
import { createWriteStream } from "node:fs";
import { PassThrough, Readable } from "node:stream";

/**
 *
 * @param {{command: string;flags: Set<string>;}} argv0
 */
export default async function handlellvm(argv0) {
  const spin = spinner({
    indicator: "timer",
    cancelMessage: "Resolving LLVM was cancelled",
  });

  spin.start();
  spin.message("Resolving LLVM from releases");

  /**
   * @type {{ assets: { name: string, browser_download_url: string, size: number }[] }}
   */
  const jsonout = await fetch(
    "https://api.github.com/repos/savmlang/llvm/releases/latest",
    {
      headers: {
        "user-agent": "SaCLI",
      },
    },
  ).then((d) => d.json());

  spin.stop("LLVM has been resolved!");

  /**
   * @type {string | undefined}
   */
  let llvmbuild = undefined;

  argv0.flags.forEach((val) => {
    if (val.startsWith("--llvm-build=")) {
      if (llvmbuild) {
        log.warn("Found multiple `--llvm-build`. Overriding to latest one");
      }

      llvmbuild = val.replace("--llvm-build=", "");
    } else {
      log.warn(`Ignoring unknown argument ${val}`);
    }
  });

  let target = jsonout.assets.find((asset) => asset.name == llvmbuild);

  // Load the target to download
  if (!target) {
    if (llvmbuild) {
      log.warn(`Unable to find LLVM Build ${llvmbuild}`);
    }

    /**
     * @type {any}
     */
    const selection = await select({
      message: "Select your LLVM Build",
      options: jsonout.assets.map((asset) => ({
        value: asset,
        label: `${asset.name} (${(asset.size / (1024 * 1024)).toFixed(3)} MB)`,
      })),
      withGuide: true,
    });

    target = selection;
  }

  const progressbar = progress({
    style: "heavy",
    indicator: "dots",
    max: target?.size || 100,
  });
  progressbar.start();
  progressbar.message("Downloading...");

  // @ts-expect-error
  const bytes = await fetch(target.browser_download_url).then((d) =>
    // @ts-expect-error
    Readable.fromWeb(d.body),
  );

  const progressTracker = new PassThrough();
  progressTracker.on("data", (chunk) => {
    progressbar.advance(chunk.length);
  });
  progressTracker.on("close", () => {
    progressbar.stop("Downloaded");
  });

  const chdir = cwd();
  // Clears the LLVM directory
  await rm(`${chdir}/llvm`, { recursive: true, force: true }).catch(() => {});
  await mkdir(`${chdir}/llvm/bin`, { recursive: true }).catch(() => {});

  const llvmtar = `${chdir}/llvm/llvm.tar.gz`;

  await new Promise((resolve, reject) => {
    const writer = createWriteStream(llvmtar);

    bytes.pipe(progressTracker).pipe(writer);

    writer.on("finish", () => {
      progressbar.stop("Downloaded");
      resolve(true);
    });

    writer.on("error", reject);
    bytes.on("error", reject);
  });

  const extractspin = spinner();
  extractspin.start("Extracting...");
  await tar.extract({
    cwd: `${chdir}/llvm/bin`,
    keepExisting: false,
    file: llvmtar,
  });
  extractspin.stop("Extracted!");

  log.success("LLVM has been downloaded and successfully extracted!");
}
