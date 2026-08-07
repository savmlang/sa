import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const EDITIONS = ["nano", "standard", "pro", "pro_standard"];

const generateC1C2C3 = (ed) => {
  const remains = EDITIONS.filter((e) => e != ed);

  return {
    "{%C1}": remains[0],
    "{%C2}": remains[1],
    "{%C3}": remains[2],
    "{%KEYGPG}": join(import.meta.dirname, "key.gpg"),
    "{%KEYPEM}": join(import.meta.dirname, "key.pem"),
  };
};

const getVersion = () => {
  // We're not being a regexp parser, but this is perfect enough
  const regexp = /metadata\.savm\.version = "v?([0-9]+\.[0-9]+\.[0-9]+(.*)?)"/;

  let v;
  readFileSync(join(import.meta.dirname, "../../../Cargo.toml"))
    .toString()
    .split("\n")
    .find((txt) => {
      const output = regexp.exec(txt);

      if (output) {
        v = output[1];
        return true;
      }
    });

  if (!v) {
    throw new Error("Version could not be found.");
  }

  return v;
};

const getReleaseSupport = () => {
  let support = false;

  try {
    support =
      typeof process.env["GPG_KEY_ID"] == "string" &&
      process.env["GPG_KEY_ID"] != "" &&
      existsSync(join(import.meta.dirname, "key.gpg"));
  } catch (err) {}

  if (!support) {
    console.log("[INFO]: NOT USING SIGNING SUPPORT");
  }

  return support;
};

const main = () => {
  let recipe = readFileSync("./nfpm.recipe.yaml").toString();

  const edition = process.env["EDITION"];

  if (!EDITIONS.includes(edition)) {
    throw new Error(`Unknown edition for recipe : ${edition}`);
  }

  const recipeMap = {
    "{%edition}": edition,
    "{%version}": getVersion(),
    ...generateC1C2C3(edition),
  };

  Object.entries(recipeMap).forEach(([k, v]) => {
    recipe = recipe.replaceAll(k, v || "");
  });

  const supportsRelease = getReleaseSupport();

  const newrecipe = [];

  let release = false;
  recipe.split("\n").forEach((line) => {
    if (line.trim() == "!#RELEASE") {
      release = true;
      return;
    }

    if (!release) {
      newrecipe.push(line);
    } else if (supportsRelease) {
      newrecipe.push(line);
    }
  });

  writeFileSync("./nfpm.yaml", newrecipe.join("\n"));
};

main();
