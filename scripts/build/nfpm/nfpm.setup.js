import { createPrivateKey } from "node:crypto";
import { decryptKey, readKey } from "openpgp";
import { join } from "node:path";
import { readFile, writeFile } from "node:fs/promises";

(async () => {
  const armor = await readFile("./key.gpg", { encoding: "utf8" });
  const passphrase = process.env["NFPM_PASSPHRASE"];

  // Get the private key
  const privateKey = await readKey({
    armoredKey: armor,
  });

  const keydata = await decryptKey({
    privateKey,
    passphrase,
  });

  const params = keydata.keyPacket.publicParams;
  const sparams = keydata.keyPacket.privateParams;

  const { n, e, d, p, q, dp } = { ...params, ...sparams };

  // 2. Convert to BigInts for CRT calculations
  const bnP = toBigInt(p);
  const bnQ = toBigInt(q);
  const bnD = toBigInt(d);

  // 3. Compute dp, dq, and qi
  const bnDp = bnD % (bnP - 1n);
  const bnDq = bnD % (bnQ - 1n);
  const bnQi = modInverse(bnQ, bnP);

  const key = createPrivateKey({
    key: {
      kty: "RSA",
      n: Buffer.from(n).toString("base64url"),
      e: Buffer.from(e).toString("base64url"),
      d: Buffer.from(d).toString("base64url"),
      p: Buffer.from(p).toString("base64url"),
      q: Buffer.from(q).toString("base64url"),

      dp: toBase64Url(bnDp),
      dq: toBase64Url(bnDq),
      qi: toBase64Url(bnQi),
    },
    format: "jwk",
  });

  const pem = key.export({
    format: "pem",
    type: "pkcs8",
  });

  await writeFile(join(import.meta.dirname, "key.pem"), pem);
})();

const toBigInt = (buf) => BigInt("0x" + Buffer.from(buf).toString("hex"));

const toBase64Url = (bn) => {
  let hex = bn.toString(16);
  if (hex.length % 2 !== 0) hex = "0" + hex; // Ensure even length
  return Buffer.from(hex, "hex").toString("base64url");
};

function modInverse(a, m) {
  let m0 = m,
    t,
    q;
  let x0 = 0n,
    x1 = 1n;
  if (m === 1n) return 0n;
  while (a > 1n) {
    q = a / m;
    t = m;
    m = a % m;
    a = t;
    t = x0;
    x0 = x1 - q * x0;
    x1 = t;
  }
  if (x1 < 0n) x1 += m0;
  return x1;
}
