"use strict";

const required = [22, 14, 0];
const actual = process.versions.node.split(".").map(Number);

const isSupported = required.every((part, index) => {
  const current = actual[index] ?? 0;

  if (current > part) {
    return true;
  }

  if (current < part) {
    return false;
  }

  return true;
}) && actual.some((part, index) => part > (required[index] ?? 0)) ||
  actual.every((part, index) => part === required[index]);

if (!isSupported) {
  console.error(
    [
      "purescript-payload requires Node.js 22.14.0 or newer.",
      `Current version: ${process.versions.node}`,
      "Run `nvm use` in this repo and retry."
    ].join("\n")
  );
  process.exit(1);
}
