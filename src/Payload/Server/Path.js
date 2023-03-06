import * as path from "path"

export const resolve = (paths) => {
  return path.resolve.apply(this, paths);
}
