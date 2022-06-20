import path from 'path';

export function resolve(paths) {
  return path.resolve.apply(this, paths);
}
