import net from "node:net";

export const rawStatusLineImpl = port => target => (onError, onSuccess) => {
  let received = "";
  const socket = net.connect(port, "localhost", () => {
    socket.write(`GET ${target} HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n`);
  });
  socket.setEncoding("utf8");
  socket.on("data", chunk => { received += chunk; });
  socket.on("end", () => onSuccess(received.split("\r\n")[0]));
  socket.on("error", onError);
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    socket.destroy();
    onCancelerSuccess();
  };
};
