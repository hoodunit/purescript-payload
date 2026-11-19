import net from "node:net";

export const rawGetImpl = port => target => (onError, onSuccess) => {
  let received = "";
  const socket = net.connect(port, "localhost", () => {
    socket.write(`GET ${target} HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n`);
  });
  socket.setEncoding("utf8");
  socket.on("data", chunk => { received += chunk; });
  socket.on("end", () => {
    const headerEnd = received.indexOf("\r\n\r\n");
    onSuccess({ status: received.split("\r\n")[0], body: received.slice(headerEnd + 4) });
  });
  socket.on("error", onError);
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    socket.destroy();
    onCancelerSuccess();
  };
};
