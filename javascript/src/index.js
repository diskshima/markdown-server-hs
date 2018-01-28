const readPath = () => window.location.pathname;
const reloadPage = () => window.location.reload(true);
const hostUrl = window.location.host;

const init = () => {
  console.log(hostUrl);
  const socket = new WebSocket(`ws://${hostUrl}/ws`);
  const path = readPath();

  socket.onopen = (ev) => {
    socket.send(path.replace(/^\//, ''));
  };

  socket.onmessage = (ev) => {
    console.log(`"${ev.data}" received. Reloading`);
    reloadPage();
  };
};

const runAttachEvent = (func) => {
  if (window.attachEvent) {
    window.attachEvent('onload', func);
  } else {
    if(window.onload) {
      const curronload = window.onload;
      const newonload = function(evt) {
        curronload(evt);
        func(evt);
      };
      window.onload = newonload;
    } else {
      window.onload = func;
    }
  }
};

runAttachEvent(init);
