const init = () => {
  console.log('Hello');
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
