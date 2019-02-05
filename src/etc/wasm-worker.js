Module = {
    noInitialRun: true,
    print: function (text) { self.postMessage({ 'type': 'stdout', 'data': text }); },
    printErr: function (text) { self.postMessage({ 'type': 'stderr', 'data': text }); },
    onRuntimeInitialized: function () {
        self.postMessage({ 'type': 'loaded' });
    }
};

importScripts('cruise.js');

onmessage = function (e) {
    var data = e.data;
    if (data.cmd != 'exec') throw new Error('Bad worker argument type');

    Module.callMain(data.data);
    self.postMessage({ 'type': 'close' });
    self.close();
};
