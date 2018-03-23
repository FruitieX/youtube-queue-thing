exports.callPlayer_ = function(func, args) {
  console.log('calling', func, 'with args', args);
    var i = 0,
        iframes = document.getElementsByTagName('iframe'),
        src = '';
    if (!iframes.length) console.log('YouTube iframe not found!');
    for (i = 0; i < iframes.length; i += 1) {
        src = iframes[i].getAttribute('src');
        if (src && src.indexOf('youtube.com/embed') !== -1) {
            iframes[i].contentWindow.postMessage(JSON.stringify({
                'event': 'command',
                'func': func,
                'args': args || []
            }), '*');
        }
    }
}
