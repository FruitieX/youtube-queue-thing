// THIS IS AWFUL
// https://developers.google.com/youtube/iframe_api_reference

exports.initPlayer_ = function(id) {
  return function(onStateChange) {
    return function(onError, onSuccess) {
      var tag = document.createElement('script');

      tag.src = "https://www.youtube.com/iframe_api";
      var firstScriptTag = document.getElementsByTagName('script')[0];
      firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

      window.onYouTubeIframeAPIReady = function() {
        var player = new YT.Player(id, {
          // height: '390',
          // width: '640',
          // videoId: 'M7lc1UVf-VE',
          events: {
            'onReady': function() { onSuccess(player); },
            'onStateChange': onStateChange
          }
        });
      };
    };
  };
}

exports.callPlayer_ = function(player, func, args) {
  if (player[func]) {
    console.log('calling', func, 'with args', args);
    player[func].apply(player, args);
  }
}
