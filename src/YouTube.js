exports.initPlayer_ = function(id) {
  return function(onError, onSuccess) {
    // yuck, ew.
    // https://developers.google.com/youtube/iframe_api_reference

    var tag = document.createElement('script');

    tag.src = "https://www.youtube.com/iframe_api";
    var firstScriptTag = document.getElementsByTagName('script')[0];
    firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

    window.onYouTubeIframeAPIReady = function() {
      var player = new YT.Player(id, {
        events: {
          'onReady': function() { onSuccess(player); },
        }
      });
    };
  };
}

exports.attachPlayerStateHandler_ = function(player, handler) {
  player.addEventListener('onStateChange', handler);
};

exports.callPlayer_ = function(player, func, args) {
  if (player[func]) {
    player[func].apply(player, args);
  }
}
