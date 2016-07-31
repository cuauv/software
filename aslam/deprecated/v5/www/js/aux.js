var fullScreen = function(e) {
  var poly = e.requestFullScreen || e.webkitRequestFullScreen || e.mozRequestFullScreen || e.msRequestFullscreen;
  poly.call(e);
};
