'use strict';

require('./index.html');
var Elm = require('./Main');

var geo_options = {
  enableHighAccuracy: false,
  maximumAge: 48 * 60 * 60 * 1000,  // 48 hours
  timeout: 10000
};


window.onload = function () {
  var app = Elm.fullscreen(Elm.Main, {
    init: {latitude: 0.0, longitude: 0.0}
  });

  navigator.geolocation.getCurrentPosition(function (pos) {
    app.ports.init.send(pos.coords);
  }, undefined, geo_options);
};
