'use strict';

require('./index.html');
var Elm = require('./Main');
var satellite = require('satellite.js');


window.onload = function () {
    var app = Elm.fullscreen(Elm.Main, {
        startSignal: false,
        passes : []
    });

    setTimeout(function () {
        app.ports.startSignal.send(true);
    }, 0);

    app.ports.passReq.subscribe(function (passReq) {
        navigator.geolocation.getCurrentPosition(function (pos) {
            app.ports.passes.send(getPasses(passReq, pos.coords));
        }, undefined, geo_options);
    });
};

var geo_options = {
    enableHighAccuracy: false,
    maximumAge: 48 * 60 * 60 * 1000,  // 48 hours
    timeout: 10000
};

function getPasses (passReq, coords) {
    var computationStart = (new Date()).getTime();

    var allPasses = [];

    passReq.tles.map(function (tle) {
        var passes = getPassesForSat(coords, tle.line1, tle.line2,
                                     passReq.begin, passReq.duration);

        passes = passes.map(function (pass) {
            pass.satName = tle.satName;
            return pass;
        });

        allPasses = allPasses.concat(passes);
    });

    allPasses.sort(function (pass1, pass2) {
        return pass1.startTime - pass2.startTime;
    });

    console.log((new Date()).getTime() - computationStart);

    return allPasses;
}


function getPassesForSat (coords, tleLine1, tleLine2, begin, duration) {
    var satrec = satellite.twoline2satrec(tleLine1, tleLine2);
    var deg2rad = 0.0174532925;

    var observerGd = {
        latitude: coords.latitude * deg2rad,
        longitude: coords.longitude * deg2rad,
        height: 0.0
    };

    var searchBegin = new Date(begin);
    var searchEnd = new Date(searchBegin.getTime() + duration);
    var passes = [];

    while (searchBegin < searchEnd) {
        var pass = getNextPass(satrec, observerGd, searchBegin);
        searchBegin = new Date(pass.endTime);
        passes.push(pass);
    }

    return passes;
}


function getNextPass (satrec, observerGd, searchBegin) {
    var date = new Date(searchBegin.getTime());
    var lookAngle = undefined;

    // Step forward coarsely until el > 0
    do {
        lookAngle = getLookAngle(satrec, observerGd, date);
        date = new Date(date.getTime() + 60000);
    } while (lookAngle.elevation < 0.0);

    // Step back finely to pass start
    while (lookAngle.elevation > 0.0) {
        date = new Date(date.getTime() - 1000);
        lookAngle = getLookAngle(satrec, observerGd, date);
    }
    date = new Date(date.getTime() + 1000);
    lookAngle = getLookAngle(satrec, observerGd, date);

    var startTime = date.getTime();
    var startAz = lookAngle.azimuth;

    // Step forward to pass end, record apogee
    var maxEl = 0.0;
    var apogeeTime = date.getTime();

    while (lookAngle.elevation > 0.0) {
        lookAngle = getLookAngle(satrec, observerGd, date);
        if (lookAngle.elevation > maxEl) {
            maxEl = lookAngle.elevation;
            apogeeTime = date.getTime();
        }
        date = new Date(date.getTime() + 1000);
    }

    var endTime = date.getTime();
    var endAz = lookAngle.azimuth;

    return {
        maxEl: toDegInt(maxEl),
        startTime: startTime,
        apogeeTime: apogeeTime,
        endTime: endTime,
        startAz: toDegInt(startAz),
        endAz: toDegInt(endAz)
    };
}


function getLookAngle (satrec, observerGd, date) {
    var positionAndVelocity = satellite.propagate(
        satrec,
        date.getUTCFullYear(),
        date.getUTCMonth() + 1,
        date.getUTCDate(),
        date.getUTCHours(),
        date.getUTCMinutes(),
        date.getUTCSeconds()
    );

    var gmst = satellite.gstimeFromDate(
        date.getUTCFullYear(),
        date.getUTCMonth() + 1,
        date.getUTCDate(),
        date.getUTCHours(),
        date.getUTCMinutes(),
        date.getUTCSeconds()
    );

    var positionEci = positionAndVelocity.position;
    var positionEcf = satellite.eciToEcf(positionEci, gmst);

    return satellite.ecfToLookAngles(observerGd, positionEcf);
}


function toDegInt (rad) {
    return Math.round(rad * 57.2957795);
}
