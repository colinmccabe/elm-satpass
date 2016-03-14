'use strict';

require('./index.html');
var Elm = require('./Main');
var satellite = require('satellite.js');


window.onload = function () {
    var app = Elm.fullscreen(Elm.Main, {
        initSignalIn: false,
        passesIn : []
    });

    setTimeout(function () {
        app.ports.initSignalIn.send(true);
    }, 0);

    app.ports.passReqOut.subscribe(function (passReq) {
        var passes = getPasses(passReq);
        app.ports.passesIn.send(passes);
    });
};


function getPasses (predictReq) {
    var computationStart = (new Date()).getTime();

    var allPasses = [];

    predictReq.tles.map(function (tle) {
        var passes = getPassesForSat(tle.line1, tle.line2,
                        predictReq.begin, predictReq.duration);

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


function getPassesForSat (tleLine1, tleLine2, begin, duration) {
    var satrec = satellite.twoline2satrec(tleLine1, tleLine2);
    var deg2rad = 0.0174532925;

    var observerGd = {
        latitude: 41.9 * deg2rad,
        longitude: -71.42 * deg2rad,
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
