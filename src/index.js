'use strict';

require('./index.html');
var Elm = require('./Main');
var satellite = require('satellite.js');


window.onload = function () {
    var app = Elm.fullscreen(Elm.Main, {
        init: {latitude: 0.0, longitude: 0.0},
        passesResp : [],
        lookAngleResp : []
    });

    navigator.geolocation.getCurrentPosition(function (pos) {
        app.ports.init.send(pos.coords);
    }, undefined, geo_options);

    app.ports.passesReq.subscribe(function (passReq) {
        app.ports.passesResp.send(getPasses(passReq));
    });

    app.ports.lookAngleReq.subscribe(function (lookAngleReq) {
        app.ports.lookAngleResp.send(getEl(lookAngleReq));
    });
};


var geo_options = {
    enableHighAccuracy: false,
    maximumAge: 48 * 60 * 60 * 1000,  // 48 hours
    timeout: 10000
};


function getEl (lookAngleReq) {
    var els = [];

    lookAngleReq.tles.map(function (tle) {
        var satrec = satellite.twoline2satrec(tle[1].line1, tle[1].line2);
        var gd = observerGd(lookAngleReq.coords);

        var lookAngle = getLookAngle(satrec, gd, new Date(lookAngleReq.time));

        els.push([tle[0], {
            elevation: toDeg(lookAngle.elevation),
            azimuth: toDeg(lookAngle.azimuth)
        }]);
    });

    return els;
}


function getPasses (passReq) {
    var computationStart = (new Date()).getTime();

    var allPasses = [];

    passReq.tles.map(function (tle) {
        var passes = getPassesForSat(passReq.coords, tle[1].line1, tle[1].line2,
                                     passReq.begin, passReq.duration);

        passes = passes.map(function (pass) {
            pass.satName = tle[0];
            pass.uid = pass.satName + '_' + String(pass.startTime);
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
    var gd = observerGd(coords);

    var searchBegin = new Date(begin);
    var searchEnd = new Date(searchBegin.getTime() + duration);
    var passes = [];

    while (searchBegin < searchEnd) {
        var pass = getNextPass(satrec, gd, searchBegin);
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
        maxEl: toDeg(maxEl),
        startTime: startTime,
        apogeeTime: apogeeTime,
        endTime: endTime,
        startAz: toDeg(startAz),
        endAz: toDeg(endAz)
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


function observerGd (coords) {
    return {
        latitude: toRad(coords.latitude),
        longitude: toRad(coords.longitude),
        height: 0.0
    };
}


function toDeg (rad) {
    return rad * 57.2957795;
}

function toRad (deg) {
    return deg * 0.0174532925;
}
