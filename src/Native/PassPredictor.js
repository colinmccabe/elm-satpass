Elm.Native.PassPredictor = {};
Elm.Native.PassPredictor.make = function(localRuntime) {
    'use strict';

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.PassPredictor = localRuntime.Native.PassPredictor || {};
    if (localRuntime.Native.PassPredictor.values)
    {
        return localRuntime.Native.PassPredictor.values;
    }

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    function toDegInt(rad) {
        return Math.round(rad * 57.2957795);
    }

    function getLookAngle(satrec, observerGd, date) {
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

    function getNextPass(satrec, observerGd, searchBegin) {
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

        // Step forward to pass end, record maxEl
        var maxEl = -1.0;

        while (lookAngle.elevation > 0.0) {
            lookAngle = getLookAngle(satrec, observerGd, date);
            date = new Date(date.getTime() + 1000);
            maxEl = lookAngle.elevation > maxEl
                        ? lookAngle.elevation
                        : maxEl;
        }

        var endTime = date.getTime();
        var endAz = lookAngle.azimuth;

        return {
            maxEl: toDegInt(maxEl),
            startTime: startTime,
            endTime: endTime,
            startAz: toDegInt(startAz),
            endAz: toDegInt(endAz)
        };
    }

    function getPassesForSat(tle, from, duration) {
        var satrec = satellite.twoline2satrec(tle.line1, tle.line2);
        var deg2rad = 0.0174532925;

        var observerGd = {
            latitude: 41.9 * deg2rad,
            longitude: -71.42 * deg2rad,
            height: 0.0
        };

        var searchBegin = new Date(from);
        var searchEnd = new Date(searchBegin.getTime() + duration);
        var passes = [];

        while (searchBegin < searchEnd) {
            var pass = getNextPass(satrec, observerGd, searchBegin);
            searchBegin = new Date(pass.endTime);
            passes.push(pass);
        }

        return passes;
    }

    function getPasses(elm_tleMap, elm_from, elm_duration) {
        try {
            var computationStart = (new Date()).getTime();

            // Copy args from Elm
            var from = Number(elm_from);
            var duration = Number(elm_duration);
            var tleMap = [];

            while (elm_tleMap.ctor !== '[]') {
                var satName = String(elm_tleMap._0._0);
                var tleLines = Object.assign({}, elm_tleMap._0._1);

                tleMap.push([satName, tleLines]);

                elm_tleMap = elm_tleMap._1;
            }

            // get predictions from satellite.js
            var allPasses = [];

            tleMap.map(function (sat) {
                var tleLines = sat[1];
                var passes = getPassesForSat(tleLines, from, duration);

                passes = passes.map(function(pass) {
                    pass.satName = sat[0];
                    return pass;
                });

                allPasses = allPasses.concat(passes);
            });

            allPasses.sort(function(pass1, pass2) {
                return pass1.startTime - pass2.startTime;
            });

            // Copy results
            var elm_allPasses = [];

            allPasses.forEach(function (pass) {
                elm_allPasses.push(Object.assign({}, pass));
            });

            console.log((new Date()).getTime() - computationStart);

            return Task.succeed(Utils.list(elm_allPasses));
        } catch (err) {
            return Task.fail('' + err.message);
        }
    }

    return localRuntime.Native.PassPredictor.values = {
        getPasses: F3(getPasses)
    };
};
