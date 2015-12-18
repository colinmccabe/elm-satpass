var passes = (function () {
    "use strict";

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
        var time = new Date(searchBegin.getTime());
        var lookAngle = undefined;

        // Step forward coarsely until el > 0
        do {
            lookAngle = getLookAngle(satrec, observerGd, time);
            time = new Date(time.getTime() + 60000);
        } while (lookAngle.elevation < 0.0);

        // Step back finely to pass start
        while (lookAngle.elevation > 0.0) {
            time = new Date(time.getTime() - 1000);
            lookAngle = getLookAngle(satrec, observerGd, time);
        }
        time = new Date(time.getTime() + 1000);
        lookAngle = getLookAngle(satrec, observerGd, time);
        
        var startTime = new Date(time.getTime());
        var startAz = lookAngle.azimuth;

        // Step forward to pass end, record maxEl
        var maxEl = -1.0;

        while (lookAngle.elevation > 0.0) {
            lookAngle = getLookAngle(satrec, observerGd, time);
            time = new Date(time.getTime() + 1000);
            maxEl = lookAngle.elevation > maxEl
                        ? lookAngle.elevation
                        : maxEl;
        }

        var endTime = new Date(time.getTime());
        var endAz = lookAngle.azimuth;

        return {
            maxEl: toDegInt(maxEl),
            startTime: startTime.getTime(),
            endTime: endTime.getTime(),
            startAz: toDegInt(startAz),
            endAz: toDegInt(endAz)
        };
    }

    function getPassesForSat(tle, start, duration) {
        var satrec = satellite.twoline2satrec(tle.line1, tle.line2);
        var deg2rad = 0.0174532925;

        var observerGd = {
            latitude: 41.9 * deg2rad,
            longitude: -71.42 * deg2rad,
            height: 0.0
        };

        var searchBegin = new Date(start.getTime());
        var searchEnd = new Date(searchBegin.getTime() + duration);
        var passes = [];

        while (searchBegin < searchEnd) {
            var pass = getNextPass(satrec, observerGd, searchBegin);
            searchBegin = new Date(pass.endTime);
            passes.push(pass);
        }

        return passes;
    }

    function get(predictReq) {
        var ret = [];
        var computationStart = (new Date()).getTime();

        var start = new Date(predictReq.start);

        predictReq.tle.map(function(sat) {
            var tleLines = sat[1];
            var passes = getPassesForSat(tleLines, start, predictReq.duration);
            
            passes = passes.map(function(pass) {
                pass.satName = sat[0];
                return pass;
            });
            
            ret = ret.concat(passes);
        });

        ret.sort(function(pass1, pass2) {
            return pass1.startTime - pass2.startTime;
        });

        console.log((new Date()).getTime() - computationStart);

        return ret;
    }

    return {
        get: get
    };
})();
