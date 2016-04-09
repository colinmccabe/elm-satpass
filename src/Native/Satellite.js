'use strict';

var make = function(localRuntime) {
  'use strict';

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Satellite = localRuntime.Native.Satellite || {};

  if (localRuntime.Native.Satellite.values) {
    return localRuntime.Native.Satellite.values;
  }

  var Task = Elm.Native.Task.make(localRuntime);


  function toDeg (rad) {
    return rad * 57.2957795;
  }


  function toRad (deg) {
    return deg * 0.0174532925;
  }


  function observerGd (coords) {
    return {
      latitude: toRad(coords.latitude),
      longitude: toRad(coords.longitude),
      height: 0.0
    };
  }


  function calcPositionAndVelocityEcf (satrec, date) {
    var gmst = satellite.gstimeFromDate(
        date.getUTCFullYear(),
        date.getUTCMonth() + 1,
        date.getUTCDate(),
        date.getUTCHours(),
        date.getUTCMinutes(),
        date.getUTCSeconds()
    );

    var pvEci = satellite.propagate(
        satrec,
        date.getUTCFullYear(),
        date.getUTCMonth() + 1,
        date.getUTCDate(),
        date.getUTCHours(),
        date.getUTCMinutes(),
        date.getUTCSeconds()
    );

    return {
      position: satellite.eciToEcf(pvEci.position, gmst),
      velocity: satellite.eciToEcf(pvEci.velocity, gmst)
    };
  }


  function calcDopplerFactor (observerPos, satPos, velocity) {
    var currentRange = Math.sqrt(
        Math.pow(satPos.x - observerPos.x, 2) +
        Math.pow(satPos.y - observerPos.y, 2) +
        Math.pow(satPos.z - observerPos.z, 2));

    var nextPos = {
      x : satPos.x + velocity.x,
      y : satPos.y + velocity.y,
      z : satPos.z + velocity.z
    };

    var nextRange =  Math.sqrt(
        Math.pow(nextPos.x - observerPos.x, 2) +
        Math.pow(nextPos.y - observerPos.y, 2) +
        Math.pow(nextPos.z - observerPos.z, 2));

    var rangeRate =  currentRange - nextRange;

    var c = 299792.458; // Speed of light in km/s
    var factor = (1 + rangeRate/c);
    return factor;
  }


  function calcLookAngle (satrec, observerGd, date) {
    var pvEcf = calcPositionAndVelocityEcf(satrec, date);
    return satellite.ecfToLookAngles(observerGd, pvEcf.position, pvEcf.velocity);
  }


  function nextPass (satrec, observerGd, searchBegin) {
    var date = new Date(searchBegin.getTime());
    var lookAngle = undefined;

    // Step forward coarsely until el > 0
    do {
      lookAngle = calcLookAngle(satrec, observerGd, date);
      date = new Date(date.getTime() + 60000);
    } while (lookAngle.elevation < 0.0);

    // Step back finely to pass start
    while (lookAngle.elevation > 0.0) {
      date = new Date(date.getTime() - 1000);
      lookAngle = calcLookAngle(satrec, observerGd, date);
    }
    date = new Date(date.getTime() + 1000);
    lookAngle = calcLookAngle(satrec, observerGd, date);

    var startTime = date.getTime();
    var startAz = lookAngle.azimuth;

    // Step forward to pass end, record apogee
    var maxEl = 0.0;
    var apogeeTime = date.getTime();

    while (lookAngle.elevation > 0.0) {
      lookAngle = calcLookAngle(satrec, observerGd, date);
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


  function passesForSat (coords, tleLine1, tleLine2, begin, duration) {
    var satrec = satellite.twoline2satrec(tleLine1, tleLine2);
    var gd = observerGd(coords);

    var searchBegin = new Date(begin);
    var searchEnd = new Date(searchBegin.getTime() + duration);
    var passes = [];

    while (searchBegin < searchEnd) {
      var pass = nextPass(satrec, gd, searchBegin);
      searchBegin = new Date(pass.endTime);
      passes.push(pass);
    }

    return passes;
  }


  function getLookAngle (coords, tle, time) {
    var gd = observerGd(coords);
    var observerEcf = satellite.geodeticToEcf(gd);
    var date = new Date(time);

    var satrec = satellite.twoline2satrec(tle.line1, tle.line2);
    var lookAngle = calcLookAngle(satrec, gd, date);
    var pvEcf = calcPositionAndVelocityEcf(satrec, date);
    var dopplerFactor = calcDopplerFactor(observerEcf, pvEcf.position, pvEcf.velocity);

    console.log(dopplerFactor);

    return Task.succeed({
      elevation: toDeg(lookAngle.elevation),
      azimuth: toDeg(lookAngle.azimuth),
      dopplerFactor: dopplerFactor
    });
  }


  function getPasses (coords, sats, begin, duration) {
    var computationStart = (new Date()).getTime();

    var allPasses = [];

    sats.map(function (sat) {
      var passes = passesForSat(coords, sat.tle.line1, sat.tle.line2,
                                begin, duration);

      passes = passes.map(function (pass) {
        pass.satName = sat.satName;
        pass.passId = pass.satName + '_' + String(pass.startTime);
        return pass;
      });

      passes = passes.filter(function (pass) {
        return pass.maxEl > 5.0;
      });

      allPasses = allPasses.concat(passes);
    });

    allPasses.sort(function (pass1, pass2) {
      return pass1.startTime - pass2.startTime;
    });

    console.log((new Date()).getTime() - computationStart);

    return Task.succeed(allPasses);
  }

  return localRuntime.Native.Satellite.values = {
    getPasses: F4(getPasses),
    getLookAngle: F3(getLookAngle)
  };
};


Elm.Native.Satellite = {};
Elm.Native.Satellite.make = make;
