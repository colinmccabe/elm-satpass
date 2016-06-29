window.SatPredict = (function() {
  'use strict';

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
      height: coords.altitude
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
    } while ((lookAngle.elevation < 0.0) && (date - searchBegin < 86400000));

    if (lookAngle === undefined) {
      return undefined;
    }

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


  function passesForSat (coords, tleLine1, tleLine2, begin, end) {
    var satrec = satellite.twoline2satrec(tleLine1, tleLine2);
    var gd = observerGd(coords);

    var searchBegin = new Date(begin);
    var searchEnd = new Date(end);
    var passes = [];

    while (searchBegin < searchEnd) {
      var pass = nextPass(satrec, gd, searchBegin);

      if (pass === undefined || pass.startTime > searchEnd.getTime()) {
        break;
      }

      passes.push(pass);
      searchBegin = new Date(pass.endTime);
    }

    return passes;
  }


  function getLookAngles (req)
  {
    var lookAngles = [];

    req.sats.forEach(function (sat) {
      var date = new Date(req.time);
      var gd = observerGd(req.coords);
      var satrec = satellite.twoline2satrec(sat.tle.line1, sat.tle.line2);

      var observerEcf = satellite.geodeticToEcf(gd);
      var lookAngle = calcLookAngle(satrec, gd, date);
      var pvEcf = calcPositionAndVelocityEcf(satrec, date);
      var dopplerFactor = calcDopplerFactor(observerEcf, pvEcf.position, pvEcf.velocity);

      lookAngles.push({
        passId: sat.passId,
        elevation: toDeg(lookAngle.elevation),
        azimuth: toDeg(lookAngle.azimuth),
        dopplerFactor: dopplerFactor
      });
    });

    return lookAngles;
  }


  function getPasses (req)
  {
    var passes = [];

    req.sats.forEach(function (sat) {
      var satPasses = passesForSat(req.coords, sat.tle.line1, sat.tle.line2,
                                   req.begin, req.end);

      satPasses.forEach(function (pass) {
        pass.satName = sat.satName,
        pass.passId = sat.satName + '_' + String(pass.startTime);
        passes.push(pass);
      });
    });

    passes = passes.filter(function (pass) {
      return pass.maxEl > 5.0;
    });

    return [req.end, passes];
  }

  return {
    getPasses: getPasses,
    getLookAngles: getLookAngles
  };
})();
