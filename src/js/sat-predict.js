window.SatPredict = (function() {
  'use strict';

  function toDeg (rad) {
    return rad * 57.2957795;
  }


  function toRad (deg) {
    return deg * 0.0174532925;
  }


  function observerGd (latitude, longitude, altitude) {
    return {
      latitude: toRad(latitude),
      longitude: toRad(longitude),
      height: altitude
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

    if (satrec.error !== 0)
      throw Error('error #' + satrec.error + ' calculating position and velocity');

    return {
      position: satellite.eciToEcf(pvEci.position, gmst),
      velocity: satellite.eciToEcf(pvEci.velocity, gmst)
    };
  }


  function calcDopplerFactor (observerPos, satEcf) {
    var satPos = satEcf.position;
    var satVelocity = satEcf.velocity;

    var currentRange = Math.sqrt(
        Math.pow(satPos.x - observerPos.x, 2) +
        Math.pow(satPos.y - observerPos.y, 2) +
        Math.pow(satPos.z - observerPos.z, 2));

    var nextSatPos = {
      x : satPos.x + satVelocity.x,
      y : satPos.y + satVelocity.y,
      z : satPos.z + satVelocity.z
    };

    var nextRange =  Math.sqrt(
        Math.pow(nextSatPos.x - observerPos.x, 2) +
        Math.pow(nextSatPos.y - observerPos.y, 2) +
        Math.pow(nextSatPos.z - observerPos.z, 2));

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
    var lookAngle = null;

    // Step forward coarsely until el > 0
    do {
      lookAngle = calcLookAngle(satrec, observerGd, date);
      date = new Date(date.getTime() + 60000);
    } while ((lookAngle.elevation < 0.0) && (date - searchBegin < 86400000));

    if (lookAngle === null) {
      return null;
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


  function passesForSat (tleLine1, tleLine2, req) {
    var satrec = satellite.twoline2satrec(tleLine1, tleLine2);
    var gd = observerGd(req.latitude, req.longitude, req.altitude);

    var searchBegin = new Date(req.begin);
    var searchEnd = new Date(req.end);
    var passes = [];

    while (searchBegin < searchEnd) {
      var pass = nextPass(satrec, gd, searchBegin);

      if (pass === null || pass.startTime > searchEnd.getTime()) {
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
      var gd = observerGd(req.latitude, req.longitude, req.altitude);
      var satrec = satellite.twoline2satrec(sat.tle.line1, sat.tle.line2);

      var observerEcf = satellite.geodeticToEcf(gd);
      var lookAngle = calcLookAngle(satrec, gd, date);
      var satEcf = calcPositionAndVelocityEcf(satrec, date);
      var dopplerFactor = calcDopplerFactor(observerEcf, satEcf);

      lookAngles.push({
        id: sat.id,
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
      try {
        var satPasses = passesForSat(sat.tle.line1, sat.tle.line2, req);
      } catch (e) {
        console.warn('Could not get passes for ' + sat.satName + ': ' + e.message);
        return;
      }

      satPasses.forEach(function (pass) {
        var approxStartTime = Math.floor(pass.startTime / 10000);
        pass.passId = sat.satName + '_' + String(approxStartTime);
        pass.satName = sat.satName,
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
