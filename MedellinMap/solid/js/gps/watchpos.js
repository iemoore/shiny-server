navigator.geolocation.watchPosition(function(pos) {
  Shiny.onInputChange("userPosition", {
    latitude: pos.coords.latitude,
    longitude: pos.coords.longitude,
    altitude: pos.coords.altitude,
    accuracy: pos.coords.accuracy // in meters
    altitudeAccuracy: pos.coords.altitudeAccuracy // in meters,
    heading: pos.coords.heading,
    speed: pos.coords.speed
  });
}, null, {enableHighAccuracy: true});